###############################################################################
# Scrape Wikipedia list of world's fairs to Dropbox input.
#
# Output:
#   file.path(DATA_INPUT, "worlds_fairs_wikipedia.xlsx")
#
# Run from prep/world_fairs_panel/:
#   Rscript 01_scrape_worlds_fairs.R
###############################################################################

suppressPackageStartupMessages({
  library(dplyr)
  library(rvest)
  library(stringr)
  library(tibble)
  library(writexl)
  library(xml2)
})

repo_root <- normalizePath(Sys.getenv("GTL_REPO", unset = NA), winslash = "/", mustWork = TRUE)
source(file.path(repo_root, "paths.R"))

url <- "https://en.wikipedia.org/wiki/List_of_world%27s_fairs"
out_file <- file.path(DATA_INPUT, "worlds_fairs_wikipedia.xlsx")

manual_additions <- tribble(
  ~Year, ~City, ~Country, ~Fair_name,
  "1790", "Hamburg", "Germany", "Hamburg Crafts Exhibition"
)

clean_text <- function(x) {
  x %>%
    str_replace_all("\\[[^\\]]*\\]", "") %>%
    str_replace_all("\\s+", " ") %>%
    str_squish()
}

parse_entry <- function(text) {
  text <- clean_text(text)

  year_match <- str_match(
    text,
    "^([0-9]{4}(?:\\s*[–-]\\s*[0-9]{4})?)\\s*[–-]\\s*(.+)$"
  )

  if (is.na(year_match[1, 1])) {
    return(tibble(
      Year = NA_character_,
      City = NA_character_,
      Country = NA_character_,
      Fair_name = text
    ))
  }

  year <- year_match[1, 2] %>%
    str_replace_all("\\s*[–-]\\s*", "-") %>%
    str_squish()
  remainder <- year_match[1, 3] %>%
    str_replace_all("([[:alnum:]\\)])–\\s*", "\\1 – ") %>%
    str_squish()

  cancelled_match <- str_match(
    remainder,
    "^cancelled \\(planned site: (.+)\\)$"
  )

  if (!is.na(cancelled_match[1, 1])) {
    location <- cancelled_match[1, 2] %>% str_squish()
    fair_name <- "cancelled"
  } else {
    sep <- str_locate(remainder, "\\s+[–-]\\s+")

    if (is.na(sep[1, 1])) {
      location <- remainder
      fair_name <- NA_character_

      # A few Wikipedia entries omit the separator, e.g.
      # "Stockholm, SwedenILIS 1936" or "Helsingborg, SwedenHelsingborg...".
      glued_country_fair <- str_match(location, "^(.+,\\s*.+[[:lower:]\\)])([[:upper:]].*)$")
      country_with_note <- str_match(location, "^(.+,\\s*[[:alpha:][:space:].'-]+)\\s+(which\\s+.*)$")

      if (!is.na(glued_country_fair[1, 1])) {
        location <- glued_country_fair[1, 2] %>% str_squish()
        fair_name <- glued_country_fair[1, 3] %>% str_squish()
      } else if (!is.na(country_with_note[1, 1])) {
        location <- country_with_note[1, 2] %>% str_squish()
        fair_name <- country_with_note[1, 3] %>% str_squish()
      } else if (!str_detect(location, ",") && str_detect(location, "\\s+")) {
        first_word <- str_match(location, "^([^\\s]+)\\s+(.+)$")
        location <- first_word[1, 2] %>% str_squish()
        fair_name <- first_word[1, 3] %>% str_squish()
      }
    } else {
      location <- str_sub(remainder, 1, sep[1, 1] - 1) %>% str_squish()
      fair_name <- str_sub(remainder, sep[1, 2] + 1) %>% str_squish()
    }
  }

  location_parts <- str_split(location, "\\s*,\\s*", simplify = FALSE)[[1]]
  location_parts <- location_parts[location_parts != ""]

  city <- if (length(location_parts) >= 1) location_parts[1] else NA_character_
  country <- if (length(location_parts) >= 2) {
    location_parts[length(location_parts)]
  } else {
    NA_character_
  }

  tibble(
    Year = year,
    City = city,
    Country = country,
    Fair_name = fair_name
  )
}

clean_fair_names <- function(fairs) {
  fairs %>%
    mutate(
      Fair_observation = NA_character_,
      is_prague_1791 = Year == "1791" & City == "Prague" & Country == "Bohemia",
      is_paris_1798 = Year == "1798" & City == "Paris" & Country == "France",
      is_paris_1801 = Year == "1801" & City == "Paris" & Country == "France",
      is_turin_1829 = Year == "1829" & City == "Turin" & Country == "Piedmont-Sardinia",
      is_london_1851 = Year == "1851" & City == "London" & Country == "United Kingdom",
      is_melbourne_1884 = Year == "1884" & City == "Melbourne" & Country == "Victoria",
      is_st_louis_1904 = Year == "1904" & City == "St. Louis" & Country == "United States",
      Fair_observation = case_when(
        is_prague_1791 | is_paris_1798 | is_paris_1801 | is_turin_1829 ~ Fair_name,
        is_london_1851 ~ "The Crystal Palace (typically listed as the \"first world's fair\")",
        is_melbourne_1884 ~ "of Wine, Fruit, Grain & other products of the soil of Australasia with machinery, plant and tools employed",
        is_st_louis_1904 ~ "also called Louisiana Purchase International Exposition and Olympic Games: 1904 Summer Olympics",
        TRUE ~ Fair_observation
      ),
      Fair_name = case_when(
        is_prague_1791 ~ "Prague 1791",
        is_paris_1798 ~ "Paris 1798",
        is_paris_1801 ~ "Paris 1801",
        is_turin_1829 ~ "Turin 1829",
        is_london_1851 ~ "The Great Exhibition of the Works of Industry of All Nations",
        is_melbourne_1884 ~ "Victorian International Exhibition 1884",
        is_st_louis_1904 ~ "Louisiana Purchase Exposition",
        TRUE ~ Fair_name
      )
    ) %>%
    select(
      Year, City, Country, Fair_name, Fair_observation,
      -starts_with("is_")
    )
}

doc <- read_html(url)
content <- html_element(doc, "#mw-content-text .mw-parser-output")
children <- xml_children(content)

current_decade <- NA_character_
entries <- list()

for (node in children) {
  node_name <- xml_name(node)
  node_class <- xml_attr(node, "class")

  if (node_name == "div" && !is.na(node_class) && str_detect(node_class, "mw-heading")) {
    heading <- html_element(node, "h2, h3") %>% html_text2()
    current_decade <- if (str_detect(heading, "^[0-9]{4}s$")) heading else NA_character_
    next
  }

  if (is.na(current_decade) || node_name != "ul") {
    next
  }

  li_text <- html_elements(node, xpath = "./li") %>%
    html_text2()

  if (length(li_text) == 0) {
    next
  }

  entries[[length(entries) + 1]] <- tibble(raw_entry = li_text)
}

worlds_fairs <- bind_rows(entries) %>%
  mutate(raw_entry = clean_text(raw_entry)) %>%
  filter(str_detect(raw_entry, "^[0-9]{4}(?:\\s*[–-]\\s*[0-9]{4})?\\s*[–-]\\s*")) %>%
  pull(raw_entry) %>%
  lapply(parse_entry) %>%
  bind_rows() %>%
  filter(!is.na(Year), !is.na(City)) %>%
  bind_rows(manual_additions) %>%
  distinct() %>%
  clean_fair_names()

dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
tmp_file <- file.path(
  dirname(out_file),
  paste0(".", tools::file_path_sans_ext(basename(out_file)), "_tmp.xlsx")
)

if (file.exists(tmp_file)) {
  unlink(tmp_file)
}

writexl::write_xlsx(worlds_fairs, tmp_file)

if (file.exists(out_file)) {
  unlink(out_file)
}

if (!file.rename(tmp_file, out_file)) {
  stop("Could not replace output file: ", out_file)
}

cat("Wrote:", out_file, "\n")
cat("Rows:", nrow(worlds_fairs), "\n")
cat("Columns:", paste(names(worlds_fairs), collapse = ", "), "\n\n")

cat("First 10 rows:\n")
print(head(worlds_fairs, 10))

cat("\nLast 10 rows:\n")
print(tail(worlds_fairs, 10))

required_cols <- c("Year", "City", "Country", "Fair_name", "Fair_observation")
if (!identical(names(worlds_fairs), required_cols)) {
  stop("Unexpected output columns: ", paste(names(worlds_fairs), collapse = ", "))
}

if (!file.exists(out_file)) {
  stop("Output file was not created: ", out_file)
}
