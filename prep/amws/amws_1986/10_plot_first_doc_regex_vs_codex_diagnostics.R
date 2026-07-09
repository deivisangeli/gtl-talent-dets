# Compare first AMWS 1986 doc raw-text counts from regex and Codex outputs.
# Also place the comparison next to the regex entries-per-page diagnostic.

suppressPackageStartupMessages({
  library(ggplot2)
  library(grid)
})

find_data_root <- function() {
  env_root <- Sys.getenv("TALENT_DETS_DATA_DIR", unset = "")
  if (nzchar(env_root) && dir.exists(env_root)) {
    return(normalizePath(env_root, winslash = "/", mustWork = TRUE))
  }

  user_root <- file.path(
    "C:/Users",
    Sys.info()[["user"]],
    "Globtalent Dropbox",
    "gtl_talent_dets"
  )
  if (dir.exists(user_root)) {
    return(normalizePath(user_root, winslash = "/", mustWork = TRUE))
  }

  stop(
    "Could not find Dropbox data root. Set TALENT_DETS_DATA_DIR to ",
    "the local gtl_talent_dets Dropbox folder."
  )
}

read_csv_base <- function(path) {
  if (!file.exists(path)) {
    stop("Missing input file: ", path)
  }
  read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    fileEncoding = "UTF-8-BOM"
  )
}

clean_text <- function(x) {
  x <- trimws(as.character(x))
  gsub("\\s+", " ", x)
}

count_unique_raw_text <- function(path) {
  dat <- read_csv_base(path)
  if (!"raw_text" %in% names(dat)) {
    stop("Missing raw_text column in: ", path)
  }
  raw_text <- clean_text(dat$raw_text)
  raw_text <- raw_text[nzchar(raw_text)]
  data.frame(
    n_rows = nrow(dat),
    unique_raw_text = length(unique(raw_text))
  )
}

data_root <- find_data_root()
amws_output <- file.path(data_root, "output", "amws")
regex_all_dir <- file.path(amws_output, "regex_all_docs")

regex_first_doc_file <- file.path(
  regex_all_dir,
  "amws_ed16_entries_regex_raw.csv"
)
codex_first_doc_file <- file.path(
  amws_output,
  "consolidated_docs",
  "amws16_A_000_200",
  "amws_entries_parsed.csv"
)
entries_per_page_file <- file.path(
  regex_all_dir,
  "amws_entries_per_pdf_page_by_doc.csv"
)

regex_raw <- read_csv_base(regex_first_doc_file)
regex_first_doc <- regex_raw[regex_raw$doc_id == "amws16_A_0_200", , drop = FALSE]
if (!nrow(regex_first_doc)) {
  stop("No rows found for doc_id == amws16_A_0_200 in regex raw file.")
}

tmp_regex_file <- tempfile(fileext = ".csv")
write.csv(regex_first_doc, tmp_regex_file, row.names = FALSE, fileEncoding = "UTF-8")

regex_counts <- count_unique_raw_text(tmp_regex_file)
codex_counts <- count_unique_raw_text(codex_first_doc_file)

counts <- data.frame(
  method = factor(
    c("Regex final", "Codex puro"),
    levels = c("Regex final", "Codex puro")
  ),
  unique_raw_text = c(
    regex_counts$unique_raw_text,
    codex_counts$unique_raw_text
  ),
  n_rows = c(regex_counts$n_rows, codex_counts$n_rows)
)

entries_per_page <- read_csv_base(entries_per_page_file)
required_cols <- c("doc_id", "n_entries", "entries_per_pdf_page")
missing_cols <- setdiff(required_cols, names(entries_per_page))
if (length(missing_cols)) {
  stop(
    "Missing required columns in entries-per-page file: ",
    paste(missing_cols, collapse = ", ")
  )
}

entries_per_page$n_entries <- as.numeric(entries_per_page$n_entries)
entries_per_page$entries_per_pdf_page <- as.numeric(entries_per_page$entries_per_pdf_page)

first_doc_page <- entries_per_page[
  entries_per_page$doc_id == "amws16_A_0_200",
  ,
  drop = FALSE
]
if (nrow(first_doc_page) != 1L) {
  stop("Expected exactly one entries-per-page row for amws16_A_0_200.")
}

summary_out <- data.frame(
  metric = c(
    "regex_final_unique_raw_text",
    "codex_puro_unique_raw_text",
    "histogram_first_doc_regex_entries",
    "histogram_first_doc_regex_entries_per_pdf_page"
  ),
  value = c(
    regex_counts$unique_raw_text,
    codex_counts$unique_raw_text,
    first_doc_page$n_entries,
    first_doc_page$entries_per_pdf_page
  )
)

summary_file <- file.path(
  regex_all_dir,
  "first_doc_regex_vs_codex_and_entries_per_page_summary.csv"
)
write.csv(summary_out, summary_file, row.names = FALSE, fileEncoding = "UTF-8")

bar_plot <- ggplot(counts, aes(x = method, y = unique_raw_text, fill = method)) +
  geom_col(width = 0.62, show.legend = FALSE) +
  geom_text(
    aes(label = format(unique_raw_text, big.mark = ",")),
    vjust = -0.45,
    size = 4.2
  ) +
  scale_fill_manual(values = c("Regex final" = "#3C6E71", "Codex puro" = "#D98C3A")) +
  scale_y_continuous(
    labels = function(x) format(x, big.mark = ",", scientific = FALSE),
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    title = "First document: unique raw texts",
    subtitle = "Final post-merge regex vs consolidated Codex-only output",
    x = NULL,
    y = "Unique raw texts"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.x = element_blank(),
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(size = 11)
  )

hist_plot <- ggplot(entries_per_page, aes(x = entries_per_pdf_page)) +
  geom_histogram(binwidth = 0.5, boundary = 0, fill = "#6B7280", color = "white") +
  geom_vline(
    xintercept = first_doc_page$entries_per_pdf_page,
    color = "#D98C3A",
    linewidth = 0.9
  ) +
  labs(
    title = "Entries per PDF page across documents",
    subtitle = "regex_all_docs diagnostic adjusted for actual PDF page counts",
    x = "Entries per PDF page",
    y = "Number of documents"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold")
  )

combined_title <- textGrob(
  "AMWS 1986 ed. 16: first document and PDF-page-adjusted regex diagnostic",
  gp = gpar(fontsize = 15, fontface = "bold")
)
combined_note <- textGrob(
  "Note: the left panel uses final post-merge raw_text counts (regex = 3,416; Codex = 2,692). The histogram uses the regex diagnostic adjusted for actual PDF pages (16_A_0_200 = 3,652 entries, 17.23/PDF page).",
  x = 0,
  hjust = 0,
  gp = gpar(fontsize = 9, col = "#4B5563")
)

plot_png <- file.path(
  regex_all_dir,
  "first_doc_regex_vs_codex_and_entries_per_page.png"
)
plot_pdf <- file.path(
  regex_all_dir,
  "first_doc_regex_vs_codex_and_entries_per_page.pdf"
)

draw_combined <- function() {
  grid.newpage()
  pushViewport(
    viewport(
      layout = grid.layout(
        nrow = 3,
        ncol = 2,
        heights = unit(c(0.09, 0.84, 0.07), "npc"),
        widths = unit(c(0.42, 0.58), "npc")
      )
    )
  )
  pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 1:2))
  grid.draw(combined_title)
  popViewport()
  print(bar_plot, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
  print(hist_plot, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
  pushViewport(viewport(layout.pos.row = 3, layout.pos.col = 1:2))
  grid.draw(combined_note)
  popViewport(2)
}

png(plot_png, width = 13, height = 6.5, units = "in", res = 220)
draw_combined()
dev.off()

pdf(plot_pdf, width = 13, height = 6.5, onefile = TRUE)
draw_combined()
dev.off()

cat("Wrote summary: ", summary_file, "\n", sep = "")
cat("Wrote PNG: ", plot_png, "\n", sep = "")
cat("Wrote PDF: ", plot_pdf, "\n", sep = "")
