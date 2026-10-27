# Census regions by state FIPS code (the four modern Census Bureau regions;
# DC in the South). Used to group counties for regional fertility schedules.
CENSUS_REGION <- c(
  `9` = "Northeast", `23` = "Northeast", `25` = "Northeast", `33` = "Northeast", `44` = "Northeast",
  `50` = "Northeast", `34` = "Northeast", `36` = "Northeast", `42` = "Northeast",
  `17` = "Midwest", `18` = "Midwest", `26` = "Midwest", `39` = "Midwest", `55` = "Midwest",
  `19` = "Midwest", `20` = "Midwest", `27` = "Midwest", `29` = "Midwest", `31` = "Midwest",
  `38` = "Midwest", `46` = "Midwest",
  `10` = "South", `11` = "South", `12` = "South", `13` = "South", `24` = "South", `37` = "South",
  `45` = "South", `51` = "South", `54` = "South", `1` = "South", `21` = "South", `28` = "South",
  `47` = "South", `5` = "South", `22` = "South", `40` = "South", `48` = "South",
  `4` = "West", `8` = "West", `16` = "West", `30` = "West", `32` = "West", `35` = "West",
  `49` = "West", `56` = "West", `2` = "West", `6` = "West", `15` = "West", `41` = "West", `53` = "West")

census_region <- function(statefip) unname(CENSUS_REGION[as.character(as.integer(statefip))])
