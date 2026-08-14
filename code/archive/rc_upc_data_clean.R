## start up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## clear working history
rm(list=ls())

## add libraries
library(tidyverse)
library(tibble)
library(dplyr)
library(tidyr)

## set working directory to home folder
setwd("../")
getwd()

## relative file paths
data <- "data/Reef_Check"
code <- "code"
figs <- "figs"
results <- "results"

## shared reference data and helper functions
source(file.path(code, "rc_data_clean_reference.R"))

df_upc <- read.csv(file.path(data, "UPC_Washington_raw.csv"))

names(df_upc) <- tolower(names(df_upc))

## drop Canadian sites
df_upc <- subset(df_upc, !(site %in% canadian_sites))

df_upc <- df_upc %>%
  left_join(basin_lookup, by = "site")

## key
df_upc <- create.key(df_upc)

## add depth_zone column
df_upc <- add.depth.zone(df_upc)

## reorder columns
df_upc <- df_upc %>%
  select(
    key, basin, basin_id, site, site_id, transect, depth_zone,
    latitude, longitude, year, date, depth_ft,
    category, classcode, amount, total_amount, percentage
  )

## scale adjustment
df_upc <- df_upc %>%
  mutate(
    amount = if_else(total_amount != 30, as.integer(amount / total_amount * 30), amount),
    total_amount = if_else(total_amount != 30, 30, total_amount),
    percentage = round(percentage / 100, digits = 2)
  )

## clean strings
df_upc <- df_upc %>%
  mutate(across(where(is.character), ~ gsub(" ", "_", .x)))

names(df_upc) <- gsub(" ", "_", names(df_upc))

df_upc$classcode <- tolower(df_upc$classcode)

## pivot
df_upc <- df_upc %>%
  mutate(classcode = recode(classcode,
                            "superlayer_brown_algae" = "sp_brown_algae",
                            "superlayer_red_algae" = "sp_red_algae",
                            "superlayer_green_algae" = "sp_green_algae",
                            "superlayer_mobile_invertebrates" = "sp_mobile_inverts",
                            "superlayer_acid_weed" = "sp_acid_weed",

                            "clay" = "sb_clay-wa",
                            "sand" = "sb_sand",
                            "pebble_(0.5-5cm-wa)" = "sb_pebble-wa",
                            "cobble" = "sb_cobble",
                            "cobble_(5-15cm-wa)" = "sb_cobble-wa",
                            "rock_(15-25cm-wa)" = "sb_rock",
                            "boulder" = "sb_boulder",
                            "small_boulder_(25-50cm-wa)" = "sb_s_boulder-wa",
                            "large_boulder_(50cm-1m-wa)" = "sb_l_boulder-wa",
                            "reef" = "sb_reef",
                            "shell_hash" = "sb_shell_hash-wa",
                            "other" = "sb_other",

                            "none" = "cv_none",
                            "kelp_holdfast" = "cv_holdfast",
                            "other_brown_algae" = "cv_brown_algae",
                            "acid_weed" = "cv_acid_weed",
                            "green_algae" = "cv_green_algae",
                            "red_algae" = "cv_red_algae",
                            "encrusting_red_algae" = "cv_encrusting",
                            "articulated_coralline" = "cv_articulated",
                            "crustose_coralline" = "cv_crustose",
                            "sessile_invertebrates" = "cv_sessile_inverts",
                            "seagrasses" = "cv_seagrass",

                            "0-10cm" = "rf_0-10cm",
                            "10cm-1m" = "rf_10cm-1m",
                            "1m-2m" = "rf_1m-2m",
                            ">2m" = "rf_over_2m"
  ))

df_upc_wide <- df_upc %>%
  select(key:depth_ft, classcode, percentage) %>%
  pivot_wider(
    names_from = classcode,
    values_from = percentage,
    values_fill = 0
  )

ordered_class_columns <- c(
  "sp_brown_algae",
  "sp_red_algae",
  "sp_green_algae",
  "sp_mobile_inverts",
  "sp_acid_weed",

  "sb_clay-wa",
  "sb_sand",
  "sb_pebble-wa",
  "sb_cobble",
  "sb_cobble-wa",
  "sb_rock",
  "sb_boulder",
  "sb_s_boulder-wa",
  "sb_l_boulder-wa",
  "sb_reef",
  "sb_shell_hash-wa",
  "sb_other",

  "cv_none",
  "cv_holdfast",
  "cv_brown_algae",
  "cv_acid_weed",
  "cv_green_algae",
  "cv_red_algae",
  "cv_encrusting",
  "cv_articulated",
  "cv_crustose",
  "cv_sessile_inverts",
  "cv_seagrass",

  "rf_0-10cm",
  "rf_10cm-1m",
  "rf_1m-2m",
  "rf_over_2m"
)

df_upc_wide <- df_upc_wide %>%
  select(
    key,
    basin,
    basin_id,
    site,
    site_id,
    transect,
    depth_zone,
    latitude,
    longitude,
    year,
    date,
    depth_ft,
    all_of(ordered_class_columns)
  )

df_upc_wide <- df_upc_wide |> group_by(site_id)
df_upc_wide <- arrange(df_upc_wide, .by_group = TRUE)

write.csv(df_upc_wide, file.path(results, "reef_check_upc_cleaned.csv"), row.names = FALSE)
