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

df_fish <- read.csv(file.path(data, "Fish_Washington_raw.csv"))

names(df_fish) <- tolower(names(df_fish))

## drop Canadian sites
df_fish <- subset(df_fish, !(site %in% canadian_sites))

df_fish <- df_fish %>%
  left_join(basin_lookup, by = "site")

## keep only the first six transects
df_fish <- df_fish %>%
  filter(transect %in% 1:6)

## key
df_fish <- create.key(df_fish)

## add depth_zone column
df_fish <- add.depth.zone(df_fish)

## reorder columns
df_fish <- df_fish %>%
  select(
    key, basin, basin_id, site, site_id, transect, depth_zone,
    latitude, longitude, year, date, depth_ft,
    classcode, amount
  )

## clean strings
df_fish <- df_fish %>%
  mutate(across(where(is.character), ~ gsub(" ", "_", .x)))

names(df_fish) <- gsub(" ", "_", names(df_fish))

df_fish$classcode <- tolower(df_fish$classcode)

df_fish <- df_fish %>%
  mutate(classcode = recode(classcode,
                            "black/yellowtail_yoy" = "black_yellowtail_yoy",
                            "blue/deacon_yoy" = "blue_deacon_yoy",
                            "brown/copper/quillback_yoy" = "brown_copper_quillback_yoy"))

## sum individual counts
df_fish <- df_fish %>%
  group_by(
    key, basin, basin_id, site, site_id,
    transect, depth_zone,
    latitude, longitude,
    year, date, depth_ft,
    classcode
  ) %>%
  summarise(
    amount = sum(amount),
    .groups = "drop"
  )

## corrects cell ordering
df_fish <- df_fish %>%
  arrange(site_id, desc(year), transect)

## pivot
df_fish_wide <- df_fish %>%
  select(key:depth_ft, classcode, amount) %>%
  pivot_wider(
    names_from = classcode,
    values_from = amount,
    values_fill = 0
  )

df_fish_wide <- df_fish_wide |> group_by(site_id)
df_fish_wide <- arrange(df_fish_wide, .by_group = TRUE)

write.csv(df_fish_wide, file.path(results, "reef_check_fish_cleaned.csv"), row.names = FALSE)
