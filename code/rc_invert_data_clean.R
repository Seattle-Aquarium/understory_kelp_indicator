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

df_invert <- read.csv(file.path(data, "Invert_Washington_raw.csv"))

names(df_invert) <- tolower(names(df_invert))

## drop Canadian sites
df_invert <- subset(df_invert, !(site %in% canadian_sites))

df_invert <- df_invert %>%
  left_join(basin_lookup, by = "site")

## key
df_invert <- create.key(df_invert)

## add depth_zone column
df_invert <- add.depth.zone(df_invert)

## reorder columns
df_invert <- df_invert %>%
  select(
    key, basin, basin_id, site, site_id, transect, depth_zone,
    latitude, longitude, year, date, depth_ft,
    classcode, amount, distance
  )

## extrapolates data to distance of 30.0 m
df_invert <- df_invert %>%
  mutate(
    amount = as.integer(if_else(distance < 30, amount / distance * 30, amount)),
    distance = if_else(distance < 30, 30, distance)
  )

## clean strings
df_invert <- df_invert %>%
  mutate(across(where(is.character), ~ gsub(" ", "_", .x)))

names(df_invert) <- gsub(" ", "_", names(df_invert))

df_invert$classcode <- tolower(df_invert$classcode)

df_invert <- df_invert %>%
  mutate(classcode = recode(classcode,
                            "dawson's_sun_star" = "dawsons_sun_star",
                            "green/pallid_urchin" = "green_pallid_urchin",
                            "kelp_crab_(juvenile)" = "kelp_crab_juv"))

## sum individual counts
df_invert <- df_invert %>%
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
df_invert <- df_invert %>%
  arrange(site_id, desc(year), transect)

## pivot
df_invert_wide <- df_invert %>%
  select(key:depth_ft, classcode, amount) %>%
  pivot_wider(
    names_from = classcode,
    values_from = amount,
    values_fill = 0
  )

df_invert_wide <- df_invert_wide |> group_by(site_id)
df_invert_wide <- arrange(df_invert_wide, .by_group = TRUE)

write.csv(df_invert_wide, file.path(results, "reef_check_invert_cleaned.csv"), row.names = FALSE)
