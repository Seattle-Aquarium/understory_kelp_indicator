rm(list=ls())
library(tidyverse)

## relative file paths
data <- "data/Reef_Check"
code <- "code"
results <- "results"
figs <- "figs"

df_rc <- read.csv(file.path(results, "reef_check_cleaned.csv"), check.names = FALSE)

## metadata columns (identify a survey; not density/cover measurements)
meta_cols <- c("key", "basin", "basin_id", "site", "site_id", "transect",
               "depth_zone", "latitude", "longitude", "year", "date", "depth_ft")

## every other column is a density/cover measurement, excluding stipe counts
## (a raw stipe tally, not a density, so it doesn't belong in an averaged scale)
density_cols <- setdiff(names(df_rc), meta_cols)
density_cols <- density_cols[!density_cols %in% c("ak_giant_kelp_stipe", "ak_feather_boa_stipe")]

## ============================================================
## Transect scale: n = 1 row per transect (raw, as collected)
## ============================================================

df_transect <- df_rc %>%
  select(all_of(meta_cols), all_of(density_cols))

write.csv(df_transect, file.path(results, "densities_transect_scale.csv"), row.names = FALSE)

## ============================================================
## Site scale: n = 1 row per site (mean of that site's transects,
## all years pooled)
## ============================================================

site_meta <- df_transect %>%
  distinct(basin, basin_id, site, site_id, latitude, longitude)

site_means <- df_transect %>%
  group_by(site) %>%
  summarise(across(all_of(density_cols), ~mean(.x, na.rm = TRUE)), .groups = "drop")

df_site <- site_meta %>%
  left_join(site_means, by = "site")

write.csv(df_site, file.path(results, "densities_site_scale.csv"), row.names = FALSE)

## ============================================================
## Basin scale: n = 1 row per basin (mean of that basin's site
## means, i.e. every site weighted equally regardless of transect
## count)
## ============================================================

basin_meta <- df_site %>%
  distinct(basin, basin_id)

basin_means <- df_site %>%
  group_by(basin) %>%
  summarise(across(all_of(density_cols), ~mean(.x, na.rm = TRUE)), .groups = "drop")

df_basin <- basin_meta %>%
  left_join(basin_means, by = "basin")

write.csv(df_basin, file.path(results, "densities_basin_scale.csv"), row.names = FALSE)

## ============================================================
## Puget Sound-wide scale: n = 1 row (mean of the basin means,
## i.e. every basin weighted equally regardless of site count)
## ============================================================

df_puget_sound <- df_basin %>%
  summarise(across(all_of(density_cols), ~mean(.x, na.rm = TRUE))) %>%
  mutate(region = "Puget_Sound_wide", .before = 1)

write.csv(df_puget_sound, file.path(results, "densities_puget_sound_scale.csv"), row.names = FALSE)