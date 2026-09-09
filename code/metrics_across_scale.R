rm(list=ls())
library(tidyverse)

## relative file paths
data <- "data/Reef_Check"
code <- "code"
results <- "results"
figs <- "figs"

setwd("../")

df_rc <- read.csv(file.path(results, "reef_check_cleaned.csv"), check.names = FALSE)

## survey metadata carried through from rc_full_data_clean.R; everything
## else is a raw count/percentage that needs to be scaled to a density
metadata_columns <- c(
  "key","basin","basin_id","site","site_id",
  "transect","depth_zone","latitude","longitude",
  "year","date","depth_ft"
)

## convert every non-metadata column to a density by dividing by 60.0, so
## all downstream calculations in this script operate on densities
df_rc <- df_rc %>%
  mutate(across(-all_of(metadata_columns), ~.x / 60.0))

## algae ("ak_") density columns only, excluding stipe counts (a raw
## stipe tally, not a density, so it doesn't belong in an averaged scale)
density_cols <- names(df_rc)[startsWith(names(df_rc), "ak_")]
density_cols <- density_cols[!density_cols %in% c("ak_giant_kelp_stipe", "ak_feather_boa_stipe")]

## _avg column produced by the previous scale (raw column names at the
## site scale, since that's the first level of aggregation)
avg_cols <- paste0(density_cols, "_avg")

## mean/sd/var of each density column, rounded to 2 decimals, with
## columns ordered density_avg | sd | var per category. na.rm = TRUE
## excludes missing values from both the sum and the denominator, rather
## than treating them as 0. input_cols may already carry an "_avg" suffix
## (when aggregating a scale that was itself built by this function) --
## that suffix is stripped before re-adding it, so output names never
## double up as "_avg_avg".
summarise_density_cols <- function(df, input_cols, ...){
  df %>%
    summarise(
      ...,
      across(
        all_of(input_cols),
        list(
          avg = ~round(mean(.x, na.rm = TRUE), 2),
          sd = ~round(sd(.x, na.rm = TRUE), 2),
          var = ~round(var(.x, na.rm = TRUE), 2)
        ),
        .names = "{sub('_avg$', '', .col)}_{.fn}"
      ),
      .groups = "drop"
    )
}

## ============================================================
## Site scale: n = 1 row per site per year (mean/sd/var of that
## site's transects within the year; n_transects records how many
## fed in)
## ============================================================

site_meta <- df_rc %>%
  distinct(basin, basin_id, site, site_id, latitude, longitude)

site_stats <- df_rc %>%
  group_by(site, year) %>%
  summarise_density_cols(density_cols, n_transects = n())

df_site <- site_meta %>%
  left_join(site_stats, by = "site")

write.csv(df_site, file.path(results, "metrics_site_scale.csv"), row.names = FALSE)

## ============================================================
## Basin scale: n = 1 row per basin per year (mean/sd/var of that
## basin's site means within the year, i.e. every site weighted
## equally regardless of transect count; n_sites records how many
## fed in)
## ============================================================

basin_meta <- df_site %>%
  distinct(basin, basin_id)

basin_stats <- df_site %>%
  group_by(basin, year) %>%
  summarise_density_cols(avg_cols, n_sites = n())

df_basin <- basin_meta %>%
  left_join(basin_stats, by = "basin")

write.csv(df_basin, file.path(results, "metrics_basin_scale.csv"), row.names = FALSE)

## ============================================================
## Puget Sound-wide scale: n = 1 row per year (mean/sd/var of the
## basin means within the year, i.e. every basin weighted equally
## regardless of site count; n_basins records how many fed in)
## ============================================================

df_puget_sound <- df_basin %>%
  group_by(year) %>%
  summarise_density_cols(avg_cols, n_basins = n()) %>%
  mutate(region = "Puget_Sound_wide", .before = 1)

write.csv(df_puget_sound, file.path(results, "metrics_puget_sound_scale.csv"), row.names = FALSE)
