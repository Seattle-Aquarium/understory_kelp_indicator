rm(list=ls())
library(tidyverse)

## relative file paths
data <- "data/Reef_Check"
code <- "code"
results <- "results"
figs <- "figs"

setwd("../")

df_rc <- read.csv(file.path(results, "reef_check_cleaned.csv"), check.names = FALSE)

## survey metadata carried through from rc_full_data_clean.R
metadata_columns <- c(
  "key","basin","basin_id","site","site_id",
  "transect","depth_zone","latitude","longitude",
  "year","date","depth_ft"
)

## matches every column carrying any of `prefixes`. NOTE: startsWith() recycles
## its prefix argument element-wise instead of testing each column against every
## prefix, so startsWith(nm, c("ak_","cv_")) silently keeps only the columns
## whose prefix happens to line up with their position -- hence the regex
## alternation here. "_" carries no regex meaning, so the prefixes paste in as-is
cols_with_prefix <- function(nm, prefixes) {
  grep(paste0("^(", paste(prefixes, collapse = "|"), ")"), nm, value = TRUE)
}

## UPC columns ("cv_" cover, "rf_" relief, "sb_" substrate, "sp_" species) are
## already proportions of the points sampled along a transect -- each prefix
## group sums to 1 within a row -- so they are carried through unscaled. Dividing
## them by the transect area would push their ceiling to 1/60 and make them
## incomparable with everything else.
prop_prefixes <- c("cv_", "rf_", "sb_", "sp_")
prop_cols <- cols_with_prefix(names(df_rc), prop_prefixes)

## every remaining non-metadata column is a raw tally, which becomes a density
## when divided by the area of the transect it was counted on
transect_area_m2 <- 60.0

df_rc <- df_rc %>%
  mutate(across(-all_of(c(metadata_columns, prop_cols)), ~.x / transect_area_m2))

## algae ("ak_") densities, excluding stipe counts (a raw stipe tally, not a
## density, so it doesn't belong in an averaged scale)
count_cols <- cols_with_prefix(names(df_rc), "ak_")
count_cols <- setdiff(count_cols, c("ak_giant_kelp_stipe", "ak_feather_boa_stipe"))

## the columns carried through all three scales: algae densities followed by the
## UPC proportions. The counts reported here are worth a glance on each run --
## a silent drop is what the startsWith() recycling above caused
metric_cols <- c(count_cols, prop_cols)
message("carrying ", length(count_cols), " algae density columns and ",
        length(prop_cols), " UPC proportion columns through the scales")

## _avg column produced by the previous scale (raw column names at the
## site scale, since that's the first level of aggregation)
avg_cols <- paste0(metric_cols, "_avg")

## mean/sd/var of each density column, rounded to 2 decimals, with
## columns ordered density_avg | sd | var per category. na.rm = TRUE
## excludes missing values from both the sum and the denominator, rather
## than treating them as 0. input_cols may already carry an "_avg" suffix
## (when aggregating a scale that was itself built by this function) --
## that suffix is stripped before re-adding it, so output names never
## double up as "_avg_avg".
summarise_metric_cols <- function(df, input_cols, ...){
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
  summarise_metric_cols(metric_cols, n_transects = n())

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
  summarise_metric_cols(avg_cols, n_sites = n())

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
  summarise_metric_cols(avg_cols, n_basins = n()) %>%
  mutate(region = "Puget_Sound_wide", .before = 1)

write.csv(df_puget_sound, file.path(results, "metrics_puget_sound_scale.csv"), row.names = FALSE)
