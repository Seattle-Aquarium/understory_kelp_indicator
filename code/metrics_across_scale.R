rm(list=ls())
library(tidyverse)

## relative file paths
data <- "data/Reef_Check"
code <- "code"
results <- "results"
figs <- "figs"

setwd("../")

## ---- FILE LOCATIONS --------------------------------------------------------
## Run this script on its own and it reads and writes the repository's own
## folders, exactly as it always has. The Shiny app in gui/ sets the environment
## variables below first, so a person can point it at files kept somewhere else
## without editing anything here. See rc_full_data_clean.R for why environment
## variables rather than options().
from_env <- function(var, default) {
  value <- Sys.getenv(var, unset = "")
  if (nzchar(value)) value else default
}

cleaned_csv <- from_env("UKELP_CLEANED_CSV", file.path(results, "reef_check_cleaned.csv"))
site_csv    <- from_env("UKELP_SITE_CSV",    file.path(results, "metrics_site_scale.csv"))
basin_csv   <- from_env("UKELP_BASIN_CSV",   file.path(results, "metrics_basin_scale.csv"))
puget_csv   <- from_env("UKELP_PUGET_CSV",   file.path(results, "metrics_puget_sound_scale.csv"))

for (d in unique(dirname(c(site_csv, basin_csv, puget_csv)))) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

df_rc <- read.csv(cleaned_csv, check.names = FALSE)

## ---- EXCLUDED BASINS -------------------------------------------------------
## Northern Coast, Western Strait of Juan de Fuca and Eastern Strait of Juan de
## Fuca are held out of every output in this repo. TO BRING THEM BACK: delete
## this block -- nothing else depends on the exclusion, and the raw files in
## data/Reef_Check still contain every basin. Search the repo for
## "EXCLUDED BASINS" to find all of the places this is applied.
##
## The cleaning scripts drop these basins too, so this is a second line of
## defense that also covers re-running against an older cleaned csv.
## ----------------------------------------------------------------------------
## These three are held out by default. UKELP_INCLUDE_BASINS names any that
## should be kept after all, comma separated -- that is the per-basin checkboxes
## in the Shiny app, which can bring back one, two or all three independently.
## Unset, which is how this script runs on its own, all three stay excluded
## exactly as before.
held_out_basins <- c("Northern Coast",
                     "Western Strait of Juan de Fuca",
                     "Eastern Strait of Juan de Fuca")

include_basins <- trimws(strsplit(Sys.getenv("UKELP_INCLUDE_BASINS", unset = ""),
                                  ",", fixed = TRUE)[[1]])
include_basins <- intersect(held_out_basins, include_basins[nzchar(include_basins)])

excluded_basins <- setdiff(held_out_basins, include_basins)

if (length(include_basins)) {
  message("Including: ", paste(include_basins, collapse = ", "))
}

df_rc <- df_rc %>% filter(!gsub("_", " ", basin) %in% excluded_basins)

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

## mean/sd/var of each metric column, rounded to 3 decimals, with
## columns ordered metric_avg | sd | var per category. na.rm = TRUE
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
          avg = ~round(mean(.x, na.rm = TRUE), 3),
          sd = ~round(sd(.x, na.rm = TRUE), 3),
          var = ~round(var(.x, na.rm = TRUE), 3)
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

write.csv(df_site, site_csv, row.names = FALSE)
cat("Wrote", site_csv, "--", nrow(df_site), "rows,", ncol(df_site), "columns
")

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

write.csv(df_basin, basin_csv, row.names = FALSE)
cat("Wrote", basin_csv, "--", nrow(df_basin), "rows,", ncol(df_basin), "columns
")

## ============================================================
## Region scale: n = 1 row per region per year (mean/sd/var of the
## basin means within the year, i.e. every basin weighted equally
## regardless of site count; n_basins records how many fed in)
##
## Two regions, written to the same file and told apart by `region`:
##
##   Puget_Sound      the inner basins only -- the outer coast and the two
##                    Straits carved out, whether or not they are in the data
##   Washington_wide  every basin present
##
## When the outer three are absent the two would be identical, so only
## Puget_Sound is written. That absence is what the Shiny app reads to grey out
## its Washington-wide option, so it is load-bearing rather than tidiness.
## ============================================================

## Deliberately separate from `excluded_basins` above: that one empties when the
## include flag is set, and this carve-out has to hold either way.
non_puget_basins <- c("Northern Coast",
                      "Western Strait of Juan de Fuca",
                      "Eastern Strait of Juan de Fuca")

basin_is_outer <- gsub("_", " ", as.character(df_basin$basin)) %in% non_puget_basins

region_rows <- function(df, label) {
  df %>%
    group_by(year) %>%
    summarise_metric_cols(avg_cols, n_basins = n()) %>%
    mutate(region = label, .before = 1)
}

df_puget_sound <- region_rows(df_basin[!basin_is_outer, , drop = FALSE], "Puget_Sound")

if (any(basin_is_outer)) {
  df_puget_sound <- bind_rows(df_puget_sound, region_rows(df_basin, "Washington_wide"))
  message("Writing both Puget_Sound and Washington_wide regions.")
} else {
  message("Outer basins absent -- writing Puget_Sound only.")
}

write.csv(df_puget_sound, puget_csv, row.names = FALSE)
cat("Wrote", puget_csv, "--", nrow(df_puget_sound), "rows,", ncol(df_puget_sound), "columns
")
