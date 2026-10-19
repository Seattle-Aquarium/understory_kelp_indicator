rm(list=ls())
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

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

cleaned_csv  <- from_env("UKELP_CLEANED_CSV",  file.path(results, "reef_check_cleaned.csv"))
transect_csv <- from_env("UKELP_TRANSECT_CSV", file.path(results, "metrics_transect_scale.csv"))
site_csv     <- from_env("UKELP_SITE_CSV",     file.path(results, "metrics_site_scale.csv"))
basin_csv    <- from_env("UKELP_BASIN_CSV",    file.path(results, "metrics_basin_scale.csv"))
puget_csv    <- from_env("UKELP_PUGET_CSV",    file.path(results, "metrics_puget_sound_scale.csv"))

for (d in unique(dirname(c(transect_csv, site_csv, basin_csv, puget_csv)))) {
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

## UPC columns ("cv_" cover, "rf_" relief, "sb_" substrate, "sp_" superlayer)
## are already proportions of the points sampled along a transect, so they are
## carried through unscaled. Cover, relief and substrate each sum to 1 within a
## row; superlayer does not -- most points have no superlayer at all. Dividing
## them by the transect area would push their ceiling to 1/60 and make them
## incomparable with everything else.
prop_prefixes <- c("cv_", "rf_", "sb_", "sp_")
prop_cols <- cols_with_prefix(names(df_rc), prop_prefixes)

## algae ("ak_"), invertebrate ("iv_") and fish ("fs_") columns are raw tallies,
## which become a density -- individuals per m^2 -- when divided by the area of
## the transect they were counted on. The two stipe columns are left out: a
## stipe tally is not a density of anything, so it doesn't belong in an
## averaged scale (it stays in the cleaned table).
transect_area_m2 <- 60.0

ak_cols <- setdiff(cols_with_prefix(names(df_rc), "ak_"),
                   c("ak_giant_kelp_stipe", "ak_feather_boa_stipe"))
iv_cols <- cols_with_prefix(names(df_rc), "iv_")
fs_cols <- cols_with_prefix(names(df_rc), "fs_")
count_cols <- c(ak_cols, iv_cols, fs_cols)

df_rc <- df_rc %>%
  mutate(across(all_of(count_cols), ~.x / transect_area_m2))

## the columns carried through all four scales: algae, invert and fish
## densities, then the UPC proportions. The counts reported here are worth a
## glance on each run -- a silent drop is what the startsWith() recycling
## above caused
metric_cols <- c(ak_cols, prop_cols, iv_cols, fs_cols)
message("carrying ", length(ak_cols), " algae, ", length(prop_cols), " UPC, ",
        length(iv_cols), " invertebrate and ", length(fs_cols),
        " fish columns through the scales")

## Which transects each dataset actually covers. A transect row exists if any
## of the four was surveyed on it, so counting rows overstates a dataset that
## missed a transect (Cattle Point 2024 has 6 rows but 5 algae transects).
has_data <- function(df, cols) rowSums(!is.na(df[cols])) > 0
df_rc <- df_rc %>%
  mutate(.algae  = has_data(df_rc, ak_cols),
         .upc    = has_data(df_rc, prop_cols),
         .invert = has_data(df_rc, iv_cols),
         .fish   = has_data(df_rc, fs_cols))

## _avg column produced by the previous scale (raw column names at the
## site scale, since that's the first level of aggregation)
avg_cols <- paste0(metric_cols, "_avg")

## mean/sd/var of each metric column, with columns ordered metric_avg | sd |
## var per category. na.rm = TRUE excludes missing values from both the sum and
## the denominator, rather than treating them as 0; a category with no values
## at all is NA rather than NaN. input_cols may already carry an "_avg" suffix
## (when aggregating a scale that was itself built by this function) -- that
## suffix is stripped before re-adding it, so output names never double up as
## "_avg_avg".
##
## Nothing is rounded here. Each scale used to be rounded to 3 decimals and the
## next scale built from the rounded values: 145 of 854 non-zero site variances
## became exactly 0, and small site means were off by up to 8%. write_scale()
## trims to 6 significant figures on the way out, and only there.
mean_or_na <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)

summarise_metric_cols <- function(df, input_cols, ...){
  df %>%
    summarise(
      ...,
      across(
        all_of(input_cols),
        list(
          avg = ~mean_or_na(.x),
          sd = ~sd(.x, na.rm = TRUE),
          var = ~var(.x, na.rm = TRUE)
        ),
        .names = "{sub('_avg$', '', .col)}_{.fn}"
      ),
      .groups = "drop"
    )
}

## 6 significant figures keeps the files readable without the damage rounding
## to a fixed number of decimals did to small densities. Only the metric columns
## -- latitude and longitude need every digit they have.
write_scale <- function(df, path, cols) {
  df <- df %>% mutate(across(any_of(cols), ~signif(.x, 6)))
  write.csv(df, path, row.names = FALSE)
  cat("Wrote", path, "--", nrow(df), "rows,", ncol(df), "columns\n")
}

stat_cols <- function(df) grep("_(avg|sd|var)$", names(df), value = TRUE)

## ============================================================
## Transect scale: n = 1 row per transect survey, the cleaned table
## in density units -- individuals per m^2 for algae, invertebrates
## and fish, proportion of points for UPC. The starting point for
## anything run on individual transects (NMDS, GLMMs).
## ============================================================

df_transect <- df_rc %>%
  select(all_of(metadata_columns), all_of(metric_cols))

write_scale(df_transect, transect_csv, metric_cols)

## ============================================================
## Site scale: n = 1 row per site per year (mean/sd/var of that
## site's transects within the year; n_transects records how many
## transect rows fed in, and n_transects_<dataset> how many of them
## that dataset was actually surveyed on)
## ============================================================

site_meta <- df_rc %>%
  distinct(basin, basin_id, site, site_id, latitude, longitude)

site_stats <- df_rc %>%
  group_by(site, year) %>%
  summarise_metric_cols(metric_cols,
                        n_transects = n(),
                        n_transects_algae = sum(.algae),
                        n_transects_upc = sum(.upc),
                        n_transects_invert = sum(.invert),
                        n_transects_fish = sum(.fish))

df_site <- site_meta %>%
  left_join(site_stats, by = "site")

write_scale(df_site, site_csv, stat_cols(df_site))

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

write_scale(df_basin, basin_csv, stat_cols(df_basin))

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

write_scale(df_puget_sound, puget_csv, stat_cols(df_puget_sound))
