## Temporal trend metric -- issue #2, after Frieder et al. (2025)
## Reads:  results/metrics_site_scale.csv   (written by metrics_across_scale.R)
## Writes: results/trend_site_scale.csv
##         results/trend_basin_scale.csv
##         results/trend_puget_sound_scale.csv
##
## For category s at site i in year t, the current density is compared with the
## mean density of the years before it (documents/temporal_trend_figure):
##
##   K[s,i,t] = 100 * D[s,i,t] / mean( D[s,i,t-1], ..., D[s,i,t-5] )
##
## so 100 is "the same as the preceding years", 50 is half and 200 is double.
## D is the site's annual mean density -- the _avg column of the site table, the
## mean of that year's transects -- and the baseline is the mean of those annual
## means over whichever of the five preceding years the site was surveyed in.
## (This replaces code/archive/algae_temporal_density.R, which pooled the raw
## transects of 2021-2024 instead: the same thing only when every year has six
## transects, and it computed 2025 alone.) K is computed for every year that has
## at least one baseline year, so it is a time series rather than one number.
##
## ---- 0, NA and Inf ---------------------------------------------------------
## A ratio of densities has three edge cases, and each gets a `status` so it is
## never confused with an ordinary value:
##
##   present  D > 0 and baseline > 0       K is an ordinary number
##   lost     D = 0, baseline > 0          K = 0 -- a real decline, kept
##   new      D > 0, baseline = 0          K would be Inf; stored as NA
##   absent   D = 0, baseline = 0          K would be NaN; stored as NA
##
## ---- From sites to basins to Puget Sound ----------------------------------
## Each larger scale is computed from the same pair of quantities, current and
## baseline density, averaged over the sites (or basins) that have both:
##
##   K[s,basin,t] = 100 * mean_i( D[s,i,t] ) / mean_i( baseline[s,i,t] )
##
## -- the ratio of the averages (trend_pct). The issue proposed the average of
## the site ratios instead, and that is written alongside as
## trend_pct_mean_of_sites, but it is not the headline number, because:
##
##   * a site whose baseline is nearly zero produces an enormous ratio and
##     swamps the average -- the 2025 site values run from 0 to 2,550%, and in
##     the San Juan Islands winged kelp averages 521% as a mean of site ratios
##     while the basin's density across those same sites is 102% of baseline;
##   * the "new" sites have to be thrown out (Inf) while the "lost" sites stay
##     in (0), which drags the average down; and the "absent" sites have to be
##     thrown out too.
##
## The ratio of averages has none of these problems: lost and new sites simply
## add their zeros to one side of the ratio, absent sites add nothing to
## either, and Inf or NaN can only happen when the category is new to, or
## absent from, every site in the basin. Only the sites that have both a
## current value and a baseline go into either average, so a basin's trend
## never compares this year's sites with a different set of earlier ones.
##
## Puget Sound repeats the step over basins, each basin weighted equally, just
## as metrics_across_scale.R weights them for the density tables.

rm(list=ls())
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

## relative file paths
results <- "results"

## set working directory to home folder
setwd("../")

## ---- FILE LOCATIONS AND SETTINGS ----------------------------------------------
## Run on its own it reads and writes the repository's results folder. The Shiny
## app in gui/ sets these environment variables first; see rc_full_data_clean.R
## for why environment variables rather than options().
from_env <- function(var, default) {
  value <- Sys.getenv(var, unset = "")
  if (nzchar(value)) value else default
}

site_csv        <- from_env("UKELP_SITE_CSV",        file.path(results, "metrics_site_scale.csv"))
trend_site_csv  <- from_env("UKELP_TREND_SITE_CSV",  file.path(results, "trend_site_scale.csv"))
trend_basin_csv <- from_env("UKELP_TREND_BASIN_CSV", file.path(results, "trend_basin_scale.csv"))
trend_puget_csv <- from_env("UKELP_TREND_PUGET_CSV", file.path(results, "trend_puget_sound_scale.csv"))

## how many preceding years make the baseline (five in the issue), and the fewest
## that will do. The survey only goes back to 2021, so a 2025 value has at most
## four baseline years and a site first surveyed in 2024 has one; every row
## records how many it had in n_baseline_years.
window_years       <- as.integer(from_env("UKELP_TREND_WINDOW", "5"))
min_baseline_years <- as.integer(from_env("UKELP_TREND_MIN_BASELINE_YEARS", "1"))

for (d in unique(dirname(c(trend_site_csv, trend_basin_csv, trend_puget_csv)))) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

df_site <- read.csv(site_csv, check.names = FALSE)

## ---- EXCLUDED BASINS -------------------------------------------------------
## Held out unless UKELP_INCLUDE_BASINS names them -- the same per-basin switch
## every other script honours. The site table is normally built without them
## already, so this is the second line of defense against an older csv.
held_out_basins <- c("Northern Coast",
                     "Western Strait of Juan de Fuca",
                     "Eastern Strait of Juan de Fuca")

include_basins <- trimws(strsplit(Sys.getenv("UKELP_INCLUDE_BASINS", unset = ""),
                                  ",", fixed = TRUE)[[1]])
include_basins <- intersect(held_out_basins, include_basins[nzchar(include_basins)])
excluded_basins <- setdiff(held_out_basins, include_basins)

df_site <- df_site %>% filter(!gsub("_", " ", basin) %in% excluded_basins)

## ============================================================
## Site scale
## ============================================================

## every category the site table carries, as its name without "_avg"
avg_cols <- grep("^(ak|cv|rf|sb|sp|iv|fs)_.*_avg$", names(df_site), value = TRUE)

annual <- df_site %>%
  select(basin, basin_id, site, site_id, year, all_of(avg_cols)) %>%
  pivot_longer(all_of(avg_cols), names_to = "category", values_to = "density") %>%
  mutate(category = sub("_avg$", "", category)) %>%
  filter(!is.na(density))

classify <- function(current, baseline) {
  case_when(current > 0 & baseline > 0 ~ "present",
            current == 0 & baseline > 0 ~ "lost",
            current > 0 & baseline == 0 ~ "new",
            TRUE                        ~ "absent")
}

## K is defined (finite) only for present and lost
trend_value <- function(current, baseline, status) {
  ifelse(status %in% c("present", "lost"), 100 * current / baseline, NA_real_)
}

## one row per site, category and year with at least min_baseline_years of
## baseline: the current density, and the mean of the annual densities in the
## window_years before it
baseline_for <- function(years, density) {
  lapply(years, function(t) {
    in_window <- years >= t - window_years & years < t
    list(baseline = if (any(in_window)) mean(density[in_window]) else NA_real_,
         n = sum(in_window),
         years = paste(sort(years[in_window]), collapse = ","))
  })
}

trend_site <- annual %>%
  group_by(basin, basin_id, site, site_id, category) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(.b = baseline_for(year, density),
         baseline = vapply(.b, `[[`, numeric(1), "baseline"),
         n_baseline_years = vapply(.b, `[[`, integer(1), "n"),
         baseline_years = vapply(.b, `[[`, character(1), "years")) %>%
  ungroup() %>%
  select(-.b) %>%
  rename(current = density) %>%
  filter(n_baseline_years >= min_baseline_years) %>%
  mutate(status = classify(current, baseline),
         trend_pct = trend_value(current, baseline, status)) %>%
  select(basin, basin_id, site, site_id, category, year, current, baseline,
         n_baseline_years, baseline_years, trend_pct, status) %>%
  arrange(basin_id, site_id, category, year)

## ============================================================
## Basin scale: the ratio of the matched-site averages
## ============================================================

## the average of the finite member ratios -- the issue's original proposal,
## written alongside for comparison. NA when no member has a finite ratio.
mean_finite <- function(x) {
  x <- x[is.finite(x)]
  if (length(x)) mean(x) else NA_real_
}

trend_basin <- trend_site %>%
  group_by(basin, basin_id, category, year) %>%
  summarise(n_sites = n(),
            current = mean(current),
            baseline = mean(baseline),
            trend_pct_mean_of_sites = mean_finite(trend_pct),
            n_sites_present = sum(status == "present"),
            n_sites_lost = sum(status == "lost"),
            n_sites_new = sum(status == "new"),
            n_sites_absent = sum(status == "absent"),
            .groups = "drop") %>%
  mutate(status = classify(current, baseline),
         trend_pct = trend_value(current, baseline, status),
         .after = baseline) %>%
  arrange(basin_id, category, year)

## ============================================================
## Puget Sound scale: the same step over basins
## ============================================================
## Two regions, as in metrics_across_scale.R: Puget_Sound is the inner basins
## only, whatever was included upstream; Washington_wide is every basin present
## and is written only when the outer three are in the data.
non_puget_basins <- c("Northern Coast",
                      "Western Strait of Juan de Fuca",
                      "Eastern Strait of Juan de Fuca")

region_trend <- function(df, label) {
  df %>%
    group_by(category, year) %>%
    summarise(n_basins = n(),
              n_sites = sum(n_sites),
              current = mean(current),
              baseline = mean(baseline),
              trend_pct_mean_of_basins = mean_finite(trend_pct),
              n_basins_present = sum(status == "present"),
              n_basins_lost = sum(status == "lost"),
              n_basins_new = sum(status == "new"),
              n_basins_absent = sum(status == "absent"),
              .groups = "drop") %>%
    mutate(status = classify(current, baseline),
           trend_pct = trend_value(current, baseline, status),
           .after = baseline) %>%
    mutate(region = label, .before = 1)
}

is_outer <- gsub("_", " ", trend_basin$basin) %in% non_puget_basins
trend_puget <- region_trend(trend_basin[!is_outer, ], "Puget_Sound")
if (any(is_outer)) {
  trend_puget <- bind_rows(trend_puget, region_trend(trend_basin, "Washington_wide"))
}
trend_puget <- trend_puget %>% arrange(region, category, year)

## ============================================================
## Write
## ============================================================

## 6 significant figures, as for the density tables
write_trend <- function(df, path) {
  df <- df %>% mutate(across(any_of(c("current", "baseline", "trend_pct",
                                      "trend_pct_mean_of_sites",
                                      "trend_pct_mean_of_basins")),
                             ~signif(.x, 6)))
  write.csv(df, path, row.names = FALSE)
  cat("Wrote", path, "--", nrow(df), "rows\n")
}

write_trend(trend_site, trend_site_csv)
write_trend(trend_basin, trend_basin_csv)
write_trend(trend_puget, trend_puget_csv)

## a line per status, so an odd run shows up in the log
cat(sprintf("Site values: %s\n", paste(names(table(trend_site$status)),
                                        table(trend_site$status), sep = " ", collapse = ", ")))
cat(sprintf("Baseline window: %d years; at least %d baseline year(s) required\n",
            window_years, min_baseline_years))
