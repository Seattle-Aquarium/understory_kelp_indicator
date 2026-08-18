rm(list=ls())
library(tidyverse)

## relative file paths
data <- "data/Reef_Check"
code <- "code"
results <- "results"
figs <- "figs"

#setwd("../")
#getwd()

df_rc <- read.csv(file.path(results, "reef_check_cleaned.csv"), check.names = FALSE)

## algae ("ak_") kelp category columns, excluding stipe counts
ak_cols <- names(df_rc)[startsWith(names(df_rc), "ak_")]
ak_cols <- ak_cols[!ak_cols %in% c("ak_giant_kelp_stipe", "ak_feather_boa_stipe")]

## site-level metadata (constant within each site)
site_meta <- df_rc %>%
  distinct(basin, basin_id, site, site_id, latitude, longitude)

## mean of each kelp category across all 2025 transects, per site
mean_2025 <- df_rc %>%
  filter(year == 2025) %>%
  select(site, all_of(ak_cols)) %>%
  pivot_longer(all_of(ak_cols), names_to = "kelp_category", values_to = "amount") %>%
  group_by(site, kelp_category) %>%
  summarise(mean_2025 = mean(amount, na.rm = TRUE), .groups = "drop")

## mean of each kelp category across all 2021-2024 transects (combined), per site
mean_prior <- df_rc %>%
  filter(year %in% 2021:2024) %>%
  select(site, all_of(ak_cols)) %>%
  pivot_longer(all_of(ak_cols), names_to = "kelp_category", values_to = "amount") %>%
  group_by(site, kelp_category) %>%
  summarise(mean_prior = mean(amount, na.rm = TRUE), .groups = "drop")

## 2025 density relative to 2021-2024 average, expressed as a percentage
rel_density <- full_join(mean_2025, mean_prior, by = c("site", "kelp_category")) %>%
  mutate(pct_of_prior_avg = round(mean_2025 / mean_prior * 100, 2)) %>%
  select(site, kelp_category, pct_of_prior_avg) %>%
  pivot_wider(names_from = kelp_category, values_from = pct_of_prior_avg)

## combine with site metadata and write out
df_out <- site_meta %>%
  left_join(rel_density, by = "site")

write.csv(df_out, file.path(results, "algae_temporal_density.csv"), row.names = FALSE)

## average pct_of_prior_avg per kelp category, across sites within each basin
## NA (0/0, no kelp in either period) and Inf (kelp newly appeared from a
## zero baseline, undefined as a percentage) are excluded from the average --
## true zeros (kelp present at baseline, absent in 2025) are kept, since
## dropping them would bias the average upward by excluding real declines
## all basin x kelp_category combos are kept (as NA) even when no valid values
## remain, so every basin still gets a row
basin_lookup <- distinct(df_out, basin, basin_id)
basin_species <- basin_lookup %>%
  expand_grid(kelp_category = ak_cols)

## number of sites per basin with a valid (non-NA, finite) kelp value --
## used as the basin row's overall sample size
n_sites_basin <- df_out %>%
  select(basin, site, all_of(ak_cols)) %>%
  pivot_longer(all_of(ak_cols), names_to = "kelp_category", values_to = "pct_of_prior_avg") %>%
  filter(!is.na(pct_of_prior_avg), is.finite(pct_of_prior_avg)) %>%
  distinct(basin, site) %>%
  count(basin, name = "n_sites")

basin_avg <- df_out %>%
  select(basin, all_of(ak_cols)) %>%
  pivot_longer(all_of(ak_cols), names_to = "kelp_category", values_to = "pct_of_prior_avg") %>%
  filter(!is.na(pct_of_prior_avg), is.finite(pct_of_prior_avg)) %>%
  group_by(basin, kelp_category) %>%
  summarise(mean_pct = round(mean(pct_of_prior_avg), 2), .groups = "drop") %>%
  right_join(basin_species, by = c("basin", "kelp_category")) %>%
  pivot_wider(names_from = kelp_category, values_from = mean_pct) %>%
  left_join(n_sites_basin, by = "basin") %>%
  mutate(n_sites = replace_na(n_sites, 0L)) %>%
  arrange(basin_id) %>%
  relocate(basin_id, .after = basin) %>%
  relocate(n_sites, .after = basin)

write.csv(basin_avg, file.path(results, "algae_temporal_density_by_basin.csv"), row.names = FALSE)

## average each kelp category's basin-level value across all basins, for a
## single Puget-Sound-wide value per taxon. basins with NA (no valid site
## data) are dropped from both the sum and the denominator rather than
## treated as 0
puget_sound_avg <- basin_avg %>%
  select(basin, all_of(ak_cols)) %>%
  pivot_longer(all_of(ak_cols), names_to = "kelp_category", values_to = "mean_pct") %>%
  group_by(kelp_category) %>%
  summarise(
    n_basins = sum(!is.na(mean_pct)),
    puget_sound_pct_of_prior_avg = round(mean(mean_pct, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  mutate(puget_sound_pct_of_prior_avg = ifelse(is.nan(puget_sound_pct_of_prior_avg), NA, puget_sound_pct_of_prior_avg))

write.csv(puget_sound_avg, file.path(results, "algae_temporal_density_puget_sound.csv"), row.names = FALSE)

