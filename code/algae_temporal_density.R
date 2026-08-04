rm(list=ls())
library(tidyverse)

## relative file paths
data <- "data/Reef_Check"
code <- "code"
results <- "results"
figs <- "figs"

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

