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

df_algae <- read.csv(file.path(data, "Algae_Washington_raw.csv"))

names(df_algae) <- tolower(names(df_algae))

## drop Canadian sites
df_algae <- subset(df_algae, !(site %in% canadian_sites))

df_algae <- df_algae %>%
  left_join(basin_lookup, by = "site")

## key
df_algae <- create.key(df_algae)

## add depth_zone column
df_algae <- add.depth.zone(df_algae)

### Functional differences occur after this line
## reorder columns
df_algae <- df_algae %>%
  select(
    key, basin, basin_id, site, site_id, transect, depth_zone,
    latitude, longitude, year, date, depth_ft,
    classcode, amount, distance, stipes
  )

## extrapolates data to distance of 30.0 m
df_algae <- df_algae %>%
  mutate(
    amount = as.integer(if_else(distance < 30, amount / distance * 30, amount)),
    distance = if_else(distance < 30, 30, distance)
  )

## create summary classcodes for Giant Kelp and Feather Boa Kelp
df_summary <- df_algae %>%
  group_by(
    key, basin, basin_id, site, site_id,
    transect, depth_zone, latitude, longitude,
    year, date, depth_ft
  ) %>%
  summarise(
    giant_kelp_n = sum(amount[classcode == "Giant Kelp"], na.rm = TRUE),
    giant_kelp_stipe = sum(stipes[classcode == "Giant Kelp"], na.rm = TRUE),
    feather_boa_n = sum(amount[classcode == "Feather Boa Kelp"], na.rm = TRUE),
    feather_boa_stipe = sum(stipes[classcode == "Feather Boa Kelp"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = c(
      giant_kelp_n,
      giant_kelp_stipe,
      feather_boa_n,
      feather_boa_stipe
    ),
    names_to = "classcode",
    values_to = "amount"
  )

## append new records to original data
df_algae <- bind_rows(
  df_algae %>%
    select(-stipes) %>%
    filter(!(classcode %in% c("Giant Kelp", "Feather Boa Kelp"))),
  df_summary
)

## clean strings
df_algae <- df_algae %>%
  mutate(across(where(is.character), ~ gsub(" ", "_", .x)))

names(df_algae) <- gsub(" ", "_", names(df_algae))

df_algae$classcode <- tolower(df_algae$classcode)

## pivot
df_algae_wide <- df_algae %>%
  select(key:depth_ft, classcode, amount) %>%
  pivot_wider(
    names_from = classcode,
    values_from = amount
  )

df_algae_wide <- df_algae_wide |> group_by(site_id)
df_algae_wide <- arrange(df_algae_wide, .by_group = TRUE)

write.csv(df_algae_wide, file.path(results, "reef_check_algae_cleaned.csv"), row.names = FALSE)
