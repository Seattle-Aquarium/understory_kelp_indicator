## start up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list=ls())

library(tidyverse)
library(tibble)
library(dplyr)
library(tidyr)

setwd("C:/Users/thomsonr/Seattle Aquarium Dropbox/Coastal_Climate_Resilience/GitHub/understory_kelp_indicator")

data <- "data/Reef_Check"
results <- "results"

df_upc <- read.csv(file.path(data, "UPC_Washington_raw.csv"))

## helper function
front.ofthe.line <- function(df_upc){
  df_upc[c(ncol(df_upc), 1:(ncol(df_upc) - 1))]
}

names(df_upc) <- tolower(names(df_upc))

## drop Canadian sites
df_upc <- subset(df_upc, !(site %in% c(
  "7751 Reef","Ogden Point","Spring Bay","Cates Park",
  "Lions Gate","Ferguson Point","South Bowyer Island",
  "Christie Islet","Whytecliff Park"
)))

## basin lookup
basin_lookup <- tribble(
  ~basin, ~basin_id, ~site, ~site_id,
  "South Puget Sound", 1, "Devils Head", 1,
  "South Puget Sound", 1, "Ketron Island", 2,
  "South Puget Sound", 1, "Squaxin Island", 3,
  "South Puget Sound", 1, "Fox Island East Wall", 4,
  "South Puget Sound", 1, "Titlow Beach", 5,
  
  "Central Puget Sound", 2, "Salmon Beach", 6,
  "Central Puget Sound", 2, "Owen Beach", 7,
  "Central Puget Sound", 2, "Point Dalco", 8,
  "Central Puget Sound", 2, "Saltwater State Park", 9,
  "Central Puget Sound", 2, "Glen Acres", 11,
  "Central Puget Sound", 2, "Point Vashon", 12,
  "Central Puget Sound", 2, "Blake Island South", 13,
  "Central Puget Sound", 2, "Lincoln Park", 14,
  "Central Puget Sound", 2, "Seattle Waterfront", 15,
  "Central Puget Sound", 2, "Wing Point", 16,
  "Central Puget Sound", 2, "Sirens of Spring", 17,
  "Central Puget Sound", 2, "Grain Terminal", 18,
  "Central Puget Sound", 2, "Elliot Bay Marina", 19,
  "Central Puget Sound", 2, "Magnolia", 20,
  "Central Puget Sound", 2, "Jefferson Head", 23,
  "Central Puget Sound", 2, "Edmonds Shell Creek", 24,
  
  "Hood Canal", 3, "Sund Rock", 10,
  "Hood Canal", 3, "Goby Gardens", 21,
  "Hood Canal", 3, "Pulali Point", 22,
  
  "Northern Coast", 4, "Teahwhit Head", 25,
  "Northern Coast", 4, "Rock 305", 28,
  
  "Admiralty Inlet", 5, "Possession Point", 26,
  "Admiralty Inlet", 5, "Foulweather Bluff", 27,
  
  "Saratoga/Whidbey Basin", 6, "South Hat Island", 29,
  "Saratoga/Whidbey Basin", 6, "Lowell Point", 31,
  
  "Eastern Strait of Juan de Fuca", 7, "Green Point", 30,
  "Eastern Strait of Juan de Fuca", 7, "Dallas Banks", 32,
  "Eastern Strait of Juan de Fuca", 7, "McCurdy Point", 33,
  "Eastern Strait of Juan de Fuca", 7, "Lower Elwha", 34,
  "Eastern Strait of Juan de Fuca", 7, "Freshwater Bay", 35,
  "Eastern Strait of Juan de Fuca", 7, "North Beach", 36,
  "Eastern Strait of Juan de Fuca", 7, "Tongue Point", 37,
  "Eastern Strait of Juan de Fuca", 7, "Ebeys Landing", 38,
  "Eastern Strait of Juan de Fuca", 7, "Partridge Point", 39,
  "Eastern Strait of Juan de Fuca", 7, "Smith Island", 43,
  "Eastern Strait of Juan de Fuca", 7, "Rosario Head", 44,
  
  "Western Strait of Juan de Fuca", 8, "Clallam Bay West", 40,
  "Western Strait of Juan de Fuca", 8, "Sekiu Point", 41,
  
  "San Juan Islands", 9, "Watmough Bay", 45,
  "San Juan Islands", 9, "Cattle Point", 46,
  "San Juan Islands", 9, "Eagle Cove", 47,
  "San Juan Islands", 9, "Deadmans Bay", 49,
  "San Juan Islands", 9, "Reef Point", 50,
  "San Juan Islands", 9, "Smallpox Bay", 51,
  "San Juan Islands", 9, "South Shaw Island", 52,
  "San Juan Islands", 9, "Point Caution", 53,
  "San Juan Islands", 9, "Reuben Tarte", 54,
  "San Juan Islands", 9, "Lawrence Point", 55,
  "San Juan Islands", 9, "Satellite Island", 56,
  
  "North Puget Sound", 10, "Burrows Lighthouse", 48,
  "North Puget Sound", 10, "Derelict Conveyor", 57,
  "North Puget Sound", 10, "Point Whitehorn", 58
)

df_upc <- df_upc %>%
  left_join(basin_lookup, by = "site")

df_upc <- df_upc[, c(13,14,1,15,6,2:5,7,9:12)]

## key
create.key <- function(df_upc){
  df_upc$key <- paste(df_upc$basin_id, df_upc$site_id, df_upc$transect,
                      substr(as.character(df_upc$year), 3, 4), sep = "_")
  front.ofthe.line(df_upc)
}

df_upc <- create.key(df_upc)

## scale adjustment
df_upc <- df_upc %>%
  mutate(
    adjust = total_amount != 30,
    amount = if_else(adjust, as.integer(amount / total_amount * 30), amount),
    total_amount = if_else(adjust, 30, total_amount)
  ) %>%
  select(-adjust)

## clean strings
df_upc <- df_upc %>%
  mutate(across(where(is.character), ~ gsub(" ", "_", .x)))

names(df_upc) <- gsub(" ", "_", names(df_upc))

df_upc$classcode <- tolower(df_upc$classcode)

## FIXED RECODE STEP (no collision problems left unresolved)
df_upc <- df_upc %>%
  mutate(classcode = recode(classcode,
                            ">2m" = "relief_over_2m",
                            "0-10cm" = "relief_0-10cm",
                            "10cm-1m" = "relief_10cm-1m",
                            "1m-2m" = "relief_1m-2m",
                            "boulder" = "substrate_boulder", ### Flagged for correction, pending Shawn's input
                            "clay" = "substrate_clay",
                            "cobble" = "substrate_cobble",
                            "cobble_(5-15cm-wa)" = "substrate_cobble",
                            "large_boulder_(50cm-1m-wa)" = "substrate_large_boulder",
                            "pebble_(0.5-5cm-wa)" = "substrate_pebble",
                            "reef" = "substrate_reef",
                            "rock_(15-25cm-wa)" = "substrate_rock",
                            "sand" = "substrate_sand",
                            "shell_hash" = "substrate_shell_hash",
                            "small_boulder_(25-50cm-wa)" = "substrate_small_boulder",
                            "other" = "substrate_other"
  )) %>%
  
  ## 🔥 CRITICAL FIX: remove duplicate (key, classcode) rows
  group_by(key, basin, basin_id, site, site_id, transect,
           latitude, longitude, year, date, depth_ft, classcode) %>%
  summarise(percentage = sum(percentage) / 100, .groups = "drop")

## pivot
df_wide <- df_upc %>%
  select(key:depth_ft, classcode, percentage) %>%
  pivot_wider(
    names_from = classcode,
    values_from = percentage,
    values_fn = sum,
    values_fill = 0
  )

write.csv(df_wide, "results/reef_check_upc_cleaned.csv", row.names = FALSE)