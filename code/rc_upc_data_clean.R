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

df_upc <- read.csv(file.path(data, "UPC_Washington_raw.csv"))

## helper function
front.ofthe.line <- function(df_upc){
  df_upc[c(ncol(df_upc), 1:(ncol(df_upc) - 1))]
}

names(df_upc) <- tolower(names(df_upc))

## drop Canadian sites
df_upc <- subset(df_upc, !(site %in% c( "7751 Reef","Ogden Point","Spring Bay","Cates Park",
                                        "Lions Gate","Ferguson Point","South Bowyer Island",
                                        "Christie Islet","Whytecliff Park")))

## basin lookup
basin_lookup <- tribble(
  ~basin, ~basin_id, ~site, ~site_id,

# 1. South Puget Sound
  "South Puget Sound", 1, "Devils Head", 1,
  "South Puget Sound", 1, "Ketron Island", 2,
  "South Puget Sound", 1, "Squaxin Island", 3,
  "South Puget Sound", 1, "Fox Island East Wall", 4,
  "South Puget Sound", 1, "Titlow Beach", 5,

# 2. Central Puget Sound  
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
  
# 3. Hood Canal
  "Hood Canal", 3, "Sund Rock", 10,
  "Hood Canal", 3, "Goby Gardens", 21,
  "Hood Canal", 3, "Pulali Point", 22,
  
# 4. Northern Coast
  "Northern Coast", 4, "Teahwhit Head", 25,
  "Northern Coast", 4, "Rock 305", 28,

# 5. Admiralty Inlet  
  "Admiralty Inlet", 5, "Possession Point", 26,
  "Admiralty Inlet", 5, "Foulweather Bluff", 27,

# 6. Saratoga/Whidbey Basin  
  "Saratoga/Whidbey Basin", 6, "South Hat Island", 29,
  "Saratoga/Whidbey Basin", 6, "Lowell Point", 31,

# 7. Eastern Strait of Juan de Fuca  
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
  
# 8. Western Strait of Juan de Fuca
  "Western Strait of Juan de Fuca", 8, "Clallam Bay West", 40,
  "Western Strait of Juan de Fuca", 8, "Sekiu Point", 41,
  
# 9. San Juan Islands
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

# 10. North Puget Sound  
  "North Puget Sound", 10, "Burrows Lighthouse", 48,
  "North Puget Sound", 10, "Derelict Conveyor", 57,
  "North Puget Sound", 10, "Point Whitehorn", 58
)
  
df_upc <- df_upc %>%
  left_join(basin_lookup, by = "site")

## key
create.key <- function(df_upc){
  df_upc$key <- paste(df_upc$basin_id, df_upc$site_id, df_upc$transect,
                      substr(as.character(df_upc$year), 3, 4), sep = "_")
  front.ofthe.line(df_upc)
}

df_upc <- create.key(df_upc)

## add depth_zone column
df_upc <- df_upc %>%
  mutate(
    depth_zone = case_when(
      transect %in% 1:3 ~ "offshore",
      transect %in% 4:6 ~ "inshore",
      TRUE ~ NA_character_
    )
  )

## scale adjustment
df_upc <- df_upc %>%
  mutate(
    adjust = total_amount != 30,
    amount = if_else(adjust, as.integer(amount / total_amount * 30), amount),
    total_amount = if_else(adjust, 30, total_amount)
  ) %>%
  select(-adjust)

df_upc$percentage <- round(df_upc$percentage / 100, digits = 2)

## clean strings
df_upc <- df_upc %>%
  mutate(across(where(is.character), ~ gsub(" ", "_", .x)))

names(df_upc) <- gsub(" ", "_", names(df_upc))

df_upc$classcode <- tolower(df_upc$classcode)

## reorder columns
df_upc <- df_upc[, c(1,14:15,2,16,7,17,3:6,8:13)]

## pivot
df_upc <- df_upc %>%
  mutate(classcode = recode(classcode,
                            "superlayer_brown_algae" = "sp_brown_algae",
                            "superlayer_red_algae" = "sp_red_algae",
                            "superlayer_green_algae" = "sp_green_algae",
                            "superlayer_mobile_invertebrates" = "sp_mobile_inverts",
                            "superlayer_acid_weed" = "sp_acid_weed",
                            
                            "clay" = "sb_clay-wa",
                            "sand" = "sb_sand",
                            "pebble_(0.5-5cm-wa)" = "sb_pebble-wa",
                            "cobble" = "sb_cobble",
                            "cobble_(5-15cm-wa)" = "sb_cobble-wa",
                            "rock_(15-25cm-wa)" = "sb_rock",
                            "boulder" = "sb_boulder",
                            "small_boulder_(25-50cm-wa)" = "sb_s_boulder-wa",
                            "large_boulder_(50cm-1m-wa)" = "sb_l_boulder-wa",
                            "reef" = "sb_reef",
                            "shell_hash" = "sb_shell_hash-wa",
                            "other" = "sb_other",
                            
                            "none" = "cv_none",
                            "kelp_holdfast" = "cv_holdfast",
                            "other_brown_algae" = "cv_brown_algae",
                            "acid_weed" = "cv_acid_weed",
                            "green_algae" = "cv_green_algae",
                            "red_algae" = "cv_red_algae",
                            "encrusting_red_algae" = "cv_encrusting",
                            "articulated_coralline" = "cv_articulated",
                            "crustose_coralline" = "cv_crustose",
                            "sessile_invertebrates" = "cv_sessile_inverts",
                            "seagrasses" = "cv_seagrass",
                            
                            "0-10cm" = "rf_0-10cm",
                            "10cm-1m" = "rf_10cm-1m",
                            "1m-2m" = "rf_1m-2m",
                            ">2m" = "rf_over_2m"
  ))

df_wide <- df_upc %>%
  select(key:depth_ft, classcode, percentage) %>%
  pivot_wider(
    names_from = classcode,
    values_from = percentage,
    values_fill = 0
  )

ordered_class_columns <- c(
  "sp_brown_algae",
  "sp_red_algae",
  "sp_green_algae",
  "sp_mobile_inverts",
  "sp_acid_weed",
  
  "sb_clay-wa",
  "sb_sand",
  "sb_pebble-wa",
  "sb_cobble",
  "sb_cobble-wa",
  "sb_rock",
  "sb_boulder",
  "sb_s_boulder-wa",
  "sb_l_boulder-wa",
  "sb_reef",
  "sb_shell_hash-wa",
  "sb_other",
  
  "cv_none",
  "cv_holdfast",
  "cv_brown_algae",
  "cv_acid_weed",
  "cv_green_algae",
  "cv_red_algae",
  "cv_encrusting",
  "cv_articulated",
  "cv_crustose",
  "cv_sessile_inverts",
  "cv_seagrass",
  
  "rf_0-10cm",
  "rf_10cm-1m",
  "rf_1m-2m",
  "rf_over_2m"
)

df_wide <- df_wide %>%
  select(
    key,
    basin,
    basin_id,
    site,
    site_id,
    transect,
    depth_zone,
    latitude,
    longitude,
    year,
    date,
    depth_ft,
    all_of(ordered_class_columns)
  )

df_wide <- df_wide |> group_by(site_id)
df_wide <- arrange(df_wide, .by_group = TRUE)

write.csv(df_wide, file.path(results, "reef_check_upc_cleaned.csv"), row.names = FALSE)