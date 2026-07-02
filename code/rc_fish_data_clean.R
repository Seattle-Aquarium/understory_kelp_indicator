## start up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## clear working history
rm(list=ls())

## add libraries
library(tidyverse)
library(tibble)
library(dplyr)
library(tidyr)

## set working directory to home folder
getwd()
setwd("../")

## relative file paths
data <- "data/Reef_Check"
code <- "code"
figs <- "figs"
results <- "results"

## invoke relative file path 
df_fish <- read.csv(file.path(data, "Fish_Washington_raw.csv"))

## function to place the last column [, ncol] in the first column position i.e. [, 1]
## this function is invoked to place the "key" column created above in the first column position 
front.ofthe.line <- function(df_fish){
  num.col <- ncol(df_fish)
  df_fish <- df_fish[c(num.col, 1:(num.col - 1))]
  return(df_fish)
}

## set column headers to lowercase
names(df_fish) <- base::tolower(names(df_fish))

## drop Canadian sites
df_fish <- subset(df_fish, !(site %in% c("7751 Reef", "Ogden Point", 
                                             "Spring Bay", "Cates Park", 
                                             "Lions Gate", "Ferguson Point",
                                             "South Bowyer Island","Christie Islet",
                                             "Whytecliff Park")))

###
## legend for basin-site
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

df_fish <- df_fish %>%
  left_join(basin_lookup, by = "site")

## create a unique basin-site-transect key (for data with multiple transects)
create.key <- function(df_fish){
  df_fish$key <- paste(
    df_fish$basin_id,
    df_fish$site_id,
    df_fish$transect,
    substr(as.character(df_fish$year), 3, 4),
    sep = "_")
  df_fish <- front.ofthe.line(df_fish)
  return(df_fish)
}

df_fish <- create.key(df_fish)

## add depth_zone column
df_fish <- df_fish %>%
  mutate(
    depth_zone = case_when(
      transect %in% 1:3 ~ "offshore",
      transect %in% 4:6 ~ "inshore",
      TRUE ~ NA_character_
    )
  )

# reorder columns
df_fish <- df_fish %>%
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
    classcode,
    amount,
    distance
  )

## extrapolates data to distance of 30.0 m
df_fish <- df_fish %>%
  mutate(
    adjust = distance < 30,
    amount = as.integer(if_else(adjust, amount / distance * 30, amount)),
    distance = if_else(adjust, 30, distance)
  ) %>%
  select(-adjust)

df_fish <- df_fish %>%
  mutate(across(where(is.character), ~ gsub(" ", "_", .x)))

names(df_fish) <- gsub(" ", "_", names(df_fish))

df_fish$classcode <- tolower(df_fish$classcode)

df_fish <- df_fish %>%
  mutate(classcode = recode(classcode,
                            "dawson's_sun_star" = "dawsons_sun_star",
                            "green/pallid_urchin" = "green_pallid_urchin",
                            "kelp_crab_(juvenile)" = "kelp_crab_juv"))

## sum individual counts
df_fish <- df_fish %>%
  group_by(
    key, basin, basin_id, site, site_id,
    transect, depth_zone,
    latitude, longitude,
    year, date, depth_ft,
    classcode
  ) %>%
  summarise(
    amount = sum(amount),
    .groups = "drop"
  )

## corrects cell ordering
df_fish <- df_fish %>%
  arrange(site_id, desc(year), transect)

## pivots table to wide form
df_wide <- df_fish %>%
  select(key:depth_ft, classcode, amount) %>%
  pivot_wider(
    names_from = classcode,
    values_from = amount,
    values_fill = 0
  )
###

df_wide <- df_wide |> group_by(site_id)
df_wide <- arrange(df_wide, .by_group = TRUE)

write.csv(df_wide,"results/reef_check_fish_cleaned.csv", row.names = FALSE)