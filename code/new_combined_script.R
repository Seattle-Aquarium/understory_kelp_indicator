## Reef Check combined cleaning script
## Combines:
##   - Algae
##   - UPC
##   - Invertebrates
##   - Fish
##
## Output:
##   results/reef_check_cleaned.csv


## ============================================================
## Start up
## ============================================================

rm(list = ls())
library(tidyverse)


## ============================================================
## Paths
## ============================================================

data <- "data/Reef_Check"
results <- "results"

## ============================================================
## Basin lookup
##
## Paste remaining basin rows where indicated
## ============================================================

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


## ============================================================
## Shared functions
## ============================================================


## Move key column to front (retained from original scripts)
front.ofthe.line <- function(df){
  df[c(ncol(df), 1:(ncol(df)-1))]
}


## Standardize names and string values
clean_columns <- function(df){
  
  names(df) <- tolower(names(df))
  
  names(df) <- gsub(
    " ",
    "_",
    names(df)
  )
  
  df
}



clean_values <- function(df){
  
  df %>%
    mutate(
      across(
        where(is.character),
        ~ gsub(" ", "_", .x)
      )
    )
  
}



## Add basin information, key, and depth zone
add_metadata <- function(df){
  
  df %>%
    
    left_join(
      basin_lookup,
      by = "site"
    ) %>%
    
    filter(!is.na(basin_id)) %>%
    
    filter(transect <= 6) %>%
    
    mutate(
      
      key = paste(
        basin_id,
        site_id,
        transect,
        substr(as.character(year), 3, 4),
        sep = "_"
      ),
      
      depth_zone = case_when(
        
        transect %in% 1:3 ~ "offshore",
        
        transect %in% 4:6 ~ "inshore",
        
        TRUE ~ NA_character_
        
      )
      
    )
  
}

make_unique_keys <- function(df){
  
  df %>%
    group_by(key) %>%
    arrange(
      key,
      date,
      depth_ft,
      .by_group = TRUE
    ) %>%
    mutate(
      key = if (n() > 1) {
        paste0(
          key,
          "_",
          LETTERS[row_number()]
        )
      } else {
        key
      }
    ) %>%
    ungroup()
  
}

## Metadata columns shared by every dataset
metadata_columns <- c(
  "key",
  "basin",
  "basin_id",
  "site",
  "site_id",
  "transect",
  "depth_zone",
  "latitude",
  "longitude",
  "year",
  "date",
  "depth_ft"
)



## Generic wide conversion
pivot_reef_check <- function(df){
  
  df %>%
    
    select(
      all_of(metadata_columns),
      classcode,
      amount
    ) %>%
    
    pivot_wider(
        names_from = classcode,
        values_from = amount,
        values_fn = sum
    )
  
}

## ============================================================
## Read raw datasets
## ============================================================

df_algae <- read.csv(file.path(data, "Algae_Washington_raw.csv"))

df_upc <- read.csv(file.path(data, "UPC_Washington_raw.csv"))

df_invert <- read.csv(file.path(data, "Invert_Washington_raw.csv"))

df_fish <- read.csv(file.path(data, "Fish_Washington_raw.csv"))

## ============================================================
## Shared cleaning
## ============================================================
df_algae <- df_algae %>%
  clean_columns() %>%
  add_metadata() %>%
  clean_values()

df_upc <- df_upc %>%
  clean_columns() %>%
  add_metadata() %>%
  clean_values()

df_invert <- df_invert %>%
  clean_columns() %>%
  add_metadata() %>%
  clean_values()

df_fish <- df_fish %>%
  clean_columns() %>%
  add_metadata() %>%
  clean_values()

## ============================================================
## ALGAE
## ============================================================


## Ensure column order and scale distance to 30 m

df_algae <- df_algae %>%
  
  mutate(
    
    adjust = distance < 30,
    
    amount = if_else(
      adjust,
      as.integer(amount / distance * 30),
      amount
    ),
    
    distance = if_else(
      adjust,
      30,
      distance
    )
    
  ) %>%
  
  select(-adjust)



## ------------------------------------------------------------
## Create Giant Kelp and Feather Boa Kelp summary columns
## ------------------------------------------------------------

df_algae_summary <- df_algae %>%
  
  group_by(
    across(all_of(metadata_columns))
  ) %>%
  
  summarise(
    
    giant_kelp_n =
      sum(
        amount[classcode == "Giant_Kelp"],
        na.rm = TRUE
      ),
    
    giant_kelp_stipe =
      sum(
        stipes[classcode == "Giant_Kelp"],
        na.rm = TRUE
      ),
    
    feather_boa_kelp_n =
      sum(
        amount[classcode == "Feather_Boa_Kelp"],
        na.rm = TRUE
      ),
    
    feather_boa_kelp_stipe =
      sum(
        stipes[classcode == "Feather_Boa_Kelp"],
        na.rm = TRUE
      ),
    
    .groups = "drop"
    
  ) %>%
  
  pivot_longer(
    
    cols = -all_of(metadata_columns),
    names_to = "classcode",
    values_to = "amount"
  )

## Remove original kelp records and append new summary records

df_algae <- bind_rows(
  df_algae %>%
    filter(
      !classcode %in%
        c(
          "Giant_Kelp",
          "Feather_Boa_Kelp"
        )
    ) %>%
    select(-stipes),
  df_algae_summary
)

## Remove stipes permanently

df_algae <- df_algae %>%
  
  select(
    -any_of("stipes")
  )

## Convert to wide format

df_algae_wide <- df_algae %>%
  mutate(
    classcode = tolower(classcode)
  ) %>%
  pivot_reef_check() %>%
  make_unique_keys()

## ============================================================
## UPC
## ============================================================


## Scale UPC observations to 30 individuals

df_upc <- df_upc %>%
  
  mutate(
    adjust = total_amount != 30,
    amount = if_else(
      adjust,
      as.integer(
        amount / total_amount * 30
      ),
      amount
    ),
    total_amount = if_else(
      adjust,
      30,
      total_amount
    ),
    percentage = round(
      percentage / 100,
      digits = 2
    )
  ) %>%
  select(
    -adjust
  )

## UPC classcode standardization

df_upc <- df_upc %>%
  
  mutate(
    
    classcode = recode(
      
      classcode,
      
      "superlayer_brown_algae" =
        "sp_brown_algae",
      
      "superlayer_red_algae" =
        "sp_red_algae",
      
      "superlayer_green_algae" =
        "sp_green_algae",
      
      "superlayer_mobile_invertebrates" =
        "sp_mobile_inverts",
      
      "superlayer_acid_weed" =
        "sp_acid_weed",
      
      
      "clay" =
        "sb_clay-wa",
      
      "sand" =
        "sb_sand",
      
      "pebble_(0.5-5cm-wa)" =
        "sb_pebble-wa",
      
      "cobble" =
        "sb_cobble",
      
      "cobble_(5-15cm-wa)" =
        "sb_cobble-wa",
      
      "rock_(15-25cm-wa)" =
        "sb_rock",
      
      "boulder" =
        "sb_boulder",
      
      "small_boulder_(25-50cm-wa)" =
        "sb_s_boulder-wa",
      
      "large_boulder_(50cm-1m-wa)" =
        "sb_l_boulder-wa",
      
      "reef" =
        "sb_reef",
      
      "shell_hash" =
        "sb_shell_hash-wa",
      
      "other" =
        "sb_other",
      
      
      "none" =
        "cv_none",
      
      "kelp_holdfast" =
        "cv_holdfast",
      
      "other_brown_algae" =
        "cv_brown_algae",
      
      "acid_weed" =
        "cv_acid_weed",
      
      "green_algae" =
        "cv_green_algae",
      
      "red_algae" =
        "cv_red_algae",
      
      "encrusting_red_algae" =
        "cv_encrusting",
      
      "articulated_coralline" =
        "cv_articulated",
      
      "crustose_coralline" =
        "cv_crustose",
      
      "sessile_invertebrates" =
        "cv_sessile_inverts",
      
      "seagrasses" =
        "cv_seagrass",
      
      
      "0-10cm" =
        "rf_0-10cm",
      
      "10cm-1m" =
        "rf_10cm-1m",
      
      "1m-2m" =
        "rf_1m-2m",
      
      ">2m" =
        "rf_over_2m"
      
    )
    
  )

## Convert percentage to wide table

df_upc_wide <- df_upc %>%
  
  mutate(
    classcode = tolower(classcode)
  ) %>%
  
  select(
    all_of(metadata_columns),
    classcode,
    amount = percentage
  ) %>%
  
  pivot_reef_check()


df_upc_wide <- df_upc %>%
  mutate(
    classcode = tolower(classcode)
  ) %>%
  select(
    all_of(metadata_columns),
    classcode,
    amount = percentage
  ) %>%
  pivot_reef_check() %>%
  make_unique_keys()



## ============================================================
## INVERTEBRATES
## ============================================================


## Scale counts to 30 m transect distance

df_invert <- df_invert %>%
  
  mutate(
    
    adjust = distance < 30,
    
    amount = if_else(
      adjust,
      as.integer(amount / distance * 30),
      amount
    )
    
  ) %>%
  
  select(
    -adjust
  )

## Standardize classcodes

df_invert <- df_invert %>%
  
  mutate(
    
    classcode = recode(
      
      classcode,
      
      "dawson's_sun_star" =
        "dawsons_sun_star",
      
      "green/pallid_urchin" =
        "green_pallid_urchin",
      
      "kelp_crab_(juvenile)" =
        "kelp_crab_juv"
      
    ), classcode = tolower(classcode))

## Combine duplicate species observations

df_invert <- df_invert %>%
  
  group_by(
    
    across(
      c(
        all_of(metadata_columns),
        classcode
      )
    )
    
  ) %>%
  
  summarise(
    
    amount = sum(amount, na.rm = TRUE),
    
    .groups = "drop"
    
  )

## Convert to wide format

df_invert_wide <- df_invert %>%
  pivot_reef_check() %>%
  make_unique_keys()

## ============================================================
## FISH
## ============================================================

## Standardize fish classcodes

df_fish <- df_fish %>%
  
  mutate(
    
    classcode = recode(
      
      classcode,
      
      "black/yellowtail_yoy" =
        "black_yellowtail_yoy",
      
      "blue/deacon_yoy" =
        "blue_deacon_yoy",
      
      "brown/copper/quillback_yoy" =
        "brown_copper_quillback_yoy"
      
    ),
    
    classcode = tolower(classcode)
    
  )

## Combine duplicate species observations

df_fish <- df_fish %>%
  
  group_by(
    
    across(
      c(
        all_of(metadata_columns),
        classcode
      )
    )
  ) %>%
  
  summarise(amount = sum(amount, na.rm = TRUE), .groups = "drop")

## Convert to wide format

df_fish_wide <- df_fish %>%
  pivot_reef_check() %>%
  make_unique_keys()

## ============================================================
## Combine datasets
## ============================================================

list(
  algae = df_algae_wide,
  upc = df_upc_wide,
  invert = df_invert_wide,
  fish = df_fish_wide
) %>%
  imap(~ .x %>%
         count(key) %>%
         filter(n > 1) %>%
         mutate(dataset=.y))


## Join all datasets by shared metadata.
## Full joins preserve transects that occur in only one dataset.

reef_check_cleaned <- df_algae_wide %>%
  
  full_join(
    df_upc_wide,
    by = metadata_columns
  ) %>%
  
  full_join(
    df_invert_wide,
    by = metadata_columns
  ) %>%
  
  full_join(
    df_fish_wide,
    by = metadata_columns
  )

## ============================================================
## Column ordering
## ============================================================

metadata_columns <- c(
  "key",
  "basin",
  "basin_id",
  "site",
  "site_id",
  "transect",
  "depth_zone",
  "latitude",
  "longitude",
  "year",
  "date",
  "depth_ft"
)

## Get columns by source, but only retain those
## that survived the joins

algae_columns <- intersect(
  setdiff(names(df_algae_wide), metadata_columns),
  names(reef_check_cleaned)
)

upc_columns <- intersect(
  setdiff(names(df_upc_wide), metadata_columns),
  names(reef_check_cleaned)
)

invert_columns <- intersect(
  setdiff(names(df_invert_wide), metadata_columns),
  names(reef_check_cleaned)
)

fish_columns <- intersect(
  setdiff(names(df_fish_wide), metadata_columns),
  names(reef_check_cleaned)
)

## Reorder:
## metadata → algae → UPC → invertebrates → fish

reef_check_cleaned <- reef_check_cleaned %>%
  
  select(
    all_of(metadata_columns),
    all_of(algae_columns),
    all_of(upc_columns),
    all_of(invert_columns),
    all_of(fish_columns),
    everything()
  )

## ============================================================
## Sort rows
## ============================================================

reef_check_cleaned <- reef_check_cleaned %>%
  
  arrange(
    site_id,
    desc(year),
    transect
  )

## ============================================================
## Export
## ============================================================

write.csv(
  reef_check_cleaned,
  file.path(results, "reef_check_cleaned.csv"),row.names = FALSE)