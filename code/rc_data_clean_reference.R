## Reef Check shared reference data and helper functions ~~~~~~~~~~~~~~~~~~~~~~
## Sourced by rc_upc_data_clean.R, rc_fish_data_clean.R,
## rc_invert_data_clean.R, and rc_algae_data_clean.R

## Canadian sites excluded from all Reef Check datasets
canadian_sites <- c("7751 Reef", "Ogden Point", "Spring Bay", "Cates Park",
                    "Lions Gate", "Ferguson Point", "South Bowyer Island",
                    "Christie Islet", "Whytecliff Park")

## Site names misspelled in the raw Reef Check exports. basin_lookup below and
## every downstream output carry the corrected spelling, so recode.sites() has to
## run BEFORE the left_join(basin_lookup, by = "site") in each cleaning script --
## correcting the lookup alone would make that join miss, and the site would then
## drop out of every dataset carrying NA basin_id. Safe to leave in place once
## the raw data is corrected upstream: recode() passes through any name it
## doesn't match.
site_name_corrections <- c("Elliot Bay Marina" = "Elliott Bay Marina")

recode.sites <- function(df){
  df %>% mutate(site = recode(site, !!!site_name_corrections))
}

## Elliott Bay Marina and Sirens of Spring were each dived twice in 2025, once in
## January and again in August. Every scale downstream assumes one survey per
## site per year -- pooled, these two site-years average a winter and a
## late-summer dive together on 12 transects instead of 6 -- so the winter dives
## are dropped here, for every dataset.
winter_surveys <- tribble(
  ~site,                ~date,
  "Elliott Bay Marina", as.Date("2025-01-28"),
  "Sirens of Spring",   as.Date("2025-01-27")
)

## the raw files don't agree on a date format (Algae is M/D/YYYY, the rest are
## YYYY-MM-DD) and these scripts don't parse dates, so the comparison is made on
## a parsed copy that is dropped again afterwards
drop.winter.surveys <- function(df){
  df %>%
    mutate(.survey_date = as.Date(date, tryFormats = c("%Y-%m-%d", "%m/%d/%Y"))) %>%
    anti_join(winter_surveys, by = c("site", ".survey_date" = "date")) %>%
    select(-.survey_date)
}

## ---- EXCLUDED BASINS -------------------------------------------------------
## Northern Coast, Western Strait of Juan de Fuca and Eastern Strait of Juan de
## Fuca are held out of every output in this repo. TO BRING THEM BACK: delete
## this block and the drop.excluded.basins() call in each cleaning script --
## nothing else depends on the exclusion, and the raw files in data/Reef_Check
## still contain every basin. Search the repo for "EXCLUDED BASINS" to find all
## of the places this is applied.
##
## Matching normalises underscores to spaces, so the filter works both before
## and after the gsub(" ", "_") string cleaning each script applies.
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

drop.excluded.basins <- function(df){
  df %>% filter(!gsub("_", " ", basin) %in% excluded_basins)
}

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
  "Central Puget Sound", 2, "Elliott Bay Marina", 19,
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

## helper function: moves the last column (e.g. a newly created "key") to the front
front.ofthe.line <- function(df){
  df[c(ncol(df), 1:(ncol(df) - 1))]
}

## creates a unique basin-site-transect-year key and moves it to the front
create.key <- function(df){
  df$key <- paste(df$basin_id, df$site_id, df$transect,
                  substr(as.character(df$year), 3, 4), sep = "_")
  front.ofthe.line(df)
}

## classifies transects 1-3 as offshore and 4-6 as inshore
add.depth.zone <- function(df){
  df %>%
    mutate(
      depth_zone = case_when(
        transect %in% 1:3 ~ "offshore",
        transect %in% 4:6 ~ "inshore",
        TRUE ~ NA_character_
      )
    )
}
