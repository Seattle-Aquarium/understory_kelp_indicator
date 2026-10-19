## Reef Check combined cleaning script
## Combines Algae, UPC, Invertebrates, Fish
## Output: results/reef_check_cleaned.csv

rm(list=ls())
## The three packages this script uses, rather than the whole tidyverse: fewer
## packages for the app's private R environment to install, and no attach
## messages cluttering the log the app shows.
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
})

## set working directory to home folder
setwd("../")

data <- "data/Reef_Check"
results <- "results"

## ---- FILE LOCATIONS --------------------------------------------------------
## Run this script on its own and it reads and writes the repository's own
## folders, exactly as it always has. The Shiny app in gui/ sets the environment
## variables below first, so a person can point it at files kept somewhere else
## without editing anything here.
##
## Environment variables rather than options() because the app starts this
## script as its own R process -- options do not cross a process boundary, and a
## separate process is what keeps the line-one rm(list=ls()) and the setwd()
## above from reaching into the running app.
from_env <- function(var, default) {
  value <- Sys.getenv(var, unset = "")
  if (nzchar(value)) value else default
}

algae_csv   <- from_env("UKELP_ALGAE_CSV",   file.path(data, "Algae_Washington_raw.csv"))
upc_csv     <- from_env("UKELP_UPC_CSV",     file.path(data, "UPC_Washington_raw.csv"))
invert_csv  <- from_env("UKELP_INVERT_CSV",  file.path(data, "Invert_Washington_raw.csv"))
fish_csv    <- from_env("UKELP_FISH_CSV",    file.path(data, "Fish_Washington_raw.csv"))
cleaned_csv <- from_env("UKELP_CLEANED_CSV", file.path(results, "reef_check_cleaned.csv"))

basin_lookup <- tribble(
  ~basin, ~basin_id, ~site, ~site_id,
  "South Puget Sound",1,"Devils Head",1,
  "South Puget Sound",1,"Ketron Island",2,
  "South Puget Sound",1,"Squaxin Island",3,
  "South Puget Sound",1,"Fox Island East Wall",4,
  "South Puget Sound",1,"Titlow Beach",5,
  "Central Puget Sound",2,"Salmon Beach",6,
  "Central Puget Sound",2,"Owen Beach",7,
  "Central Puget Sound",2,"Point Dalco",8,
  "Central Puget Sound",2,"Saltwater State Park",9,
  "Central Puget Sound",2,"Glen Acres",11,
  "Central Puget Sound",2,"Point Vashon",12,
  "Central Puget Sound",2,"Blake Island South",13,
  "Central Puget Sound",2,"Lincoln Park",14,
  "Central Puget Sound",2,"Seattle Waterfront",15,
  "Central Puget Sound",2,"Wing Point",16,
  "Central Puget Sound",2,"Sirens of Spring",17,
  "Central Puget Sound",2,"Grain Terminal",18,
  "Central Puget Sound",2,"Elliott Bay Marina",19,
  "Central Puget Sound",2,"Magnolia",20,
  "Central Puget Sound",2,"Jefferson Head",23,
  "Central Puget Sound",2,"Edmonds Shell Creek",24,
  "Hood Canal",3,"Sund Rock",10,
  "Hood Canal",3,"Goby Gardens",21,
  "Hood Canal",3,"Pulali Point",22,
  "Northern Coast",4,"Teahwhit Head",25,
  "Northern Coast",4,"Rock 305",28,
  "Admiralty Inlet",5,"Possession Point",26,
  "Admiralty Inlet",5,"Foulweather Bluff",27,
  "Saratoga/Whidbey Basin",6,"South Hat Island",29,
  "Saratoga/Whidbey Basin",6,"Lowell Point",31,
  "Eastern Strait of Juan de Fuca",7,"Green Point",30,
  "Eastern Strait of Juan de Fuca",7,"Dallas Banks",32,
  "Eastern Strait of Juan de Fuca",7,"McCurdy Point",33,
  "Eastern Strait of Juan de Fuca",7,"Lower Elwha",34,
  "Eastern Strait of Juan de Fuca",7,"Freshwater Bay",35,
  "Eastern Strait of Juan de Fuca",7,"North Beach",36,
  "Eastern Strait of Juan de Fuca",7,"Tongue Point",37,
  "Eastern Strait of Juan de Fuca",7,"Ebeys Landing",38,
  "Eastern Strait of Juan de Fuca",7,"Partridge Point",39,
  "Eastern Strait of Juan de Fuca",7,"Smith Island",43,
  "Eastern Strait of Juan de Fuca",7,"Rosario Head",44,
  "Western Strait of Juan de Fuca",8,"Clallam Bay West",40,
  "Western Strait of Juan de Fuca",8,"Sekiu Point",41,
  "San Juan Islands",9,"Watmough Bay",45,
  "San Juan Islands",9,"Cattle Point",46,
  "San Juan Islands",9,"Eagle Cove",47,
  "San Juan Islands",9,"Deadmans Bay",49,
  "San Juan Islands",9,"Reef Point",50,
  "San Juan Islands",9,"Smallpox Bay",51,
  "San Juan Islands",9,"South Shaw Island",52,
  "San Juan Islands",9,"Point Caution",53,
  "San Juan Islands",9,"Reuben Tarte",54,
  "San Juan Islands",9,"Lawrence Point",55,
  "San Juan Islands",9,"Satellite Island",56,
  "North Puget Sound",10,"Burrows Lighthouse",48,
  "North Puget Sound",10,"Derelict Conveyor",57,
  "North Puget Sound",10,"Point Whitehorn",58
)

metadata_columns <- c(
  "key","basin","basin_id","site","site_id",
  "transect","depth_zone","latitude","longitude",
  "year","date","depth_ft"
)

## columns used to identify the same real-world dive across datasets,
## once "date" has been normalized to a common Date type below
survey_id_columns <- c("basin_id","site_id","transect","year","date","depth_ft")

clean_columns <- function(df){
  names(df) <- tolower(gsub(" ","_",names(df)))
  df
}

## Algae uses M/D/YYYY dates; UPC/Invert/Fish use YYYY-MM-DD. Both are
## internally consistent (one format per file), so tryFormats can pick the
## right one per dataset. Normalizing here lets dives be matched across
## datasets by date, and write.csv() will emit the result as YYYY-MM-DD.
parse_dates <- function(df){
  df %>% mutate(date=as.Date(date,tryFormats=c("%Y-%m-%d","%m/%d/%Y")))
}

clean_values <- function(df){
  df %>% mutate(across(where(is.character),~gsub(" ","_",.x)))
}

add_metadata <- function(df){
  df %>%
    left_join(basin_lookup,by="site") %>%
    filter(!is.na(basin_id)) %>%
    mutate(
      key=paste(
        basin_id,site_id,transect,
        substr(as.character(year),3,4),
        sep="_"
      ),
      depth_zone=case_when(
        transect %in% 1:3 ~ "offshore",
        transect %in% 4:6 ~ "inshore",
        TRUE ~ NA_character_
      )
    )
}

## A category a survey's datasheet did not carry was never looked for, which is
## not the same as looked for and not found. Within each survey (one site on one
## date) every transect gets an explicit 0 for every category that survey
## recorded anywhere; a category the survey never recorded is left out here and
## so comes through the pivot below as NA.
##
## The datasheets changed over the years. UPC gained the superlayer in 2022,
## Clay in 2023, and Acid Weed (as both a cover and a superlayer class) in 2024;
## the invert and fish sheets gained species in 2022-2023. The algae sheet never
## changed. Filling those gaps with 0, as this script used to, made datasheet
## changes look like declines and first appearances.
complete_on_sheet <- function(df){
  df %>%
    select(key,site_id,date,classcode,amount) %>%
    group_by(site_id,date) %>%
    complete(key,classcode,fill=list(amount=0)) %>%
    ungroup()
}

## No values_fill: complete_on_sheet() has already written every real 0, so a
## cell still empty after the pivot is a category that was not on the sheet.
pivot_reef_check <- function(df){
  wide <- df %>%
    select(key,classcode,amount) %>%
    pivot_wider(
      names_from=classcode,
      values_from=amount,
      values_fn=sum
    )
  wide %>% select(key,all_of(sort(setdiff(names(wide),"key"))))
}

## Assigns a final, unique survey key. Some sites are surveyed more than
## once in the same year (e.g. Elliott Bay Marina in 2025); those share the
## same base "key" (basin_site_transect_yr) but have distinct dates. This
## must run once on the combined set of survey instances across all four
## datasets (see all_metadata below) so that every dataset ends up using
## the *same* suffixed key for the same real-world dive.
make_unique_keys <- function(df){

  df %>%
    group_by(
      basin_id,
      site_id,
      transect,
      year
    ) %>%
    arrange(
      date,
      depth_ft,
      .by_group=TRUE
    ) %>%
    mutate(
      survey_number=row_number(),
      key=case_when(
        n()>1 & survey_number==1 ~ paste0(key,"_A"),
        n()>1 & survey_number==2 ~ paste0(key,"_B"),
        n()>1 & survey_number==3 ~ paste0(key,"_C"),
        TRUE ~ key
      )
    ) %>%
    select(-survey_number) %>%
    ungroup()
}

## ============================================================
## Read raw datasets
## ============================================================

df_algae <- read.csv(algae_csv)
df_upc <- read.csv(upc_csv)
df_invert <- read.csv(invert_csv)
df_fish <- read.csv(fish_csv)

## ============================================================
## Shared cleaning
## ============================================================

canadian_sites <- c(
  "7751 Reef","Ogden Point","Spring Bay",
  "Cates Park","Lions Gate","Ferguson Point",
  "South Bowyer Island","Christie Islet",
  "Whytecliff Park"
)

## Site names misspelled in the raw Reef Check exports. basin_lookup and every
## downstream output carry the corrected spelling, so this has to run BEFORE
## add_metadata() joins on "site" -- correcting the lookup alone would make that
## join miss and add_metadata()'s filter(!is.na(basin_id)) would then drop the
## site from all four datasets silently. Safe to leave in place once the raw data
## is corrected upstream: recode() passes through any name it doesn't match.
site_name_corrections <- c("Elliot Bay Marina" = "Elliott Bay Marina")

recode_sites <- function(df){
  df %>% mutate(site = recode(site, !!!site_name_corrections))
}

## Elliott Bay Marina and Sirens of Spring were each dived twice in 2025, once in
## January and again in August. Every scale downstream assumes one survey per
## site per year -- pooled, these two site-years average a winter and a
## late-summer dive together on 12 transects instead of 6 -- so the winter dives
## are dropped. Runs after parse_dates() so the match is on real Dates.
winter_surveys <- tribble(
  ~site,                ~date,
  "Elliott Bay Marina", as.Date("2025-01-28"),
  "Sirens of Spring",   as.Date("2025-01-27")
)

drop_winter_surveys <- function(df){
  df %>% anti_join(winter_surveys, by = c("site","date"))
}

## ---- EXCLUDED BASINS -------------------------------------------------------
## Northern Coast, Western Strait of Juan de Fuca and Eastern Strait of Juan de
## Fuca are held out of every output in this repo. TO BRING THEM BACK: delete
## this block and the drop_excluded_basins() call in clean_dataset() below --
## nothing else depends on the exclusion, and the raw files in data/Reef_Check
## still contain every basin. Search the repo for "EXCLUDED BASINS" to find all
## of the places this is applied.
##
## Runs after add_metadata(), which is what attaches "basin", and before
## clean_values(), so the names still carry spaces at this point.
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

drop_excluded_basins <- function(df){
  df %>% filter(!gsub("_", " ", basin) %in% excluded_basins)
}

clean_dataset <- function(df){
  df %>%
    clean_columns() %>%
    filter(!site %in% canadian_sites) %>%
    recode_sites() %>%
    parse_dates() %>%
    drop_winter_surveys() %>%
    add_metadata() %>%
    drop_excluded_basins() %>%
    clean_values()
}

df_algae <- clean_dataset(df_algae)
df_upc <- clean_dataset(df_upc)
df_invert <- clean_dataset(df_invert)
df_fish <- clean_dataset(df_fish)

## ============================================================
## Correct algae's site coordinates
## ============================================================
## latitude/longitude are recorded per-row in each raw file rather than
## looked up from a fixed site table. UPC/Invert/Fish agree exactly on a
## single lat/long per site, but Algae disagrees for some sites (e.g.
## Tongue Point: Algae has 48.166808/-123.702363 for 2023-2025, the other
## three have 48.256822/-124.275318) -- confirmed as an error in the
## Algae raw data. Every dataset is normalized to the UPC/Invert/Fish
## coordinates so all four datasets refer to the same site location.

site_coords <- bind_rows(
  df_upc %>% select(site_id,latitude,longitude),
  df_invert %>% select(site_id,latitude,longitude),
  df_fish %>% select(site_id,latitude,longitude)
) %>%
  distinct()

use_correct_coords <- function(df){
  df %>%
    select(-latitude,-longitude) %>%
    left_join(site_coords,by="site_id")
}

df_algae <- use_correct_coords(df_algae)
df_upc <- use_correct_coords(df_upc)
df_invert <- use_correct_coords(df_invert)
df_fish <- use_correct_coords(df_fish)

## ============================================================
## Correct fish depth_ft to match the other three datasets
## ============================================================
## Fish depth_ft readings can differ slightly from Algae/UPC/Invert for the
## same real-world survey (same basin_id/site_id/transect/year/date), even
## though all four datasets originate from the same dive. Left uncorrected,
## this makes the fish row of a survey look like a distinct survey when
## building the canonical key below (make_unique_keys groups on depth_ft),
## causing fish data to end up under its own suffixed key instead of
## joining onto the shared one. Algae/UPC/Invert agree exactly on depth_ft
## for a given survey, so fish is normalized to their value; fish's own
## depth_ft is kept as a fallback for surveys with no match in the other
## three datasets.

fish_survey_id_columns <- c("basin_id","site_id","transect","year","date")

correct_depth <- bind_rows(
  df_algae %>% select(all_of(fish_survey_id_columns),depth_ft),
  df_upc %>% select(all_of(fish_survey_id_columns),depth_ft),
  df_invert %>% select(all_of(fish_survey_id_columns),depth_ft)
) %>%
  distinct()

df_fish <- df_fish %>%
  rename(depth_ft_orig=depth_ft) %>%
  left_join(correct_depth,by=fish_survey_id_columns) %>%
  mutate(depth_ft=coalesce(depth_ft,depth_ft_orig)) %>%
  select(-depth_ft_orig)

## ============================================================
## ALGAE
## ============================================================

## A dense patch is sometimes counted over only part of the transect; Distance is
## how far the count ran, and the count is scaled up to the full 30 m. round()
## rather than as.integer(): as.integer() truncates, which biased every scaled
## count down, and on floating-point values such as 56/16.8*30 = 99.99999...
## it dropped a whole individual.
df_algae <- df_algae %>%
  mutate(
    adjust=distance < 30,
    amount=if_else(adjust,round(amount*30/distance),as.numeric(amount)),
    distance=if_else(adjust,30,distance)
  ) %>%
  select(-adjust)

df_algae_summary <- df_algae %>%
  group_by(across(all_of(metadata_columns))) %>%
  summarise(
    giant_kelp_n=sum(amount[classcode=="Giant_Kelp"],na.rm=TRUE),
    giant_kelp_stipe=sum(stipes[classcode=="Giant_Kelp"],na.rm=TRUE),
    feather_boa_n=sum(amount[classcode=="Feather_Boa_Kelp"],na.rm=TRUE),
    feather_boa_stipe=sum(stipes[classcode=="Feather_Boa_Kelp"],na.rm=TRUE),
    .groups="drop"
  ) %>%
  pivot_longer(
    cols=-all_of(metadata_columns),
    names_to="classcode",
    values_to="amount"
  )

df_algae <- bind_rows(
  df_algae %>%
    filter(!classcode %in%
             c("Giant_Kelp","Feather_Boa_Kelp")) %>%
    select(-any_of("stipes")),
  df_algae_summary
) %>%
  mutate(classcode=paste0("ak_",tolower(classcode)))

## ============================================================
## UPC
## ============================================================

## Each UPC value is the share of the points read along the transect that fell in
## the category. Total_Amount is not always 30 -- between 24 and 42 points were
## read on some transects -- so the share is taken over the points actually read.
## Not rounded: rounding to 3 decimals here put every later scale a little off.
df_upc <- df_upc %>%
  mutate(
    amount=amount/total_amount,
    classcode=tolower(classcode),
    classcode=recode(
      classcode,
      "superlayer_brown_algae"="sp_brown_algae",
      "superlayer_red_algae"="sp_red_algae",
      "superlayer_green_algae"="sp_green_algae",
      "superlayer_mobile_invertebrates"="sp_mobile_inverts",
      "superlayer_acid_weed"="sp_acid_weed",
      "clay"="sb_clay-wa",
      "sand"="sb_sand",
      "pebble_(0.5-5cm-wa)"="sb_pebble-wa",
      "cobble"="sb_cobble",
      "cobble_(5-15cm-wa)"="sb_cobble-wa",
      "rock_(15-25cm-wa)"="sb_rock",
      "boulder"="sb_boulder",
      "small_boulder_(25-50cm-wa)"="sb_s_boulder-wa",
      "large_boulder_(50cm-1m-wa)"="sb_l_boulder-wa",
      "reef"="sb_reef",
      "shell_hash"="sb_shell_hash-wa",
      "other"="sb_other",
      "none"="cv_none",
      "kelp_holdfast"="cv_holdfast",
      "other_brown_algae"="cv_brown_algae",
      "acid_weed"="cv_acid_weed",
      "green_algae"="cv_green_algae",
      "red_algae"="cv_red_algae",
      "encrusting_red_algae"="cv_encrusting",
      "articulated_coralline"="cv_articulated",
      "crustose_coralline"="cv_crustose",
      "sessile_invertebrates"="cv_sessile_inverts",
      "mobile_invertebrates"="cv_mobile_inverts",
      "seagrasses"="cv_seagrass",
      "0-10cm"="rf_0-10cm",
      "10cm-1m"="rf_10cm-1m",
      "1m-2m"="rf_1m-2m",
      ">2m"="rf_over_2m"
    )
  )

## A class the recode above does not know -- a new one on a future datasheet --
## would otherwise come through with no prefix, and every later script finds its
## columns by prefix, so it would vanish without a word. It is given its
## category's prefix instead, and named in the log.
upc_category_prefix <- c("upc_cover"="cv_","upc_relief"="rf_",
                         "upc_substrate"="sb_","upc_superlayer"="sp_")
unprefixed <- !grepl("^(cv|rf|sb|sp)_",df_upc$classcode)
if (any(unprefixed)) {
  message("UPC classes not in the recode list, prefixed by their category: ",
          paste(unique(df_upc$classcode[unprefixed]),collapse=", "))
  df_upc$classcode[unprefixed] <- paste0(
    upc_category_prefix[tolower(df_upc$category[unprefixed])],
    df_upc$classcode[unprefixed])
}

## ============================================================
## INVERTEBRATES
## ============================================================

## scaled to the full 30 m exactly as the algae counts are, round() and all
df_invert <- df_invert %>%
  mutate(
    adjust=distance < 30,
    amount=if_else(adjust,round(amount*30/distance),as.numeric(amount))
  ) %>%
  select(-adjust) %>%
  mutate(
    classcode=tolower(classcode),
    classcode=recode(
      classcode,
      "dawson's_sun_star"="dawsons_sun_star",
      "green/pallid_urchin"="green_pallid_urchin",
      "kelp_crab_(juvenile)"="kelp_crab_juv"
    ),
    classcode=paste0("iv_",classcode)
  ) %>%
  group_by(
    across(c(all_of(metadata_columns),classcode))
  ) %>%
  summarise(
    amount=sum(amount,na.rm=TRUE),
    .groups="drop"
  )

## ============================================================
## FISH
## ============================================================

## Fish are counted on up to 25 transects per site, but only transects 1-6 share
## their numbers -- and so their rows -- with the algae, UPC and invert
## transects. The rest are dropped here, which is right for a one-row-per-
## transect table but discards roughly two thirds of the fish counts; a
## fish-only site average would want all of them.
df_fish <- df_fish %>%
  filter(transect %in% 1:6) %>%
  mutate(
    classcode=tolower(classcode),
    classcode=recode(
      classcode,
      "black/yellowtail_yoy"="black_yellowtail_yoy",
      "blue/deacon_yoy"="blue_deacon_yoy",
      "brown/copper/quillback_yoy"="brown_copper_quillback_yoy"
    ),
    classcode=paste0("fs_",classcode)
  ) %>%
  group_by(
    across(c(all_of(metadata_columns),classcode))
  ) %>%
  summarise(
    amount=sum(amount,na.rm=TRUE),
    .groups="drop"
  )

## ============================================================
## Build one canonical, de-duplicated survey key across all datasets
## ============================================================

all_metadata <- bind_rows(
  df_algae %>% select(all_of(metadata_columns)),
  df_upc %>% select(all_of(metadata_columns)),
  df_invert %>% select(all_of(metadata_columns)),
  df_fish %>% select(all_of(metadata_columns))
) %>%
  distinct() %>%
  make_unique_keys()

key_lookup <- all_metadata %>%
  select(all_of(survey_id_columns),key)

apply_final_key <- function(df){
  df %>%
    select(-key) %>%
    left_join(key_lookup,by=survey_id_columns)
}

df_algae <- apply_final_key(df_algae)
df_upc <- apply_final_key(df_upc)
df_invert <- apply_final_key(df_invert)
df_fish <- apply_final_key(df_fish)

## every row has to have found its survey; one that didn't would be dropped by
## the joins below without a trace
for (nm in c("df_algae","df_upc","df_invert","df_fish")) {
  if (anyNA(get(nm)$key)) stop(nm, " has rows that matched no survey key")
}

df_algae_wide <- pivot_reef_check(complete_on_sheet(df_algae))
df_upc_wide <- pivot_reef_check(complete_on_sheet(df_upc))
df_invert_wide <- pivot_reef_check(complete_on_sheet(df_invert))
df_fish_wide <- pivot_reef_check(complete_on_sheet(df_fish))

## ============================================================
## Combine datasets
## ============================================================
## all_metadata already carries one row per unique survey with all
## metadata columns, so joining the (metadata-free) wide species tables
## onto it by "key" needs no suffixing/restoring of duplicate columns.
## Species columns are NA where that dataset wasn't surveyed on a given
## dive or the class wasn't on that survey's datasheet (complete_on_sheet()),
## and 0 where it was looked for and nothing of that class was found.

reef_check_cleaned <- all_metadata %>%
  left_join(df_algae_wide,by="key") %>%
  left_join(df_upc_wide,by="key") %>%
  left_join(df_invert_wide,by="key") %>%
  left_join(df_fish_wide,by="key") %>%
  arrange(
    site_id,
    desc(year),
    transect
  )

dir.create(dirname(cleaned_csv), recursive = TRUE, showWarnings = FALSE)

write.csv(
  reef_check_cleaned,
  cleaned_csv,
  row.names=FALSE
)

cat("Wrote", normalizePath(cleaned_csv, winslash = "/", mustWork = FALSE),
    "--", nrow(reef_check_cleaned), "rows,", ncol(reef_check_cleaned), "columns\n")
