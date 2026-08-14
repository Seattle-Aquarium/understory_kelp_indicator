## Reef Check combined cleaning script
## Combines Algae, UPC, Invertebrates, Fish
## Output: results/reef_check_cleaned.csv

rm(list=ls())
library(tidyverse)

## set working directory to home folder
setwd("../")

data <- "data/Reef_Check"
results <- "results"

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
  "Central Puget Sound",2,"Elliot Bay Marina",19,
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

pivot_reef_check <- function(df){
  wide <- df %>%
    select(key,classcode,amount) %>%
    pivot_wider(
      names_from=classcode,
      values_from=amount,
      values_fn=sum,
      values_fill=0
    )
  wide %>% select(key,all_of(sort(setdiff(names(wide),"key"))))
}

## Assigns a final, unique survey key. Some sites are surveyed more than
## once in the same year (e.g. Elliot Bay Marina in 2025); those share the
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

df_algae <- read.csv(file.path(data,"Algae_Washington_raw.csv"))
df_upc <- read.csv(file.path(data,"UPC_Washington_raw.csv"))
df_invert <- read.csv(file.path(data,"Invert_Washington_raw.csv"))
df_fish <- read.csv(file.path(data,"Fish_Washington_raw.csv"))

## ============================================================
## Shared cleaning
## ============================================================

canadian_sites <- c(
  "7751 Reef","Ogden Point","Spring Bay",
  "Cates Park","Lions Gate","Ferguson Point",
  "South Bowyer Island","Christie Islet",
  "Whytecliff Park"
)

clean_dataset <- function(df){
  df %>%
    clean_columns() %>%
    filter(!site %in% canadian_sites) %>%
    parse_dates() %>%
    add_metadata() %>%
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

df_algae <- df_algae %>%
  mutate(
    adjust=distance < 30,
    amount=as.integer(if_else(adjust,amount/distance*30,amount)),
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

df_upc <- df_upc %>%
  mutate(
    adjust=total_amount != 30,
    amount=if_else(
      adjust,
      as.integer(amount/total_amount*30),
      amount
    ),
    total_amount=if_else(adjust,30,total_amount),
    percentage=round(percentage/100,digits=2)
  ) %>%
  select(-adjust) %>%
  mutate(
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
      "seagrasses"="cv_seagrass",
      "0-10cm"="rf_0-10cm",
      "10cm-1m"="rf_10cm-1m",
      "1m-2m"="rf_1m-2m",
      ">2m"="rf_over_2m"
    ),
    amount=percentage
  )

## ============================================================
## INVERTEBRATES
## ============================================================

df_invert <- df_invert %>%
  mutate(
    adjust=distance < 30,
    amount=as.integer(if_else(adjust,amount/distance*30,amount))
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

df_algae_wide <- pivot_reef_check(df_algae)
df_upc_wide <- pivot_reef_check(df_upc)
df_invert_wide <- pivot_reef_check(df_invert)
df_fish_wide <- pivot_reef_check(df_fish)

## ============================================================
## Combine datasets
## ============================================================
## all_metadata already carries one row per unique survey with all
## metadata columns, so joining the (metadata-free) wide species tables
## onto it by "key" needs no suffixing/restoring of duplicate columns.
## Species columns are NA where that dataset wasn't surveyed on a given
## dive, and 0 where it was surveyed but nothing of that class was found.

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

write.csv(
  reef_check_cleaned,
  file.path(results,"reef_check_cleaned.csv"),
  row.names=FALSE
)
