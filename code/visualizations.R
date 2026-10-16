## start up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## clear working history
rm(list=ls())

# Make text larger across the board
# add properly formatted sci. names for each kelp

## add libraries
library(tidyverse)
library(ggpubr)
library(egg)


## set working directory to home folder
setwd("../")
getwd()

## relative file paths
data <- "data/Reef_Check"
code <- "code"
results <- "results"
figs <- "figs"

## ---- FILE LOCATIONS --------------------------------------------------------
## Run this script on its own and it reads and writes the repository's own
## folders, exactly as it always has. The Shiny app in gui/ sets the environment
## variables below first, so a person can point it at files kept somewhere else
## without editing anything here. See rc_full_data_clean.R for why environment
## variables rather than options().
from_env <- function(var, default) {
  value <- Sys.getenv(var, unset = "")
  if (nzchar(value)) value else default
}

site_csv  <- from_env("UKELP_SITE_CSV",  file.path(results, "metrics_site_scale.csv"))
basin_csv <- from_env("UKELP_BASIN_CSV", file.path(results, "metrics_basin_scale.csv"))
puget_csv <- from_env("UKELP_PUGET_CSV", file.path(results, "metrics_puget_sound_scale.csv"))
figs      <- from_env("UKELP_FIGS_DIR",  figs)

## import datasets
df_site <- read.csv(site_csv)
df_basin <- read.csv(basin_csv)
df_puget <- read.csv(puget_csv)

## ---- EXCLUDED BASINS -------------------------------------------------------
## Northern Coast, Western Strait of Juan de Fuca and Eastern Strait of Juan de
## Fuca are held out of every output in this repo. TO BRING THEM BACK: delete
## this block -- nothing else depends on the exclusion, and the raw files in
## data/Reef_Check still contain every basin. Search the repo for
## "EXCLUDED BASINS" to find all of the places this is applied.
##
## Applied here before the factor conversions below, so the dropped basins leave
## no empty facets behind. df_puget has no basin column -- it is the sound-wide
## composite, and metrics_across_scale.R has already built it without these
## three, so re-running that script is what keeps the blue sound-wide line
## consistent with the basins plotted here.
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

df_site <- df_site %>% filter(!gsub("_", " ", basin) %in% excluded_basins)
df_basin <- df_basin %>% filter(!gsub("_", " ", basin) %in% excluded_basins)

## ---- SHARED FIGURE DEFINITIONS ---------------------------------------------
## The theme, the metric groups, the label helpers and the four figure builders
## live in figure_functions.R, so that the Shiny app's preview and the files
## written here are the same figure by construction. Sourced into this script's
## own environment, so the builders see df_site / df_basin / df_puget as globals
## exactly as they did when they were defined here.
source(file.path(code, "figure_functions.R"))

## The recorded on-screen device is for working through the figures in RStudio.
## Run headless -- from Rscript, or from the Shiny app -- there is no one to page
## through it, and opening a device anyway pops a window in front of whatever
## the person is doing and leaves a stray Rplots.pdf behind. So the device, and
## the print() calls that draw to it, happen only in an interactive session;
## ggsave() writes the files either way.
draw_to_screen <- interactive()

if (draw_to_screen) {
  graphics.off()
  windows(fig_width, fig_height, record=T)
}

## print() only where there is a device worth drawing to
show <- function(p) if (draw_to_screen) print(p) else invisible(p)

## convert to factor
df_site$site <- as.factor(df_site$site)
df_site$year <- as.factor(df_site$year)
df_basin$basin <- as.factor(df_basin$basin)
df_basin$year <- as.factor(df_basin$year)
df_puget$region <- as.factor(df_puget$region)
df_puget$year <- as.factor(df_puget$year)

## ---- WHERE THE FIGURES GO --------------------------------------------------
## One folder per figure set, each holding a folder per metric group.
figs_basin         <- file.path(figs, "basin_composites")      # per basin, faceted by category
figs_basin_species <- file.path(figs, "basin_species")         # per basin, one axis
figs_species       <- file.path(figs, "species_composites")    # per category, faceted by basin
figs_puget         <- file.path(figs, "puget_sound")           # region-wide
figs_basin_sd      <- file.path(figs, "basin_composites_sd")
figs_species_sd    <- file.path(figs, "species_composites_sd")
figs_puget_sd      <- file.path(figs, "puget_sound_sd")
figs_percent       <- file.path(figs, "percent_change")       # per basin AND category

## ---- WHAT TO DRAW ----------------------------------------------------------
## Run this script on its own and it draws everything, exactly as it always has.
## The Shiny app in gui/ sets the three variables below first, so a person can
## ask for a subset without editing anything here.
##
## Each figure set is one (scale, type) pair:
##
##   scale       type        set
##   -----       ----        ---
##   site        plain       species_composites/    one file per category
##   site        sd          species_composites_sd/
##   basin       plain       basin_composites/      one file per basin
##   basin       sd          basin_composites_sd/
##   basin       composite   basin_species/         every category on one axis
##   puget       plain       puget_sound/           Puget_Sound_*.png
##   puget       sd          puget_sound_sd/
##   washington  plain       puget_sound/           Washington_wide_*.png
##   washington  sd          puget_sound_sd/
##
## The two region scales share a folder and are told apart by file name. The
## washington pair is skipped when the metrics table has no Washington_wide
## rows, which is what happens when the outer basins were excluded upstream.
##
## "composite" exists only at the basin scale -- no other scale has an
## equivalent one-axis layout -- so asking for it alone draws nothing.
from_env_set <- function(var, all) {
  value <- Sys.getenv(var, unset = "")
  if (!nzchar(value)) return(all)
  chosen <- trimws(strsplit(value, ",", fixed = TRUE)[[1]])
  intersect(all, chosen[nzchar(chosen)])
}

want_groups <- from_env_set("UKELP_FIG_GROUPS",
                            vapply(metric_groups, function(g) g$folder, character(1)))
want_scales <- from_env_set("UKELP_FIG_SCALES",
                            c("site", "basin", "puget", "washington", "percent"))
want_types  <- from_env_set("UKELP_FIG_TYPES",  c("plain", "sd", "composite"))

## A region scale is only drawable if the metrics table holds that region.
regions_available <- unique(as.character(df_puget$region))
region_for_scale <- c(puget = "Puget_Sound", washington = "Washington_wide")

for (sc in names(region_for_scale)) {
  if (sc %in% want_scales && !region_for_scale[[sc]] %in% regions_available) {
    want_scales <- setdiff(want_scales, sc)
    message("Skipping the '", sc, "' scale -- the metrics table has no ",
            region_for_scale[[sc]], " rows.")
  }
}

## true when a given figure set has been asked for
drawing <- function(scale, type) scale %in% want_scales && type %in% want_types

metric_groups <- Filter(function(g) g$folder %in% want_groups, metric_groups)

if (!length(metric_groups) || !length(want_scales) || !length(want_types)) {
  message("Nothing selected to draw -- no figures written.")
}

## Only the folders a selected set will actually write into, so deselecting a
## group does not leave empty folders behind
fig_roots <- unique(c(
  if (drawing("basin", "plain"))     figs_basin,
  if (drawing("basin", "sd"))        figs_basin_sd,
  if (drawing("basin", "composite")) figs_basin_species,
  if (drawing("site",  "plain"))     figs_species,
  if (drawing("site",  "sd"))        figs_species_sd,
  if (drawing("puget", "plain") || drawing("washington", "plain")) figs_puget,
  if (drawing("puget", "sd")    || drawing("washington", "sd"))    figs_puget_sd,
  if ("percent" %in% want_scales) figs_percent
))
group_folders <- vapply(metric_groups, function(grp) grp$folder, character(1))

if (length(fig_roots) && length(group_folders)) {
  for (fig_dir in c(figs, fig_roots,
                    as.vector(t(outer(fig_roots, group_folders, file.path))))) {
    if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)
  }
}



## trajectories into figs/basin_composites/[group]/, and the standard deviation
## band in their place into figs/basin_composites_sd/[group]/
basin_variants <- c(
  if (drawing("basin", "plain")) list(list(show_sd = FALSE, root = figs_basin)),
  if (drawing("basin", "sd"))    list(list(show_sd = TRUE,  root = figs_basin_sd))
)

for (grp in metric_groups) {
  for (focal_basin in sort(unique(as.character(df_basin$basin)))) {
    for (variant in basin_variants) {
      basin_density_facets <- basin_density_figure(focal_basin, grp, variant$show_sd)
      show(basin_density_facets)

      basin_fig_file <- file.path(variant$root, grp$folder,
                                  paste0(safe_file_name(focal_basin), "_avg_density.png"))
      ggsave(basin_fig_file, plot = basin_density_facets,
             width = fig_width, height = fig_height, dpi = fig_dpi, bg = "white")
      message("wrote ", basin_fig_file)
    }
  }
}



## one figure per basin per group, drawn to the device and written to
## figs/basin_species/[group]/[basin_name]_species_avg_density.png -- the
## "_species" keeps these from overwriting the faceted basin figures above.
## This is the basin scale's "composite" layout; there is no site or sound-wide
## equivalent, so it answers to that one pairing alone.
for (grp in if (drawing("basin", "composite")) metric_groups else list()) {
  for (focal_basin in sort(unique(as.character(df_basin$basin)))) {
    basin_species_plot <- basin_species_figure(focal_basin, grp)
    show(basin_species_plot)

    basin_species_file <- file.path(figs_basin_species, grp$folder,
                                    paste0(safe_file_name(focal_basin),
                                           "_species_avg_density.png"))
    ggsave(basin_species_file, plot = basin_species_plot,
           width = fig_width, height = fig_height, dpi = fig_dpi, bg = "white")
    message("wrote ", basin_species_file)
  }
}


## one combined figure per category per group, drawn to the device and written to
## figs/species_composites/[group]/[category_name]_avg_density.png. Files are
## produced in the group's declared order, matching the facet order used by every
## other figure set; the basin panels inside each one stay alphabetical
species_variants <- c(
  if (drawing("site", "plain")) list(list(show_sd = FALSE, root = figs_species)),
  if (drawing("site", "sd"))    list(list(show_sd = TRUE,  root = figs_species_sd))
)

for (grp in metric_groups) {
  species_cols <- group_cols(grp)

  for (focal_species in species_cols) {
    for (variant in species_variants) {
      species_density_facets <- species_density_figure(focal_species, grp, variant$show_sd)
      show(species_density_facets)

      species_fig_file <- file.path(variant$root, grp$folder,
                                    paste0(safe_file_name(species_label(focal_species)),
                                           "_avg_density.png"))
      ggsave(species_fig_file, plot = species_density_facets,
             width = fig_width, height = fig_height, dpi = fig_dpi, bg = "white")
      message("wrote ", species_fig_file)
    }
  }
}


## one region-wide figure per group per variant, written to
## figs/puget_sound/[group]/ and figs/puget_sound_sd/[group]/. The two regions
## share those folders and are told apart by file name:
## Puget_Sound_avg_density.png and Washington_wide_avg_density.png.
sound_variants <- c(
  if (drawing("puget", "plain"))
    list(list(show_sd = FALSE, root = figs_puget,    region = "Puget_Sound")),
  if (drawing("puget", "sd"))
    list(list(show_sd = TRUE,  root = figs_puget_sd, region = "Puget_Sound")),
  if (drawing("washington", "plain"))
    list(list(show_sd = FALSE, root = figs_puget,    region = "Washington_wide")),
  if (drawing("washington", "sd"))
    list(list(show_sd = TRUE,  root = figs_puget_sd, region = "Washington_wide"))
)

for (grp in metric_groups) {
  for (variant in sound_variants) {
    sound_density_facets <- sound_density_figure(grp, variant$show_sd, variant$region)
    show(sound_density_facets)

    sound_fig_file <- file.path(variant$root, grp$folder,
                                paste0(safe_file_name(variant$region), "_avg_density.png"))
    ggsave(sound_fig_file, plot = sound_density_facets,
           width = fig_width, height = fig_height, dpi = fig_dpi, bg = "white")
    message("wrote ", sound_fig_file)
  }
}


# -----------------------------------
## percent difference from the average of all other years

## One figure per basin per category -- deliberately isolated, so nothing shares
## an axis with anything else. Percent change explodes when the baseline is near
## zero (the data reaches +11,600%), which would flatten every other line on a
## shared axis.
##
## This scale has no plain/sd/composite variants, so it answers to the scale
## alone rather than to a (scale, type) pair. Figures that cannot be drawn
## honestly are skipped with the reason reported, rather than being drawn wrong.
if ("percent" %in% want_scales) {
  made <- 0L
  skipped <- character(0)

  for (grp in metric_groups) {
    for (focal_basin in sort(unique(as.character(df_basin$basin)))) {
      for (focal_col in group_cols(grp)) {
        res <- percent_change_figure(focal_basin, focal_col)
        if (!isTRUE(res$ok)) {
          skipped <- c(skipped, res$reason)
          next
        }
        f <- file.path(figs, percent_change_file(focal_basin, focal_col, grp$folder))
        dir.create(dirname(f), recursive = TRUE, showWarnings = FALSE)
        show(res$plot)
        ggsave(f, plot = res$plot, width = fig_width, height = fig_height,
               dpi = fig_dpi, bg = "white")
        message("wrote ", f)
        made <- made + 1L
      }
    }
  }

  message(sprintf("percent change: %d figures written, %d skipped", made,
                  length(skipped)))
  for (r in skipped) message("  skipped -- ", r)
}
