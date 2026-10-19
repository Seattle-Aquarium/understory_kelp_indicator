## start up ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## clear working history
rm(list=ls())

## Writes the figure sets. Every figure is defined in figure_functions.R, which
## the Shiny app's preview uses too; this script only decides which ones to
## draw and where to save them.

## set working directory to home folder
setwd("../")

## relative file paths
data <- "data/Reef_Check"
code <- "code"
results <- "results"
figs <- "figs"

## ---- FILE LOCATIONS --------------------------------------------------------
## Run this script on its own and it reads and writes the repository's own
## folders. The Shiny app in gui/ sets the environment variables below first, so
## a person can point it at files kept somewhere else without editing anything
## here. See rc_full_data_clean.R for why environment variables rather than
## options().
from_env <- function(var, default) {
  value <- Sys.getenv(var, unset = "")
  if (nzchar(value)) value else default
}

table_paths <- list(
  transect    = from_env("UKELP_TRANSECT_CSV",    file.path(results, "metrics_transect_scale.csv")),
  site        = from_env("UKELP_SITE_CSV",        file.path(results, "metrics_site_scale.csv")),
  basin       = from_env("UKELP_BASIN_CSV",       file.path(results, "metrics_basin_scale.csv")),
  puget       = from_env("UKELP_PUGET_CSV",       file.path(results, "metrics_puget_sound_scale.csv")),
  trend_site  = from_env("UKELP_TREND_SITE_CSV",  file.path(results, "trend_site_scale.csv")),
  trend_basin = from_env("UKELP_TREND_BASIN_CSV", file.path(results, "trend_basin_scale.csv")),
  trend_puget = from_env("UKELP_TREND_PUGET_CSV", file.path(results, "trend_puget_sound_scale.csv")),
  nmds_points  = from_env("UKELP_NMDS_POINTS_CSV",  file.path(results, "nmds_algae_points.csv")),
  nmds_species = from_env("UKELP_NMDS_SPECIES_CSV", file.path(results, "nmds_algae_species.csv")),
  nmds_stress  = from_env("UKELP_NMDS_STRESS_CSV",  file.path(results, "nmds_algae_stress.csv"))
)
figs <- from_env("UKELP_FIGS_DIR", figs)

## ---- EXCLUDED BASINS -------------------------------------------------------
## Northern Coast, Western Strait of Juan de Fuca and Eastern Strait of Juan de
## Fuca are held out of every output in this repo unless UKELP_INCLUDE_BASINS
## names them -- that is the per-basin checkboxes in the Shiny app, which can
## bring back one, two or all three independently. Unset, which is how this
## script runs on its own, all three stay excluded. The Puget Sound tables are
## built without them by metrics_across_scale.R, so re-running that script is
## what keeps the blue sound-wide line consistent with the basins plotted here.
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

## ---- SHARED FIGURE DEFINITIONS ---------------------------------------------
## Sourced into this script's own environment, so the builders see the tables
## load_figure_tables() reads as globals.
source(file.path(code, "figure_functions.R"))
load_figure_tables(table_paths, exclude = excluded_basins)

if (is.null(df_basin)) stop("No basin-scale table at ", table_paths$basin, " -- run step 2 first.")

## The recorded on-screen device is for working through the figures in RStudio.
## Run headless -- from Rscript, or from the Shiny app -- there is no one to page
## through it, and opening a device anyway pops a window in front of whatever
## the person is doing and leaves a stray Rplots.pdf behind. So the device, and
## the print() calls that draw to it, happen only in an interactive session;
## ggsave() writes the files either way.
draw_to_screen <- interactive()

if (draw_to_screen) {
  graphics.off()
  if (.Platform$OS.type == "windows") windows(fig_width, fig_height, record = TRUE)
}

## print() only where there is a device worth drawing to
show <- function(p) if (draw_to_screen) print(p) else invisible(p)

## ---- WHAT TO DRAW ----------------------------------------------------------
## Every figure is one (analysis, scale, view, group) -- the same four choices
## as the app's preview -- and each set covers every place or species that
## combination applies to:
##
##   view       one file per
##   within     site, basin or region                 one panel per species
##   across     species (basin scale), or basin x     one panel per basin, or
##              species (site scale)                  per site in that basin
##   composite  site, basin or region                 every species, one panel
##
## into figs/<analysis>/<scale>/<view>/<group>/ -- see figure_file(). The NMDS
## of the algae community is a set of its own, whatever scales, views and data
## types are ticked: coloured by basin (plain, and sized by each species and by
## the total), by each basin's sites, by year and by depth zone.
##
## Run on its own this draws every analysis, view and group at the basin and
## Puget Sound scales. The site scale is left out unless asked for: it is one
## file per site per group per view, about 1,500 files. The Shiny app sets the
## five variables below to whatever is ticked.
from_env_set <- function(var, all, default = all) {
  value <- Sys.getenv(var, unset = "")
  if (!nzchar(value)) return(default)
  chosen <- trimws(strsplit(value, ",", fixed = TRUE)[[1]])
  intersect(all, chosen[nzchar(chosen)])
}

want_analyses <- from_env_set("UKELP_FIG_ANALYSES", unname(ANALYSES))
want_scales   <- from_env_set("UKELP_FIG_SCALES",   unname(SCALES), default = c("basin", "puget"))
want_views    <- from_env_set("UKELP_FIG_VIEWS",    unname(VIEWS))
want_groups   <- from_env_set("UKELP_FIG_GROUPS",
                              vapply(metric_groups, function(g) g$folder, character(1)))
y_lock        <- identical(Sys.getenv("UKELP_FIG_YLOCK", unset = "0"), "1")

if (!length(want_analyses) || !length(want_scales) || !length(want_views) ||
    !length(want_groups)) {
  message("Nothing selected to draw -- no figures written.")
}

message(sprintf("Drawing %s | %s | %s | %s, y-axis %s",
                paste(want_analyses, collapse = ","), paste(want_scales, collapse = ","),
                paste(want_views, collapse = ","), paste(want_groups, collapse = ","),
                if (y_lock) "locked" else "floating"))

made <- 0L
skipped <- character(0)

## figure_sets() turns the ticked scales and views into what each analysis
## draws; the NMDS has a fixed set of its own (every colouring, at the transect
## scale) and is the algae community whatever data types are ticked
for (analysis in want_analyses) {
  groups <- if (is_nmds(analysis)) list(group_by_folder("algae"))
            else Filter(function(g) g$folder %in% want_groups, metric_groups)
  for (set in figure_sets(analysis, want_scales, want_views)) {
    for (grp in groups) {
      for (target in figure_targets(analysis, set$scale, set$view, grp)) {
        res <- make_figure(analysis, set$scale, set$view, grp, where = target$where,
                           category = target$category, y_lock = y_lock)
        rel <- figure_file(analysis, set$scale, set$view, grp, target$where, target$category)
        if (!isTRUE(res$ok)) {
          skipped <- c(skipped, paste0(rel, " -- ", res$reason))
          next
        }
        out <- file.path(figs, rel)
        dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
        show(res$plot)
        ggsave(out, plot = res$plot, width = fig_width, height = fig_height,
               dpi = fig_dpi, bg = "white")
        made <- made + 1L
        message("wrote ", out)
      }
    }
  }
}

message(sprintf("%d figures written, %d skipped", made, length(skipped)))
for (r in skipped) message("  skipped: ", r)
