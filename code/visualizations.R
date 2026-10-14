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
excluded_basins <- c(
  "Northern Coast",
  "Western Strait of Juan de Fuca",
  "Eastern Strait of Juan de Fuca"
)

df_site <- df_site %>% filter(!gsub("_", " ", basin) %in% excluded_basins)
df_basin <- df_basin %>% filter(!gsub("_", " ", basin) %in% excluded_basins)

my.theme = theme(panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.background = element_blank(),
                 axis.line = element_line(colour = "black"),
                 axis.title=element_text(size=16),
                 axis.text=element_text(size=14),
                 plot.title = element_text(size=16))

## size (inches) and resolution of the exported figures; the on-screen device is
## opened at the same size, so what's drawn matches what's written to figs/
fig_width <- 12
fig_height <- 6
fig_dpi <- 300

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

## ---- METRIC GROUPS ---------------------------------------------------------
## Every figure set below is built once per group, into its own subfolder.
## "ak_" is the algae data: counts scaled to a density per 60 m^2 transect. The
## other four are the UPC point-intercept categories, which are proportions of
## the points sampled along a transect -- a different quantity entirely, hence
## the separate y-axis label and the separate folder.
##
## "species" is used throughout the code below as the generic name for whatever
## a group's categories are, since the algae case came first; for the UPC groups
## read it as "category".
## ----------------------------------------------------------------------------
upc_y_lab <- expression("Average Proportion of Transect Points")

## `order` fixes the sequence categories appear in -- facet layout, legend order
## and colour assignment all follow it, so a category sits in the same place in
## every figure of its group. Groups without an `order` are sorted alphabetically
## by their displayed label. Relief runs smallest to largest, substrate runs
## finest to coarsest and then the non-sediment classes.
##
## Column names are the post-read.csv form, where hyphens have become periods.
## sb_cobble_avg and sb_boulder_avg are listed but no longer exist -- both were
## recorded only in Northern Coast, one of the EXCLUDED BASINS -- so they are
## skipped silently and drop back into their declared slot if that basin returns.
metric_groups <- list(
  list(prefix = "ak_", folder = "algae",
       y_lab = expression("Avgerage Density per 60m"^2*" Transect")),
  list(prefix = "cv_", folder = "cover",      y_lab = upc_y_lab),
  list(prefix = "rf_", folder = "relief",     y_lab = upc_y_lab,
       order = c("rf_0.10cm_avg", "rf_10cm.1m_avg", "rf_1m.2m_avg",
                 "rf_over_2m_avg")),
  list(prefix = "sb_", folder = "substrate",  y_lab = upc_y_lab,
       order = c("sb_clay.wa_avg", "sb_sand_avg", "sb_pebble.wa_avg",
                 "sb_shell_hash.wa_avg", "sb_cobble.wa_avg", "sb_cobble_avg",
                 "sb_rock_avg", "sb_s_boulder.wa_avg", "sb_l_boulder.wa_avg",
                 "sb_boulder_avg", "sb_reef_avg", "sb_other_avg")),
  list(prefix = "sp_", folder = "superlayer", y_lab = upc_y_lab)
)

## categories excluded from every composite visualization (algae only; the
## setdiff below is a no-op for the four UPC groups)
exclude_species <- c("ak_bull_kelp_avg", "ak_feather_boa_n_avg", "ak_giant_kelp_n_avg")

## the "_avg" columns belonging to one group, shared by the site-, basin- and
## sound-scale tables, returned in the group's display order. Anything the group
## declares but that isn't in the data is skipped; anything in the data that the
## group doesn't declare is appended alphabetically rather than dropped, so a new
## category added upstream still shows up somewhere rather than vanishing
group_cols <- function(grp) {
  cols <- grep(paste0("^", grp$prefix, ".*_avg$"), names(df_basin), value = TRUE)
  cols <- setdiff(cols, exclude_species)

  alphabetical <- function(x) x[order(vapply(x, species_label, character(1)))]

  if (is.null(grp$order)) return(alphabetical(cols))
  declared <- intersect(grp$order, cols)
  c(declared, alphabetical(setdiff(cols, declared)))
}

## "ak_bull_kelp_avg" -> "bull kelp"; drops the "_n" used for count-based
## species. read.csv() converts hyphens in column names to periods, so periods
## are turned back into hyphens and only underscores become spaces -- without
## that, "rf_0.10cm" would read "0 10cm" rather than "0-10cm"
species_label <- function(col) {
  label <- sub("^(ak|cv|rf|sb|sp)_", "", col)
  label <- sub("_avg$", "", label)
  label <- sub("_n$", "", label)
  gsub("_", " ", gsub("[.]", "-", label))
}

## scientific name for each retained kelp species -- fill in manually, keyed by
## the raw column name (e.g. species_sci_names["ak_sugar_kelp_avg"] <- "Saccharina latissima")
## The UPC categories have no scientific names; facet_labels_for() falls through
## to the common name alone for anything missing here.
species_sci_names <- c(
  "ak_acid_weed_avg" = "Desmarestia spp.",
  "ak_broad.ribbed_kelp_avg" = "Pleurophycus gardneri",
  "ak_five.ribbed_kelp_avg" = "Costaria costata",
  "ak_sieve_kelp_avg" = "A. clathratum & N. fimbriatum",
  "ak_sugar_kelp_avg" = "Saccharina latissima",
  "ak_three.ribbed_kelp_avg" = "Cymathaere triplicata",
  "ak_torn_kelp_avg" = "Laminaria setchellii",
  "ak_winged_kelp_avg" = "Alaria marginata",
  "ak_wire_weed_avg" = "Sargassum muticum",
  "ak_woody_kelp_avg" = "Pterygophora californica"
)

## adjust this to resize the facet strip titles (both composite plots)
facet_label_size <- 8

## builds each facet's title as "common name (scientific name)", italicizing the
## scientific name via a plotmath expression string (parsed by label_parsed);
## omits the parentheses until a scientific name has been filled in above
facet_labels_for <- function(cols) {
  common <- vapply(cols, species_label, character(1))
  sci <- species_sci_names[cols]
  ifelse(is.na(sci) | sci == "",
         paste0('"', common, '"'),
         paste0('"', common, '"~"("*italic("', sci, '")*")"'))
}

## basin names aren't guaranteed to be file-path safe (e.g. the "/" in
## "Saratoga/Whidbey_Basin"), so collapse anything outside [A-Za-z0-9_-] to "_"
safe_file_name <- function(x) gsub("[^A-Za-z0-9_-]+", "_", x)

## each set of figures writes to its own subfolder of figs/, and within that to a
## folder per metric group, so the sets stay separable as they grow
figs_basin <- file.path(figs, "basin_composites")          # one per basin, faceted by category
figs_basin_species <- file.path(figs, "basin_species")     # one per basin, all categories on one axis
figs_species <- file.path(figs, "species_composites")      # one per category, faceted by basin
figs_puget <- file.path(figs, "puget_sound")               # the sound-wide composite

## the "_sd" siblings of the three sets that carry gray composite lines. Each
## drops the gray trajectories and shades the composite's standard deviation
## underneath the average line instead. basin_species has no gray lines -- it is
## one colored line per category -- so it has no _sd counterpart
figs_basin_sd <- file.path(figs, "basin_composites_sd")
figs_species_sd <- file.path(figs, "species_composites_sd")
figs_puget_sd <- file.path(figs, "puget_sound_sd")

## the composite average's line colour, and the faded fill used for its standard
## deviation band -- same hue, dropped to a low alpha
composite_color <- "#1963b0"
composite_fill_alpha <- 0.25

## ---- WHAT TO DRAW ----------------------------------------------------------
## Run this script on its own and it draws everything, exactly as it always has.
## The Shiny app in gui/ sets the three variables below first, so a person can
## ask for a subset without editing anything here.
##
## Each figure set is one (scale, type) pair:
##
##   scale   type        set
##   -----   ----        ---
##   site    plain       species_composites/      one file per category
##   site    sd          species_composites_sd/
##   basin   plain       basin_composites/        one file per basin
##   basin   sd          basin_composites_sd/
##   basin   composite   basin_species/           every category on one axis
##   puget   plain       puget_sound/             one sound-wide file
##   puget   sd          puget_sound_sd/
##
## "composite" exists only at the basin scale -- the other two scales have no
## equivalent one-axis layout -- so asking for it alone draws nothing.
from_env_set <- function(var, all) {
  value <- Sys.getenv(var, unset = "")
  if (!nzchar(value)) return(all)
  chosen <- trimws(strsplit(value, ",", fixed = TRUE)[[1]])
  intersect(all, chosen[nzchar(chosen)])
}

want_groups <- from_env_set("UKELP_FIG_GROUPS",
                            vapply(metric_groups, function(g) g$folder, character(1)))
want_scales <- from_env_set("UKELP_FIG_SCALES", c("site", "basin", "puget"))
want_types  <- from_env_set("UKELP_FIG_TYPES",  c("plain", "sd", "composite"))

## true when a given figure set has been asked for
drawing <- function(scale, type) scale %in% want_scales && type %in% want_types

metric_groups <- Filter(function(g) g$folder %in% want_groups, metric_groups)

if (!length(metric_groups) || !length(want_scales) || !length(want_types)) {
  message("Nothing selected to draw -- no figures written.")
}

## Only the folders a selected set will actually write into, so deselecting a
## group does not leave empty folders behind
fig_roots <- c(
  if (drawing("basin", "plain"))     figs_basin,
  if (drawing("basin", "sd"))        figs_basin_sd,
  if (drawing("basin", "composite")) figs_basin_species,
  if (drawing("site",  "plain"))     figs_species,
  if (drawing("site",  "sd"))        figs_species_sd,
  if (drawing("puget", "plain"))     figs_puget,
  if (drawing("puget", "sd"))        figs_puget_sd
)
group_folders <- vapply(metric_groups, function(grp) grp$folder, character(1))

if (length(fig_roots) && length(group_folders)) {
  for (fig_dir in c(figs, fig_roots,
                    as.vector(t(outer(fig_roots, group_folders, file.path))))) {
    if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)
  }
}




# -----------------------------------
## basin-level average densities with composite sites

## builds the composite figure for one basin, one facet per category. With
## show_sd = FALSE that is the gray trajectory of every site in the basin behind
## the blue basin-wide average; with show_sd = TRUE the gray trajectories give
## way to a faded band showing the basin average's standard deviation
basin_density_figure <- function(focal_basin, grp, show_sd = FALSE) {

  species_cols <- group_cols(grp)

  ## reshape each scale to long format so every category can be one facet
  df_site_focal <- df_site %>%
    filter(basin == focal_basin) %>%
    select(site, year, all_of(species_cols)) %>%
    pivot_longer(all_of(species_cols), names_to = "species", values_to = "density")

  ## the "_sd" sibling of each "_avg" column comes along so the band can be
  ## drawn; names_pattern splits "ak_sugar_kelp_avg"/"ak_sugar_kelp_sd" into one
  ## row carrying both, and the "_avg" suffix is put back so the category name
  ## still matches what facet_labels_for() is keyed on
  df_basin_focal <- df_basin %>%
    filter(basin == focal_basin) %>%
    select(year, all_of(species_cols), all_of(sub("_avg$", "_sd", species_cols))) %>%
    pivot_longer(-year, names_to = c("species", ".value"),
                 names_pattern = "^(.*)_(avg|sd)$") %>%
    mutate(species = paste0(species, "_avg"),
           density = avg,
           ## a symmetric +/- 1 SD band, with the lower edge clamped at zero:
           ## densities and proportions can't go negative, and avg - sd does for
           ## roughly a third of the values
           sd_lower = pmax(avg - sd, 0),
           sd_upper = avg + sd)

  ## number of composite sites with at least one non-zero value, per category --
  ## annotated on each facet, but deliberately not used to order them
  n_sites_focal <- df_site_focal %>%
    group_by(species) %>%
    summarise(n_sites = n_distinct(site[!is.na(density) & density != 0]),
              .groups = "drop")

  ## facet_wrap() lays panels out in factor-level order, so holding the levels at
  ## the group's declared order keeps a category in the same position in every
  ## figure of its group, whatever basin is in view
  ordered_cols <- species_cols
  ordered_labels <- facet_labels_for(ordered_cols)
  as_ordered_species <- function(df) {
    mutate(df, species = factor(species, levels = ordered_cols, labels = ordered_labels))
  }

  df_site_focal <- as_ordered_species(df_site_focal)
  df_basin_focal <- as_ordered_species(df_basin_focal)
  n_sites_focal <- as_ordered_species(n_sites_focal)

  ## either the gray site trajectories, or the standard deviation band in their
  ## place -- never both, so the band isn't lost behind the gray lines
  composite_backdrop <- if (show_sd) {
    list(geom_ribbon(data = df_basin_focal,
                     aes(x = year, ymin = sd_lower, ymax = sd_upper, group = 1),
                     fill = composite_color, alpha = composite_fill_alpha))
  } else {
    list(geom_path(data = df_site_focal,
                   aes(x = year, y = density, group = site),
                   color = "gray70", linewidth = 0.6, alpha = 0.6),
         geom_point(data = df_site_focal,
                    aes(x = year, y = density, group = site),
                    color = "gray70", size = 1, alpha = 0.6))
  }

  ggplot() +
    composite_backdrop +
    geom_path(data = df_basin_focal,
              aes(x = year, y = density, group = 1),
              color = composite_color, linewidth = 1.3) +
    geom_point(data = df_basin_focal,
               aes(x = year, y = density, group = 1),
               color = composite_color, size = 2) +
    geom_text(data = n_sites_focal,
              aes(x = -Inf, y = Inf, label = paste0("n = ", n_sites, " sites")),
              hjust = -0.1, vjust = 1.5,
              size = 3.2, fontface = 3, inherit.aes = FALSE) +
    facet_wrap(~species, scales = "free_y", ncol = 4, labeller = label_parsed) +
    my.theme +
    labs(y = grp$y_lab,
         title = paste0(gsub("_", " ", focal_basin))) +
    theme(
      axis.text.x = element_text(angle = 0, size = 9, hjust = 0.5),
      axis.title.x = element_blank(),
      axis.text.y = element_text(size = 9),
      axis.title.y = element_text(size = 12, hjust = 0.5),
      strip.text = element_text(size = facet_label_size),
      plot.title = element_text(hjust = 0.5),
      legend.position = "none")
}

## one combined figure per basin per group, in both variants: gray site
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




# -----------------------------------
## basin-level average densities by category

## builds the figure for one basin: every category's basin average drawn on a
## single set of axes, one colored line each, so the categories can be compared
## directly against each other within the basin
basin_species_figure <- function(focal_basin, grp) {

  species_cols <- group_cols(grp)

  ## categories keep species_cols order (alphabetical), so a given category draws
  ## in the same color and sits in the same legend position in every basin figure
  df_basin_focal <- df_basin %>%
    filter(basin == focal_basin) %>%
    select(year, all_of(species_cols)) %>%
    pivot_longer(all_of(species_cols), names_to = "species", values_to = "density") %>%
    mutate(species = factor(species, levels = species_cols))

  ggplot(df_basin_focal,
         aes(x = year, y = density, group = species, color = species)) +
    geom_path(linewidth = 1.1) +
    geom_point(size = 2.4) +
    ## common name only in this legend -- the faceted figures elsewhere pair it
    ## with the scientific name, but ten of those make the key too wide to sit
    ## beside a single panel. vapply() names each label with its column, so the
    ## labels match the factor levels by name rather than by position
    scale_color_brewer(palette = "Paired",
                       labels = vapply(species_cols, species_label, character(1))) +
    my.theme +
    labs(y = grp$y_lab,
         title = gsub("_", " ", focal_basin),
         color = NULL) +
    theme(
      axis.title.x = element_blank(),
      plot.title = element_text(hjust = 0.5),
      legend.text = element_text(size = 10),
      legend.key.width = unit(1.4, "lines"),
      legend.position = "right")
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




# -----------------------------------
## category-level average densities with composite sites

## the mirror image of the basin figure above: a focal category across every
## basin, rather than a focal basin across every category. One facet per basin.
## show_sd swaps the gray site trajectories for a faded band showing the basin
## average's standard deviation, exactly as in basin_density_figure()
species_density_figure <- function(focal_species, grp, show_sd = FALSE) {

  ## one column instead of a pivot -- the focal category is the only one drawn,
  ## so basin becomes what varies across the facets
  df_site_focal <- df_site %>%
    select(basin, site, year, density = all_of(focal_species))

  ## a symmetric +/- 1 SD band, with the lower edge clamped at zero: densities
  ## and proportions can't go negative, and avg - sd does for roughly a third of
  ## the values
  df_basin_focal <- df_basin %>%
    select(basin, year,
           density = all_of(focal_species),
           sd_value = all_of(sub("_avg$", "_sd", focal_species))) %>%
    mutate(sd_lower = pmax(density - sd_value, 0),
           sd_upper = density + sd_value)

  ## number of composite sites with at least one non-zero value, per basin --
  ## annotated on each facet, but deliberately not used to order them
  n_sites_focal <- df_site_focal %>%
    group_by(basin) %>%
    summarise(n_sites = n_distinct(site[!is.na(density) & density != 0]),
              .groups = "drop")

  ## facet_wrap() lays panels out in factor-level order, so holding the levels
  ## alphabetical keeps the layout identical from one category figure to the next
  ## -- a given basin is always in the same position, which the most-observed
  ## -first sort used elsewhere would break
  ordered_basins <- sort(unique(as.character(df_basin$basin)))
  as_ordered_basin <- function(df) {
    mutate(df, basin = factor(as.character(basin), levels = ordered_basins))
  }

  df_site_focal <- as_ordered_basin(df_site_focal)
  df_basin_focal <- as_ordered_basin(df_basin_focal)
  n_sites_focal <- as_ordered_basin(n_sites_focal)

  ## same "common name (scientific name)" construction the facet strips use,
  ## parsed into an expression so the title can carry the italics
  species_title <- parse(text = facet_labels_for(focal_species))

  composite_backdrop <- if (show_sd) {
    list(geom_ribbon(data = df_basin_focal,
                     aes(x = year, ymin = sd_lower, ymax = sd_upper, group = 1),
                     fill = composite_color, alpha = composite_fill_alpha))
  } else {
    list(geom_path(data = df_site_focal,
                   aes(x = year, y = density, group = site),
                   color = "gray70", linewidth = 0.6, alpha = 0.6),
         geom_point(data = df_site_focal,
                    aes(x = year, y = density, group = site),
                    color = "gray70", size = 1, alpha = 0.6))
  }

  ggplot() +
    composite_backdrop +
    geom_path(data = df_basin_focal,
              aes(x = year, y = density, group = 1),
              color = composite_color, linewidth = 1.3) +
    geom_point(data = df_basin_focal,
               aes(x = year, y = density, group = 1),
               color = composite_color, size = 2) +
    geom_text(data = n_sites_focal,
              aes(x = -Inf, y = Inf, label = paste0("n = ", n_sites, " sites")),
              hjust = -0.1, vjust = 1.5,
              size = 3.2, fontface = 3, inherit.aes = FALSE) +
    facet_wrap(~basin, scales = "free_y", ncol = 4,
               labeller = as_labeller(function(x) gsub("_", " ", x))) +
    my.theme +
    labs(y = grp$y_lab,
         title = species_title) +
    theme(
      axis.text.x = element_text(angle = 0, size = 9, hjust = 0.5),
      axis.title.x = element_blank(),
      axis.text.y = element_text(size = 9),
      axis.title.y = element_text(size = 12, hjust = 0.5),
      strip.text = element_text(size = facet_label_size),
      plot.title = element_text(hjust = 0.5),
      legend.position = "none")
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




# -----------------------------------
## puget sound-level average densities with composite basins

ps_wide <- "Puget_Sound_wide"

## builds the sound-wide figure for one group, one facet per category: the gray
## trajectory of every basin composite behind the blue sound-wide average, or
## with show_sd = TRUE that average's standard deviation band in their place.
## The band is the spread among basins, since basins are what this scale averages
sound_density_figure <- function(grp, show_sd = FALSE) {

  species_cols <- group_cols(grp)

  ## reshape each scale to long format so every category can be one facet
  ## (df_basin holds every retained basin, so no basin-level filter is needed
  ## here -- it's the full composite behind the sound-wide average)
  df_basin_all <- df_basin %>%
    select(basin, year, all_of(species_cols)) %>%
    pivot_longer(all_of(species_cols), names_to = "species", values_to = "density")

  ## symmetric +/- 1 SD, lower edge clamped at zero, as in the other two sets
  df_puget_focal <- df_puget %>%
    filter(region == ps_wide) %>%
    select(year, all_of(species_cols), all_of(sub("_avg$", "_sd", species_cols))) %>%
    pivot_longer(-year, names_to = c("species", ".value"),
                 names_pattern = "^(.*)_(avg|sd)$") %>%
    mutate(species = paste0(species, "_avg"),
           density = avg,
           sd_lower = pmax(avg - sd, 0),
           sd_upper = avg + sd)

  ## number of composite basins with at least one non-zero value, per category --
  ## annotated on each facet, not used to order them. The sound-wide composite is
  ## built from basins, so basins are what gets counted here
  n_basins_focal <- df_basin_all %>%
    group_by(species) %>%
    summarise(n_basins = n_distinct(basin[!is.na(density) & density != 0]),
              .groups = "drop")

  ## same declared order as every other figure in the group
  ordered_cols <- species_cols
  ordered_labels <- facet_labels_for(ordered_cols)
  as_ordered_species <- function(df) {
    mutate(df, species = factor(species, levels = ordered_cols, labels = ordered_labels))
  }

  df_basin_all <- as_ordered_species(df_basin_all)
  df_puget_focal <- as_ordered_species(df_puget_focal)
  n_basins_focal <- as_ordered_species(n_basins_focal)

  composite_backdrop <- if (show_sd) {
    list(geom_ribbon(data = df_puget_focal,
                     aes(x = year, ymin = sd_lower, ymax = sd_upper, group = 1),
                     fill = composite_color, alpha = composite_fill_alpha))
  } else {
    list(geom_path(data = df_basin_all,
                   aes(x = year, y = density, group = basin),
                   color = "gray70", linewidth = 0.6, alpha = 0.6),
         geom_point(data = df_basin_all,
                    aes(x = year, y = density, group = basin),
                    color = "gray70", size = 1, alpha = 0.6))
  }

  ggplot() +
    composite_backdrop +
    geom_path(data = df_puget_focal,
              aes(x = year, y = density, group = 1),
              color = composite_color, linewidth = 1.3) +
    geom_point(data = df_puget_focal,
               aes(x = year, y = density, group = 1),
               color = composite_color, size = 2) +
    geom_text(data = n_basins_focal,
              aes(x = -Inf, y = Inf, label = paste0("basin n = ", n_basins)),
              hjust = -0.1, vjust = 1.5,
              size = 3.2, fontface = 3, inherit.aes = FALSE) +
    facet_wrap(~species, scales = "free_y", ncol = 4, labeller = label_parsed) +
    my.theme +
    labs(y = grp$y_lab,
         title = "Puget Sound-wide") +
    theme(
      axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
      axis.title.x = element_blank(),
      axis.text.y = element_text(size = 9),
      axis.title.y = element_text(size = 12, hjust = 0.5),
      strip.text = element_text(size = facet_label_size),
      plot.title = element_text(hjust = 0.5),
      legend.position = "none")
}

## one sound-wide figure per group in both variants, written to
## figs/puget_sound/[group]/ and figs/puget_sound_sd/[group]/
sound_variants <- c(
  if (drawing("puget", "plain")) list(list(show_sd = FALSE, root = figs_puget)),
  if (drawing("puget", "sd"))    list(list(show_sd = TRUE,  root = figs_puget_sd))
)

for (grp in metric_groups) {
  for (variant in sound_variants) {
    sound_density_facets <- sound_density_figure(grp, variant$show_sd)
    show(sound_density_facets)

    sound_fig_file <- file.path(variant$root, grp$folder,
                                paste0(safe_file_name(ps_wide), "_avg_density.png"))
    ggsave(sound_fig_file, plot = sound_density_facets,
           width = fig_width, height = fig_height, dpi = fig_dpi, bg = "white")
    message("wrote ", sound_fig_file)
  }
}
