## Shared figure definitions for the Understory Kelp Forests Analyzer.
##
## Everything here builds a ggplot and nothing here writes a file or decides
## what to draw. Two things use it:
##
##   code/visualizations.R   loops over every selection and saves the PNGs
##   gui/app.R               renders one figure at a time for the preview
##
## Keeping the builders in one place is the point: the preview and the saved
## file are then the same figure by construction rather than by two
## definitions being kept in step by hand.
##
## The builders read the three metrics tables from their own environment --
## `df_site`, `df_basin`, `df_puget` -- rather than taking them as arguments,
## which is how they were written when they lived in the script. A consumer
## therefore assigns those three before calling:
##
##   fig <- new.env(); sys.source("code/figure_functions.R", envir = fig)
##   fig$df_site <- ...; fig$df_basin <- ...; fig$df_puget <- ...
##   p <- fig$basin_density_figure("Hood_Canal", fig$metric_groups[[1]])
##
## Sourcing it into the global environment, as visualizations.R does, works the
## same way.

suppressPackageStartupMessages(library(tidyverse))



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


## the composite average's line colour, and the faded fill used for its standard
## deviation band -- same hue, dropped to a low alpha
composite_color <- "#1963b0"
composite_fill_alpha <- 0.25

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

# -----------------------------------
## puget sound-level average densities with composite basins

## The basins carved out of Puget Sound proper. Deliberately independent of the
## EXCLUDED BASINS filter, which empties when a run is asked to include them --
## this carve-out has to hold either way.
non_puget_basins <- c("Northern Coast",
                      "Western Strait of Juan de Fuca",
                      "Eastern Strait of Juan de Fuca")

#: region -> how it reads in a title
REGION_LABELS <- c(Puget_Sound = "Puget Sound",
                   Washington_wide = "Washington-wide")

#: Which basins belong to a region, given everything present in df_basin.
region_basins <- function(all_basins, focal_region) {
  if (identical(focal_region, "Puget_Sound")) {
    all_basins[!gsub("_", " ", as.character(all_basins)) %in% non_puget_basins]
  } else {
    all_basins
  }
}

## builds the region-wide figure for one group, one facet per category: the gray
## trajectory of every basin composite in that region behind the blue regional
## average, or with show_sd = TRUE that average's standard deviation band in
## their place. The band is the spread among basins, since basins are what this
## scale averages.
##
## `focal_region` is "Puget_Sound" (the inner basins) or "Washington_wide"
## (every basin present). The gray lines follow the region, so the Puget Sound
## figure never shows an outer-coast trajectory behind its own average.
sound_density_figure <- function(grp, show_sd = FALSE,
                                 focal_region = "Puget_Sound") {

  species_cols <- group_cols(grp)

  keep <- region_basins(unique(df_basin$basin), focal_region)

  df_basin_all <- df_basin %>%
    filter(basin %in% keep) %>%
    select(basin, year, all_of(species_cols)) %>%
    pivot_longer(all_of(species_cols), names_to = "species", values_to = "density")

  ## symmetric +/- 1 SD, lower edge clamped at zero, as in the other two sets
  df_puget_focal <- df_puget %>%
    filter(as.character(region) == focal_region) %>%
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
         title = unname(REGION_LABELS[focal_region])) +
    theme(
      axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
      axis.title.x = element_blank(),
      axis.text.y = element_text(size = 9),
      axis.title.y = element_text(size = 12, hjust = 0.5),
      strip.text = element_text(size = facet_label_size),
      plot.title = element_text(hjust = 0.5),
      legend.position = "none")
}
