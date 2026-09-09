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

## import datasets
df_site <- read.csv(file.path(results, "metrics_site_scale.csv"))
df_basin <- read.csv(file.path(results, "metrics_basin_scale.csv"))
df_puget <- read.csv(file.path(results, "metrics_puget_sound_scale.csv"))

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

graphics.off()
windows(fig_width, fig_height, record=T)

## convert to factor
df_site$site <- as.factor(df_site$site)
df_site$year <- as.factor(df_site$year)
df_basin$basin <- as.factor(df_basin$basin)
df_basin$year <- as.factor(df_basin$year)
df_puget$region <- as.factor(df_puget$region)
df_puget$year <- as.factor(df_puget$year)

## species excluded from every basin/sound-scale composite visualization
exclude_species <- c("ak_bull_kelp_avg", "ak_feather_boa_n_avg", "ak_giant_kelp_n_avg")

## "ak_bull_kelp_avg" -> "bull kelp"; drops the "_n" used for count-based species;
## "." appears in place of "-" (e.g. "three.ribbed") because read.csv() converts
## hyphens in column names to periods
species_label <- function(col) {
  label <- sub("^ak_", "", col)
  label <- sub("_avg$", "", label)
  label <- sub("_n$", "", label)
  gsub("[_.]", " ", label)
}

## scientific name for each retained kelp species -- fill in manually, keyed by
## the raw column name (e.g. species_sci_names["ak_sugar_kelp_avg"] <- "Saccharina latissima")
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

## builds each facet's title as "common name (scientific name)", bold-italicizing
## the scientific name via a plotmath expression string (parsed by label_parsed);
## omits the parentheses until a scientific name has been filled in above
facet_labels_for <- function(cols) {
  common <- vapply(cols, species_label, character(1))
  sci <- species_sci_names[cols]
  ifelse(is.na(sci) | sci == "",
         paste0('"', common, '"'),
         paste0('"', common, '"~"("*italic("', sci, '")*")"'))
}

## shared y-axis label: "Average Density per 60m^2 Transect" with superscript 2
density_y_lab <- expression("Avgerage Density per 60m"^2*" Transect")

## algae density columns shared by the site-, basin- and sound-scale tables
species_cols <- names(df_basin)[grepl("^ak_.*_avg$", names(df_basin))]
species_cols <- setdiff(species_cols, exclude_species)

## basin names aren't guaranteed to be file-path safe (e.g. the "/" in
## "Saratoga/Whidbey_Basin"), so collapse anything outside [A-Za-z0-9_-] to "_"
safe_file_name <- function(x) gsub("[^A-Za-z0-9_-]+", "_", x)

## each set of figures writes to its own subfolder of figs/, so the four sets
## stay separable as they grow
figs_basin <- file.path(figs, "basin_composites")          # one per basin, faceted by species
figs_basin_species <- file.path(figs, "basin_species")     # one per basin, all species on one axis
figs_species <- file.path(figs, "species_composites")      # one per species, faceted by basin
figs_puget <- file.path(figs, "puget_sound")               # the sound-wide composite

for (fig_dir in c(figs, figs_basin, figs_basin_species, figs_species, figs_puget)) {
  if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)
}





# -----------------------------------
## basin-level average densities with composite sites

## builds the composite figure for one basin: the gray trajectory of every site
## in that basin behind the blue basin-wide average, one facet per species
basin_density_figure <- function(focal_basin) {

  ## reshape each scale to long format so every species can be one facet
  df_site_focal <- df_site %>%
    filter(basin == focal_basin) %>%
    select(site, year, all_of(species_cols)) %>%
    pivot_longer(all_of(species_cols), names_to = "species", values_to = "density")

  df_basin_focal <- df_basin %>%
    filter(basin == focal_basin) %>%
    select(year, all_of(species_cols)) %>%
    pivot_longer(all_of(species_cols), names_to = "species", values_to = "density")

  ## number of composite sites with at least one non-zero density value, per
  ## species; sorted here so the facets below run from the most-observed species
  ## to the least, with ties broken alphabetically
  n_sites_focal <- df_site_focal %>%
    group_by(species) %>%
    summarise(n_sites = n_distinct(site[!is.na(density) & density != 0]),
              .groups = "drop") %>%
    arrange(desc(n_sites), species)

  ## facet_wrap() lays panels out in factor-level order, so that sort is applied
  ## by making species a factor whose levels are the sorted columns
  ordered_cols <- n_sites_focal$species
  ordered_labels <- facet_labels_for(ordered_cols)
  as_ordered_species <- function(df) {
    mutate(df, species = factor(species, levels = ordered_cols, labels = ordered_labels))
  }

  df_site_focal <- as_ordered_species(df_site_focal)
  df_basin_focal <- as_ordered_species(df_basin_focal)
  n_sites_focal <- as_ordered_species(n_sites_focal)

  ggplot() +
    geom_path(data = df_site_focal,
              aes(x = year, y = density, group = site),
              color = "gray70", linewidth = 0.6, alpha = 0.6) +
    geom_point(data = df_site_focal,
               aes(x = year, y = density, group = site),
               color = "gray70", size = 1, alpha = 0.6) +
    geom_path(data = df_basin_focal,
              aes(x = year, y = density, group = 1),
              color = "#1963b0", linewidth = 1.3) +
    geom_point(data = df_basin_focal,
               aes(x = year, y = density, group = 1),
               color = "#1963b0", size = 2) +
    geom_text(data = n_sites_focal,
              aes(x = -Inf, y = Inf, label = paste0("n = ", n_sites, " sites")),
              hjust = -0.1, vjust = 1.5,
              size = 3.2, fontface = 3, inherit.aes = FALSE) +
    facet_wrap(~species, scales = "free_y", ncol = 4, labeller = label_parsed) +
    my.theme +
    labs(y = density_y_lab,
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

## one combined figure per basin, drawn to the device and written to
## figs/[basin_name]_avg_density.png
for (focal_basin in sort(unique(as.character(df_basin$basin)))) {
  basin_density_facets <- basin_density_figure(focal_basin)
  print(basin_density_facets)

  basin_fig_file <- file.path(figs_basin, paste0(safe_file_name(focal_basin), "_avg_density.png"))
  ggsave(basin_fig_file, plot = basin_density_facets,
         width = fig_width, height = fig_height, dpi = fig_dpi, bg = "white")
  message("wrote ", basin_fig_file)
}




# -----------------------------------
## basin-level average densities with composite sites

## builds the composite figure for one basin: the gray trajectory of every site
## in that basin behind the blue basin-wide average, one facet per species
basin_density_figure <- function(focal_basin) {

  ## reshape each scale to long format so every species can be one facet
  df_site_focal <- df_site %>%
    filter(basin == focal_basin) %>%
    select(site, year, all_of(species_cols)) %>%
    pivot_longer(all_of(species_cols), names_to = "species", values_to = "density")

  df_basin_focal <- df_basin %>%
    filter(basin == focal_basin) %>%
    select(year, all_of(species_cols)) %>%
    pivot_longer(all_of(species_cols), names_to = "species", values_to = "density")

  ## number of composite sites with at least one non-zero density value, per
  ## species; sorted here so the facets below run from the most-observed species
  ## to the least, with ties broken alphabetically
  n_sites_focal <- df_site_focal %>%
    group_by(species) %>%
    summarise(n_sites = n_distinct(site[!is.na(density) & density != 0]),
              .groups = "drop") %>%
    arrange(desc(n_sites), species)

  ## facet_wrap() lays panels out in factor-level order, so that sort is applied
  ## by making species a factor whose levels are the sorted columns
  ordered_cols <- n_sites_focal$species
  ordered_labels <- facet_labels_for(ordered_cols)
  as_ordered_species <- function(df) {
    mutate(df, species = factor(species, levels = ordered_cols, labels = ordered_labels))
  }

  df_site_focal <- as_ordered_species(df_site_focal)
  df_basin_focal <- as_ordered_species(df_basin_focal)
  n_sites_focal <- as_ordered_species(n_sites_focal)

  ggplot() +
    geom_path(data = df_site_focal,
              aes(x = year, y = density, group = site),
              color = "gray70", linewidth = 0.6, alpha = 0.6) +
    geom_point(data = df_site_focal,
               aes(x = year, y = density, group = site),
               color = "gray70", size = 1, alpha = 0.6) +
    geom_path(data = df_basin_focal,
              aes(x = year, y = density, group = 1),
              color = "#1963b0", linewidth = 1.3) +
    geom_point(data = df_basin_focal,
               aes(x = year, y = density, group = 1),
               color = "#1963b0", size = 2) +
    geom_text(data = n_sites_focal,
              aes(x = -Inf, y = Inf, label = paste0("n = ", n_sites, " sites")),
              hjust = -0.1, vjust = 1.5,
              size = 3.2, fontface = 3, inherit.aes = FALSE) +
    facet_wrap(~species, scales = "free_y", ncol = 4, labeller = label_parsed) +
    my.theme +
    labs(y = density_y_lab,
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

## one combined figure per basin, drawn to the device and written to
## figs/[basin_name]_avg_density.png
for (focal_basin in sort(unique(as.character(df_basin$basin)))) {
  basin_density_facets <- basin_density_figure(focal_basin)
  print(basin_density_facets)

  basin_fig_file <- file.path(figs_basin, paste0(safe_file_name(focal_basin), "_avg_density.png"))
  ggsave(basin_fig_file, plot = basin_density_facets,
         width = fig_width, height = fig_height, dpi = fig_dpi, bg = "white")
  message("wrote ", basin_fig_file)
}




# -----------------------------------
## basin-level average densities by species

## builds the figure for one basin: every algae species' basin average drawn on
## a single set of axes, one colored line each, so the species can be compared
## directly against each other within the basin
basin_species_figure <- function(focal_basin) {

  ## species keep species_cols order (alphabetical), so a given species draws in
  ## the same color and sits in the same legend position in every basin figure
  df_basin_focal <- df_basin %>%
    filter(basin == focal_basin) %>%
    select(year, all_of(species_cols)) %>%
    pivot_longer(all_of(species_cols), names_to = "species", values_to = "density") %>%
    mutate(species = factor(species, levels = species_cols))

  ggplot(df_basin_focal,
         aes(x = year, y = density, group = species, color = species)) +
    geom_path(linewidth = 1.1) +
    geom_point(size = 2.4) +
    scale_color_brewer(palette = "Paired",
                       labels = parse(text = facet_labels_for(species_cols))) +
    my.theme +
    labs(y = density_y_lab,
         title = gsub("_", " ", focal_basin),
         color = NULL) +
    theme(
      axis.title.x = element_blank(),
      plot.title = element_text(hjust = 0.5),
      legend.text = element_text(size = 10),
      legend.key.width = unit(1.4, "lines"),
      legend.position = "right")
}

## one figure per basin, drawn to the device and written to
## figs/[basin_name]_species_avg_density.png -- the "_species" keeps these from
## overwriting the faceted basin figures written above
for (focal_basin in sort(unique(as.character(df_basin$basin)))) {
  basin_species_plot <- basin_species_figure(focal_basin)
  print(basin_species_plot)

  basin_species_file <- file.path(figs_basin_species, paste0(safe_file_name(focal_basin),
                                               "_species_avg_density.png"))
  ggsave(basin_species_file, plot = basin_species_plot,
         width = fig_width, height = fig_height, dpi = fig_dpi, bg = "white")
  message("wrote ", basin_species_file)
}




# -----------------------------------
## species-level average densities with composite sites

## the mirror image of the basin figure above: a focal species across every
## basin, rather than a focal basin across every species. One facet per basin,
## the gray trajectory of every site in that basin behind the blue composite
## average of those sites
species_density_figure <- function(focal_species) {

  ## one column instead of a pivot -- the focal species is the only one drawn,
  ## so basin becomes what varies across the facets
  df_site_focal <- df_site %>%
    select(basin, site, year, density = all_of(focal_species))

  df_basin_focal <- df_basin %>%
    select(basin, year, density = all_of(focal_species))

  ## number of composite sites with at least one non-zero density value, per
  ## basin -- annotated on each facet, but deliberately not used to order them
  n_sites_focal <- df_site_focal %>%
    group_by(basin) %>%
    summarise(n_sites = n_distinct(site[!is.na(density) & density != 0]),
              .groups = "drop")

  ## facet_wrap() lays panels out in factor-level order, so holding the levels
  ## alphabetical keeps the layout identical from one species figure to the next
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

  ggplot() +
    geom_path(data = df_site_focal,
              aes(x = year, y = density, group = site),
              color = "gray70", linewidth = 0.6, alpha = 0.6) +
    geom_point(data = df_site_focal,
               aes(x = year, y = density, group = site),
               color = "gray70", size = 1, alpha = 0.6) +
    geom_path(data = df_basin_focal,
              aes(x = year, y = density, group = 1),
              color = "#1963b0", linewidth = 1.3) +
    geom_point(data = df_basin_focal,
               aes(x = year, y = density, group = 1),
               color = "#1963b0", size = 2) +
    geom_text(data = n_sites_focal,
              aes(x = -Inf, y = Inf, label = paste0("n = ", n_sites, " sites")),
              hjust = -0.1, vjust = 1.5,
              size = 3.2, fontface = 3, inherit.aes = FALSE) +
    facet_wrap(~basin, scales = "free_y", ncol = 4,
               labeller = as_labeller(function(x) gsub("_", " ", x))) +
    my.theme +
    labs(y = density_y_lab,
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

## species ordered by the number of sites sound-wide with observations, so the
## figures are produced most-observed first -- this is the order the files are
## written in, not a layout choice; the panels inside each one stay alphabetical
species_by_n_sites <- df_site %>%
  select(site, all_of(species_cols)) %>%
  pivot_longer(all_of(species_cols), names_to = "species", values_to = "density") %>%
  group_by(species) %>%
  summarise(n_sites = n_distinct(site[!is.na(density) & density != 0]),
            .groups = "drop") %>%
  arrange(desc(n_sites), species)

## one combined figure per species, drawn to the device and written to
## figs/[species_name]_avg_density.png
for (focal_species in species_by_n_sites$species) {
  species_density_facets <- species_density_figure(focal_species)
  print(species_density_facets)

  species_fig_file <- file.path(figs_species, paste0(safe_file_name(species_label(focal_species)),
                                             "_avg_density.png"))
  ggsave(species_fig_file, plot = species_density_facets,
         width = fig_width, height = fig_height, dpi = fig_dpi, bg = "white")
  message("wrote ", species_fig_file)
}





# -----------------------------------
## puget sound-level average densities with composite basins

ps_wide <- "Puget_Sound_wide"

## reshape each scale to long format so every species can be one facet
## (df_basin holds every basin, so no basin-level filter is needed here --
## it's the full composite behind the sound-wide average)
df_basin_all <- df_basin %>%
  select(basin, year, all_of(species_cols)) %>%
  pivot_longer(all_of(species_cols), names_to = "species", values_to = "density")

df_puget_focal <- df_puget %>%
  filter(region == ps_wide) %>%
  select(year, all_of(species_cols)) %>%
  pivot_longer(all_of(species_cols), names_to = "species", values_to = "density")

## number of composite basins with at least one non-zero density value, per
## species; sorted the same way as the basin figures so the facets run from the
## most-observed species to the least -- the sound-wide composite is built from
## basins, so basins are what gets counted here
n_basins_focal <- df_basin_all %>%
  group_by(species) %>%
  summarise(n_basins = n_distinct(basin[!is.na(density) & density != 0]),
            .groups = "drop") %>%
  arrange(desc(n_basins), species)

ordered_cols <- n_basins_focal$species
ordered_labels <- facet_labels_for(ordered_cols)
as_ordered_species <- function(df) {
  mutate(df, species = factor(species, levels = ordered_cols, labels = ordered_labels))
}

df_basin_all <- as_ordered_species(df_basin_all)
df_puget_focal <- as_ordered_species(df_puget_focal)
n_basins_focal <- as_ordered_species(n_basins_focal)

sound_density_facets <- ggplot() +
  geom_path(data = df_basin_all,
            aes(x = year, y = density, group = basin),
            color = "gray70", linewidth = 0.6, alpha = 0.6) +
  geom_point(data = df_basin_all,
             aes(x = year, y = density, group = basin),
             color = "gray70", size = 1, alpha = 0.6) +
  geom_path(data = df_puget_focal,
            aes(x = year, y = density, group = 1),
            color = "#1963b0", linewidth = 1.3) +
  geom_point(data = df_puget_focal,
             aes(x = year, y = density, group = 1),
             color = "#1963b0", size = 2) +
  geom_text(data = n_basins_focal,
            aes(x = -Inf, y = Inf, label = paste0("basin n = ", n_basins)),
            hjust = -0.1, vjust = 1.5,
            size = 3.2, fontface = 3, inherit.aes = FALSE) +
  facet_wrap(~species, scales = "free_y", ncol = 4, labeller = label_parsed) +
  my.theme +
  labs(y = density_y_lab,
       title = "Puget Sound-wide") +
  theme(
    axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
    axis.title.x = element_blank(),
    axis.text.y = element_text(size = 9),
    axis.title.y = element_text(size = 12, hjust = 0.5),
    strip.text = element_text(size = facet_label_size),
    plot.title = element_text(hjust = 0.5),
    legend.position = "none")

print(sound_density_facets)

sound_fig_file <- file.path(figs_puget, paste0(safe_file_name(ps_wide), "_avg_density.png"))
ggsave(sound_fig_file, plot = sound_density_facets,
       width = fig_width, height = fig_height, dpi = fig_dpi, bg = "white")
message("wrote ", sound_fig_file)





