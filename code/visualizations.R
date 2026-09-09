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

graphics.off()
windows(12,6,record=T)

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
facet_label_size <- 13

## builds each facet's title as "common name (scientific name)", bold-italicizing
## the scientific name via a plotmath expression string (parsed by label_parsed);
## omits the parentheses until a scientific name has been filled in above
facet_labels_for <- function(cols) {
  common <- vapply(cols, species_label, character(1))
  sci <- species_sci_names[cols]
  ifelse(sci == "",
         paste0('bold("', common, '")'),
         paste0('bold("', common, '")~"("*bolditalic("', sci, '")*")"'))
}

## shared y-axis label: "Average Density per 60m^2 Transect" with superscript 2
density_y_lab <- expression("Avgerage Density per 60m"^2*" Transect")

# -----------------------------------
## basin-level average densities with composite sites

focal_basin <- "Central_Puget_Sound"

## algae density columns shared by the site- and basin-scale tables
species_cols <- names(df_basin)[grepl("^ak_.*_avg$", names(df_basin))]
species_cols <- setdiff(species_cols, exclude_species)
species_labels <- facet_labels_for(species_cols)

## reshape each scale to long format so every species can be one facet
df_site_focal <- df_site %>%
  filter(basin == focal_basin) %>%
  select(site, year, all_of(species_cols)) %>%
  pivot_longer(all_of(species_cols), names_to = "species", values_to = "density") %>%
  mutate(species = factor(species, levels = species_cols, labels = species_labels))

df_basin_focal <- df_basin %>%
  filter(basin == focal_basin) %>%
  select(year, all_of(species_cols)) %>%
  pivot_longer(all_of(species_cols), names_to = "species", values_to = "density") %>%
  mutate(species = factor(species, levels = species_cols, labels = species_labels))

## number of composite sites with at least one non-zero density value, per species
n_sites_focal <- df_site_focal %>%
  group_by(species) %>%
  summarise(n_sites = n_distinct(site[density != 0]), .groups = "drop")

basin_density_facets <- ggplot() +
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

print(basin_density_facets)



# -----------------------------------
## puget sound-level average densities with composite basins

ps_wide <- "Puget_Sound_wide"

## algae density columns shared by the basin- and sound-scale tables
species_cols <- names(df_puget)[grepl("^ak_.*_avg$", names(df_puget))]
species_cols <- setdiff(species_cols, exclude_species)
species_labels <- facet_labels_for(species_cols)

## reshape each scale to long format so every species can be one facet
## (df_basin holds every basin, so no basin-level filter is needed here --
## it's the full composite behind the sound-wide average)
df_basin_all <- df_basin %>%
  select(basin, year, all_of(species_cols)) %>%
  pivot_longer(all_of(species_cols), names_to = "species", values_to = "density") %>%
  mutate(species = factor(species, levels = species_cols, labels = species_labels))

df_puget_focal <- df_puget %>%
  filter(region == ps_wide) %>%
  select(year, all_of(species_cols)) %>%
  pivot_longer(all_of(species_cols), names_to = "species", values_to = "density") %>%
  mutate(species = factor(species, levels = species_cols, labels = species_labels))

## number of composite basins with at least one non-zero density value, per species
n_basins_focal <- df_basin_all %>%
  group_by(species) %>%
  summarise(n_basins = n_distinct(basin[density != 0]), .groups = "drop")

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