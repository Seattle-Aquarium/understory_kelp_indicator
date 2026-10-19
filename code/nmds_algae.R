## NMDS of the understory algae community -- one point per transect
## Reads:  results/metrics_transect_scale.csv   (written by metrics_across_scale.R)
## Writes: results/nmds_algae_points.csv    one row per transect: survey metadata,
##                                          its ordination scores and its densities
##         results/nmds_algae_species.csv   each species' score
##         results/nmds_algae_stress.csv    stress by number of dimensions, and
##                                          the settings the ordination used
##
## Modelled on CCR_benthic_analyses/ROV_diver_comparison/code/NMDS.R: metaMDS,
## Bray-Curtis, many random starts run in parallel, the transformation done
## here rather than left to autotransform, and species scores kept for the
## arrows on the figures.
##
## ---- What goes in ------------------------------------------------------------
## Sample unit: the transect (30 x 2 m). Community: the ten understory kelps
## and other algae, as individuals per m^2. The three canopy kelps (bull,
## giant, feather boa) are left out, as they are from every other figure in
## this repository.
##
## Two kinds of transect cannot be placed, and are counted rather than
## silently dropped:
##   * no algae survey on that transect (the row exists for another dataset);
##   * algae surveyed but no understory algae found. Bray-Curtis between two
##     empty transects is 0/0, and an empty transect has no composition to
##     place.
##
## ---- Transformation ------------------------------------------------------------
## Wisconsin double standardization of square-root densities: each species is
## scaled to its own maximum, then each transect to its own total. This is
## what vegan's metaMDS applies to community counts by default, and it makes
## the ordination about composition -- which species, in what proportions --
## rather than total density, which the density and trend analyses already
## cover. Without it, sugar kelp (on 87% of transects, at up to 20 per m^2)
## dominates every distance. Two-dimensional stress was 0.160 with it and
## 0.196 on square-root densities alone (nmds_algae_stress.csv keeps the
## profile for the transformation used).
##
## ---- Dimensions ------------------------------------------------------------------
## The figures use the two-dimensional solution. A three-dimensional solution
## is computed too and its scores stored, so the stress profile can be judged
## and a 3-D view added without re-running anything. Stress by k is written
## for k = 1-4 (Clarke 1993: < 0.1 good, < 0.2 usable, > 0.3 arbitrary).

rm(list=ls())
suppressPackageStartupMessages({
  library(dplyr)
  library(vegan)
})

## relative file paths
results <- "results"

## set working directory to home folder
setwd("../")

## ---- FILE LOCATIONS AND SETTINGS --------------------------------------------
## Run on its own it reads and writes the repository's results folder. The
## Shiny app in gui/ sets these environment variables first; see
## rc_full_data_clean.R for why environment variables rather than options().
from_env <- function(var, default) {
  value <- Sys.getenv(var, unset = "")
  if (nzchar(value)) value else default
}

transect_csv <- from_env("UKELP_TRANSECT_CSV",     file.path(results, "metrics_transect_scale.csv"))
points_csv   <- from_env("UKELP_NMDS_POINTS_CSV",  file.path(results, "nmds_algae_points.csv"))
species_csv  <- from_env("UKELP_NMDS_SPECIES_CSV", file.path(results, "nmds_algae_species.csv"))
stress_csv   <- from_env("UKELP_NMDS_STRESS_CSV",  file.path(results, "nmds_algae_stress.csv"))

## random starts: the 2-D solution the figures use gets the most (and a second
## round if its best was not repeated); the stored 3-D solution and the other
## points of the stress profile get fewer. A fixed seed, so a re-run reproduces
## the same ordination.
trymax_main    <- 100
trymax_3d      <- 50
trymax_profile <- 30
seed           <- 20261019
cores          <- max(1, parallel::detectCores() - 2)
transformation <- "wisconsin(sqrt(density))"

for (d in unique(dirname(c(points_csv, species_csv, stress_csv)))) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

df_tr <- read.csv(transect_csv, check.names = FALSE)

## ---- EXCLUDED BASINS -------------------------------------------------------
## Held out unless UKELP_INCLUDE_BASINS names them -- the same per-basin switch
## every other script honours; the transect table is normally built without them.
held_out_basins <- c("Northern Coast",
                     "Western Strait of Juan de Fuca",
                     "Eastern Strait of Juan de Fuca")
include_basins <- trimws(strsplit(Sys.getenv("UKELP_INCLUDE_BASINS", unset = ""),
                                  ",", fixed = TRUE)[[1]])
include_basins <- intersect(held_out_basins, include_basins[nzchar(include_basins)])
excluded_basins <- setdiff(held_out_basins, include_basins)
df_tr <- df_tr %>% filter(!gsub("_", " ", basin) %in% excluded_basins)

## ============================================================
## The community matrix
## ============================================================

canopy <- c("ak_bull_kelp", "ak_giant_kelp_n", "ak_feather_boa_n")
species <- setdiff(grep("^ak_", names(df_tr), value = TRUE), canopy)

metadata_columns <- c("key", "basin", "basin_id", "site", "site_id", "year",
                      "transect", "depth_zone")

surveyed <- rowSums(!is.na(df_tr[species])) > 0
empty    <- surveyed & rowSums(df_tr[species], na.rm = TRUE) == 0
use      <- surveyed & !empty

community <- as.matrix(df_tr[use, species])
community[is.na(community)] <- 0
x <- wisconsin(sqrt(community))

cat(sprintf("NMDS of %d transects x %d species (%d without understory algae and %d without an algae survey left out)\n",
            sum(use), length(species), sum(empty), sum(!surveyed)))

## ============================================================
## Reuse the last run if nothing it depends on has changed
## ============================================================
## metaMDS with 100 random starts on ~900 transects takes a few minutes, and
## step 2 is re-run far more often than the transects change, so the inputs
## and settings are fingerprinted and a matching earlier run is kept.
fingerprint <- local({
  f <- tempfile(fileext = ".txt")
  on.exit(unlink(f))
  writeLines(c(transformation, seed, trymax_main, trymax_3d, trymax_profile,
               paste(df_tr$key[use], collapse = ","),
               format(as.vector(community), digits = 15)), f)
  unname(tools::md5sum(f))
})

previous <- if (file.exists(stress_csv)) tryCatch(read.csv(stress_csv), error = function(e) NULL)
reuse <- !is.null(previous) && "fingerprint" %in% names(previous) &&
  identical(as.character(previous$fingerprint[1]), fingerprint) &&
  file.exists(points_csv) && file.exists(species_csv) &&
  !identical(Sys.getenv("UKELP_NMDS_FORCE"), "1")

if (reuse) {
  cat("The transect table and settings are unchanged since the last NMDS, so it is reused:\n")
  cat("  ", points_csv, "\n  ", species_csv, "\n  ", stress_csv, "\n")
  quit(save = "no", status = 0)
}

## ============================================================
## Ordinate
## ============================================================

## the best of `trymax` random starts; with `retry`, if the best was not
## reached twice, another round starting from it, as metaMDS's documentation
## recommends. With ~900 transects the best is seldom repeated exactly, so the
## second round mostly confirms it -- worth it only for the solution drawn.
ordinate <- function(k, trymax, retry = FALSE) {
  set.seed(seed + k)
  ord <- metaMDS(x, distance = "bray", k = k, trymax = trymax, autotransform = FALSE,
                 wascores = TRUE, parallel = cores, trace = 0)
  if (retry && !ord$converged) {
    ord <- metaMDS(x, distance = "bray", k = k, trymax = trymax, autotransform = FALSE,
                   wascores = TRUE, parallel = cores, trace = 0, previous.best = ord)
  }
  ord
}

cat(sprintf("Running metaMDS on %d cores; the first run takes a few minutes.\n", cores))
t0 <- Sys.time()
ord2 <- ordinate(2, trymax_main, retry = TRUE)
cat(sprintf("  2-D: stress %.3f%s\n", ord2$stress,
            if (ord2$converged) "" else " (best solution not repeated -- see nmds_algae_stress.csv)"))
ord3 <- ordinate(3, trymax_3d)
cat(sprintf("  3-D: stress %.3f\n", ord3$stress))
profile <- lapply(c(1, 4), function(k) ordinate(k, trymax_profile))
cat(sprintf("  done in %.1f min\n", as.numeric(difftime(Sys.time(), t0, units = "mins"))))

## ============================================================
## Write
## ============================================================

scores_of <- function(ord, prefix, display) {
  s <- as.data.frame(scores(ord, display = display))
  names(s) <- paste0(prefix, seq_len(ncol(s)))
  s
}

points <- df_tr[use, metadata_columns] %>%
  bind_cols(scores_of(ord2, "NMDS", "sites")) %>%
  bind_cols(scores_of(ord3, "NMDS3D_", "sites")) %>%
  bind_cols(as.data.frame(community)) %>%
  mutate(total_density = rowSums(community),
         n_species = rowSums(community > 0))

species_scores <- data.frame(category = species) %>%
  bind_cols(scores_of(ord2, "NMDS", "species")) %>%
  bind_cols(scores_of(ord3, "NMDS3D_", "species"))

all_ord <- list(profile[[1]], ord2, ord3, profile[[2]])
stress <- data.frame(
  k = c(1, 2, 3, 4),
  stress = vapply(all_ord, function(o) o$stress, numeric(1)),
  converged = vapply(all_ord, function(o) isTRUE(o$converged), logical(1)),
  tries = vapply(all_ord, function(o) as.integer(o$tries), integer(1)),
  role = c("stress profile", "figures (2-D)", "3-D scores stored", "stress profile"),
  distance = "bray",
  transformation = transformation,
  n_transects = sum(use),
  n_no_understory_algae = sum(empty),
  n_no_algae_survey = sum(!surveyed),
  seed = seed,
  fingerprint = fingerprint)

write.csv(points, points_csv, row.names = FALSE)
write.csv(species_scores, species_csv, row.names = FALSE)
write.csv(stress, stress_csv, row.names = FALSE)
cat("Wrote", points_csv, "--", nrow(points), "transects\n")
cat("Wrote", species_csv, "\n")
cat("Wrote", stress_csv, "\n")
cat(sprintf("Stress by dimensions: %s\n",
            paste0("k=", stress$k, " ", sprintf("%.3f", stress$stress), collapse = ", ")))
