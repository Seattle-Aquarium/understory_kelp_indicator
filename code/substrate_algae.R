## Substrate and the understory algae community
## Reads:  results/metrics_transect_scale.csv   (metrics_across_scale.R)
##         results/nmds_algae_points.csv        (nmds_algae.R)
## Writes: results/nmds_algae_envfit.csv     substrate vectors fitted onto the NMDS:
##                                           the groups, and the eight classes one by one
##         results/dbrda_algae_points.csv    each transect's db-RDA scores
##         results/dbrda_algae_species.csv   species scores on the db-RDA axes
##         results/dbrda_algae_terms.csv     each substrate term: arrow and tests
##         results/dbrda_algae_summary.csv   variation explained, tests, settings
##         results/dbrda_algae_varpart.csv   variation partitioning
##
## The algae are the response; substrate and relief, from the UPC on the same
## transects, are the predictors. The community is the one the NMDS ordinates
## -- the ten understory algae, Wisconsin double-standardized square-root
## densities, Bray-Curtis -- so the two analyses describe the same thing.
##
## ---- Predictors --------------------------------------------------------------
## The ten UPC substrate classes are shares of the transect's points and sum to
## one, so they are collapsed into three gradients, and one of the three is
## left out of every model as the reference (with all three in, the variance
## inflation factors run to 160-230; with two, under 3). The groups are
## defined once, in substrate_classes.R, which the GLMMs use too:
##   hard   rock, small and large boulder, reef        (and legacy "boulder")
##   mixed  pebble, cobble                             (and legacy "cobble") -- the reference
##   soft   sand, clay, shell hash
## "Other" substrate averages 0.3% of points and is left out. Clay was only on
## the datasheet from 2023; counting it with sand as "soft" keeps that gradient
## the same before and after. Relief enters as one number, the mean relief of
## the transect's points in metres (class midpoints 0.05, 0.55, 1.5, 2.5 m).
##
## ---- Which questions, which tests ---------------------------------------------
## Transects at the same site are not independent of one another, so p-values
## from shuffling transects freely would overstate the evidence. Instead:
##   * the figure is a transect-level db-RDA with year partialled out --
##     described by its R^2, not tested;
##   * between sites: db-RDA of the site means (every transect and year),
##     sites permuted freely -- does a site's substrate predict its algae?
##   * within sites: the transect-level db-RDA with site and year partialled
##     out, transects permuted only within their own site -- at the same site,
##     do transects with more hard (or soft) substrate carry different algae?
##   * variation partitioning splits what substrate explains on its own from
##     what basin and year explain, and from what it shares with basin (basins
##     differ in substrate, so much of each is also the other).
## The substrate vectors on the NMDS (envfit) are tested both ways too: site
## means on site centroids, and transects permuted within their site.

rm(list=ls())
suppressPackageStartupMessages({
  library(dplyr)
  library(vegan)
})

## relative file paths
results <- "results"

## set working directory to home folder
setwd("../")

## the substrate groups and relief, shared with glmm_substrate.R
source(file.path("code", "substrate_classes.R"))

## ---- FILE LOCATIONS AND SETTINGS --------------------------------------------
## Run on its own it reads and writes the repository's results folder. The
## Shiny app in gui/ sets these environment variables first; see
## rc_full_data_clean.R for why environment variables rather than options().
from_env <- function(var, default) {
  value <- Sys.getenv(var, unset = "")
  if (nzchar(value)) value else default
}

transect_csv <- from_env("UKELP_TRANSECT_CSV",       file.path(results, "metrics_transect_scale.csv"))
nmds_csv     <- from_env("UKELP_NMDS_POINTS_CSV",    file.path(results, "nmds_algae_points.csv"))
envfit_csv   <- from_env("UKELP_NMDS_ENVFIT_CSV",    file.path(results, "nmds_algae_envfit.csv"))
points_csv   <- from_env("UKELP_DBRDA_POINTS_CSV",   file.path(results, "dbrda_algae_points.csv"))
species_csv  <- from_env("UKELP_DBRDA_SPECIES_CSV",  file.path(results, "dbrda_algae_species.csv"))
terms_csv    <- from_env("UKELP_DBRDA_TERMS_CSV",    file.path(results, "dbrda_algae_terms.csv"))
summary_csv  <- from_env("UKELP_DBRDA_SUMMARY_CSV",  file.path(results, "dbrda_algae_summary.csv"))
varpart_csv  <- from_env("UKELP_DBRDA_VARPART_CSV",  file.path(results, "dbrda_algae_varpart.csv"))

## permutations for every test, and a fixed seed so a re-run gives the same p
n_perm <- 999
seed   <- 20261019
cores  <- max(1, parallel::detectCores() - 2)

## The figure's scores. Axis 1 carries almost all the variation substrate
## explains (17.7% of the total in the 2021-2025 data, axis 2 1%), so vegan's
## scaling 1 -- sites scaled by each axis's share, distances between transects
## kept -- rather than its default scaling 2, in which the transects'
## weighted-average scores on the weak second axis are mostly noise and
## stretch the cloud to 2.5 times its width. The site scores are those
## weighted averages: where each transect's algae actually place it, not where
## its substrate predicts.
scaling <- 1

outputs <- c(envfit_csv, points_csv, species_csv, terms_csv, summary_csv, varpart_csv)
for (d in unique(dirname(outputs))) dir.create(d, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(nmds_csv)) stop("No NMDS at ", nmds_csv, " -- nmds_algae.R runs first in step 2.")
df_tr <- read.csv(transect_csv, check.names = FALSE)
pts   <- read.csv(nmds_csv, check.names = FALSE)

## ============================================================
## Substrate gradients, per transect
## ============================================================

predictor_labels <- c(hard = "hard substrate", mixed = "mixed substrate",
                      soft = "soft substrate", relief = "mean relief")

## the NMDS's transects, in its order, with their UPC
upc <- df_tr[match(pts$key, df_tr$key), , drop = FALSE]

env <- data.frame(
  key = pts$key, basin = pts$basin, site = pts$site,
  year = factor(pts$year), depth_zone = pts$depth_zone,
  hard   = substrate_share(upc, substrate_groups$hard),
  mixed  = substrate_share(upc, substrate_groups$mixed),
  soft   = substrate_share(upc, substrate_groups$soft),
  relief = mean_relief(upc))

## the individual UPC classes too, for the NMDS's class-by-class vectors.
## envfit fits each one on its own, so their shares summing to one does not
## get in the way, as it would in the db-RDA.
classes <- names(substrate_individual)
for (k in classes) env[[k]] <- upc[[substrate_individual[[k]]]]
class_labels <- c(setNames(classes, classes), relief = "mean relief")

## ============================================================
## The community -- the NMDS's transects and transformation
## ============================================================

canopy  <- c("ak_bull_kelp", "ak_giant_kelp_n", "ak_feather_boa_n")
species <- setdiff(grep("^ak_", names(pts), value = TRUE), canopy)

## the Wisconsin standardization is done on every NMDS transect, as the NMDS
## did, and then those without a UPC are dropped -- so two transects are the
## same Bray-Curtis distance apart here as in the NMDS
use <- complete.cases(env[c("hard", "mixed", "soft", "relief")])
x   <- wisconsin(sqrt(as.matrix(pts[species])))[use, , drop = FALSE]
community <- as.matrix(pts[use, species])
env <- env[use, , drop = FALSE]
bc  <- vegdist(x, "bray")
cat(sprintf("Substrate and algae: %d transects at %d sites (%d NMDS transects without a UPC survey left out)\n",
            nrow(env), length(unique(env$site)), sum(!use)))

## ============================================================
## Reuse the last run if nothing it depends on has changed
## ============================================================
## The permutation tests take about two minutes; step 2 is re-run far more
## often than the transects change.
fingerprint <- local({
  f <- tempfile(fileext = ".txt")
  on.exit(unlink(f))
  writeLines(c("v2", n_perm, seed, scaling, paste(env$key, collapse = ","),
               format(as.vector(community), digits = 15),
               format(unlist(env[c("hard", "mixed", "soft", "relief")]), digits = 15),
               format(unlist(env[classes]), digits = 15),
               format(as.vector(as.matrix(pts[use, c("NMDS1", "NMDS2")])), digits = 15)), f)
  unname(tools::md5sum(f))
})
previous <- if (file.exists(summary_csv)) tryCatch(read.csv(summary_csv), error = function(e) NULL)
if (!is.null(previous) && "fingerprint" %in% names(previous) &&
    identical(as.character(previous$fingerprint[1]), fingerprint) &&
    all(file.exists(outputs)) && !identical(Sys.getenv("UKELP_NMDS_FORCE"), "1")) {
  cat("The transects, substrate and NMDS are unchanged since the last run, so it is reused:\n")
  cat(paste0("   ", outputs, "\n"), sep = "")
  quit(save = "no", status = 0)
}

## The permutations run on a cluster. Its workers are fresh R processes, so
## they are given this R's library first -- the app's private library is not
## somewhere R looks by default.
cat(sprintf("Permutation tests (%d each) on %d cores.\n", n_perm, cores))
t0 <- Sys.time()
cl <- parallel::makeCluster(cores)
invisible(parallel::clusterCall(cl, function(p) { .libPaths(p); invisible(NULL) }, .libPaths()))
invisible(parallel::clusterEvalQ(cl, suppressPackageStartupMessages(library(vegan))))

within_sites <- function(sites) how(blocks = factor(sites), nperm = n_perm)
freely <- how(nperm = n_perm)

## site means: of the densities (then transformed as above), of the
## predictors, and of the NMDS scores (the site's centroid)
site_order <- sort(unique(env$site))
site_mean  <- function(m) {
  s <- rowsum(as.matrix(m), env$site)[site_order, , drop = FALSE]
  s / as.vector(table(env$site)[site_order])
}
site_env  <- as.data.frame(site_mean(env[c("hard", "mixed", "soft", "relief")]))
site_comm <- site_mean(community)
site_nmds <- site_mean(pts[use, c("NMDS1", "NMDS2")])

## ============================================================
## Substrate vectors on the NMDS (envfit)
## ============================================================
## r2 is across transects; the vector's length on the figure is sqrt(r2).

set.seed(seed)
ef <- envfit(as.matrix(pts[use, c("NMDS1", "NMDS2")]), env[c("hard", "mixed", "soft", "relief")],
             permutations = within_sites(env$site))
set.seed(seed)
ef_sites <- envfit(site_nmds, site_env, permutations = freely)

## the eight individual classes and mean relief, tested the same two ways
class_vars <- c(classes, "relief")
set.seed(seed)
ef_cls <- envfit(as.matrix(pts[use, c("NMDS1", "NMDS2")]), env[class_vars],
                 permutations = within_sites(env$site), na.rm = TRUE)
site_cls <- as.data.frame(site_mean(env[class_vars]))
names(site_cls) <- class_vars
set.seed(seed)
ef_cls_sites <- envfit(site_nmds, site_cls, permutations = freely, na.rm = TRUE)

vector_table <- function(fit, fit_sites, set, labels) {
  v <- scores(fit, "vectors")
  data.frame(set = set, variable = rownames(v), label = unname(labels[rownames(v)]),
             NMDS1 = v[, 1], NMDS2 = v[, 2],
             r2 = unname(fit$vectors$r),
             p_within_sites = unname(fit$vectors$pvals),
             r2_between_sites = unname(fit_sites$vectors$r[rownames(v)]),
             p_between_sites = unname(fit_sites$vectors$pvals[rownames(v)]),
             permutations = n_perm, row.names = NULL)
}
envfit_out <- rbind(vector_table(ef, ef_sites, "grouped", predictor_labels),
                    vector_table(ef_cls, ef_cls_sites, "individual", class_labels))

## ============================================================
## The db-RDA drawn: transects, year partialled out
## ============================================================

predictors <- c("hard", "soft", "relief")
model <- dbrda(bc ~ hard + soft + relief + Condition(year), data = env)
sppscores(model) <- x
r2 <- RsquareAdj(model)
axis_share <- eigenvals(model, model = "constrained") / model$tot.chi

sc <- scores(model, display = c("sites", "species", "bp"), choices = 1:2, scaling = scaling)
points_out <- pts[use, c("key", "basin", "basin_id", "site", "site_id", "year", "transect",
                         "depth_zone", species, "total_density", "n_species")] %>%
  bind_cols(setNames(as.data.frame(sc$sites), c("dbRDA1", "dbRDA2"))) %>%
  bind_cols(env[c("hard", "mixed", "soft", "relief")])
species_out <- data.frame(category = species,
                          setNames(as.data.frame(sc$species), c("dbRDA1", "dbRDA2")))

## ============================================================
## Between sites: site means, sites permuted freely
## ============================================================

site_bc <- vegdist(wisconsin(sqrt(site_comm)), "bray")
between <- dbrda(site_bc ~ hard + soft + relief, data = site_env)
set.seed(seed)
between_terms  <- anova(between, by = "margin", permutations = freely, parallel = cl)
set.seed(seed)
between_global <- anova(between, permutations = freely, parallel = cl)

## ============================================================
## Within sites: site and year partialled out, transects permuted within site
## ============================================================
## The slowest part (most of the run): 999 permutations of ~900 transects, with
## site and year partialled out, for each term in turn.

within <- dbrda(bc ~ hard + soft + relief + Condition(site + year), data = env)
## R^2 above is a share of all the variation, most of which (site and year)
## was partialled out; this is the share of what is left -- the variation among
## transects at the same site in the same year
within_share <- within$CCA$tot.chi / (within$tot.chi - within$pCCA$tot.chi)
set.seed(seed)
within_terms  <- anova(within, by = "margin", permutations = within_sites(env$site), parallel = cl)
set.seed(seed)
within_global <- anova(within, permutations = within_sites(env$site), parallel = cl)

parallel::stopCluster(cl)
cat(sprintf("  done in %.0f s\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))

## ============================================================
## Variation partitioning: substrate, basin, year
## ============================================================
## varpart()'s fractions for three tables, in its order: [a] [b] [c] each
## alone, [d] X1 and X2, [e] X2 and X3, [f] X1 and X3, [g] all three, [h] the
## residual -- here X1 substrate, X2 basin, X3 year.

vp <- varpart(bc, ~ hard + soft + relief, ~ basin, ~ year, data = env)
varpart_out <- data.frame(
  fraction = c("substrate alone", "basin alone", "year alone",
               "substrate and basin together", "basin and year together",
               "substrate and year together", "all three together", "unexplained"),
  adj_r2 = vp$part$indfract$Adj.R.square)

## ============================================================
## Write
## ============================================================

terms_out <- data.frame(
  variable = predictors, label = unname(predictor_labels[predictors]),
  dbRDA1 = sc$biplot[predictors, 1], dbRDA2 = sc$biplot[predictors, 2],
  vif = unname(vif.cca(model)[predictors]),
  F_between_sites = between_terms[predictors, "F"],
  p_between_sites = between_terms[predictors, "Pr(>F)"],
  F_within_sites  = within_terms[predictors, "F"],
  p_within_sites  = within_terms[predictors, "Pr(>F)"])

summary_out <- data.frame(
  n_transects = nrow(env), n_sites = length(site_order), n_no_upc = sum(!use),
  r2 = r2$r.squared, adj_r2 = r2$adj.r.squared,
  axis1_share = unname(axis_share[1]), axis2_share = unname(axis_share[2]),
  between_r2 = RsquareAdj(between)$r.squared,
  between_adj_r2 = RsquareAdj(between)$adj.r.squared,
  between_F = between_global$F[1], between_p = between_global$`Pr(>F)`[1],
  within_r2 = RsquareAdj(within)$r.squared,
  within_adj_r2 = RsquareAdj(within)$adj.r.squared,
  within_share_of_within_site = within_share,
  within_F = within_global$F[1], within_p = within_global$`Pr(>F)`[1],
  permutations = n_perm, scaling = scaling,
  model = "dbrda(Bray-Curtis ~ hard + soft + relief + Condition(year))",
  transformation = "wisconsin(sqrt(density))",
  seed = seed, fingerprint = fingerprint)

write.csv(envfit_out,  envfit_csv,  row.names = FALSE)
write.csv(points_out,  points_csv,  row.names = FALSE)
write.csv(species_out, species_csv, row.names = FALSE)
write.csv(terms_out,   terms_csv,   row.names = FALSE)
write.csv(varpart_out, varpart_csv, row.names = FALSE)
write.csv(summary_out, summary_csv, row.names = FALSE)

cat(sprintf("db-RDA: substrate explains %.1f%% of the algae community's variation (adjusted %.1f%%), year partialled out\n",
            100 * r2$r.squared, 100 * r2$adj.r.squared))
cat(sprintf("  between sites (%d site means): adjusted R2 %.3f, p = %.3f\n",
            summary_out$n_sites, summary_out$between_adj_r2, summary_out$between_p))
cat(sprintf("  within sites: %.1f%% of the variation among a site's transects, p = %.3f\n",
            100 * within_share, summary_out$within_p))
for (i in seq_len(nrow(terms_out))) {
  cat(sprintf("  %-16s between sites p = %.3f   within sites p = %.3f\n", terms_out$label[i],
              terms_out$p_between_sites[i], terms_out$p_within_sites[i]))
}
cat("NMDS substrate vectors:\n")
for (i in seq_len(nrow(envfit_out))) {
  cat(sprintf("  %-16s r2 %.2f   between sites p = %.3f   within sites p = %.3f\n", envfit_out$label[i],
              envfit_out$r2[i], envfit_out$p_between_sites[i], envfit_out$p_within_sites[i]))
}
cat("Wrote", paste(basename(outputs), collapse = ", "), "\n")
