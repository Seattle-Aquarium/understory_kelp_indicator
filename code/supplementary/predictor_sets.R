## How much of the algae community different predictor sets explain
## Reads:  results/dbrda_algae_points.csv, results/metrics_transect_scale.csv,
##         results/reef_check_cleaned.csv
## Writes: results/supplementary/predictor_sets.csv
##
## A check behind the methods, not part of the app: the db-RDA's R^2 (year
## partialled out; the same community and Bray-Curtis distances as
## substrate_algae.R) for substrate alone and with what else the surveys
## measured -- the ten individual classes, transect depth, urchins, canopy kelp
## -- and for basin and site identity. Site identity is the ceiling for
## anything that is constant at a site; what is left is variation among
## transects at the same site.
##
## Two cautions on depth: depth_ft is the recorded transect depth, and 14
## transects record under 3 ft (Wing Point 2021 transect 1 reads 0 ft), worth
## checking at the source before depth goes into a model; and depth explains a
## lot here (13% on its own), so it is the first thing to add if substrate's
## R^2 is to be compared with anything else.
##
## Run from code/supplementary. Under a minute.

rm(list=ls())
suppressPackageStartupMessages({ library(vegan) })
setwd("../../")
results <- "results"
out_res <- file.path(results, "supplementary")
dir.create(out_res, recursive = TRUE, showWarnings = FALSE)

env <- read.csv(file.path(results, "dbrda_algae_points.csv"), check.names = FALSE)
tr  <- read.csv(file.path(results, "metrics_transect_scale.csv"), check.names = FALSE)
cl  <- read.csv(file.path(results, "reef_check_cleaned.csv"), check.names = FALSE)
spp <- grep("^ak_", names(env), value = TRUE)
bc  <- vegdist(wisconsin(sqrt(as.matrix(env[spp]))), "bray")

u <- tr[match(env$key, tr$key), ]
e <- data.frame(year = factor(env$year), basin = factor(env$basin), site = factor(env$site),
                depth_zone = factor(env$depth_zone), hard = env$hard, soft = env$soft,
                relief = env$relief, depth_ft = cl$depth_ft[match(env$key, cl$key)])
## the ten classes, cobble the reference (all ten sum to one); clay was not on
## the datasheet before 2023, so 0 there
cls <- c(rock = "sb_rock", reef = "sb_reef", lboulder = "sb_l_boulder-wa", sboulder = "sb_s_boulder-wa",
         pebble = "sb_pebble-wa", sand = "sb_sand", shell = "sb_shell_hash-wa", clay = "sb_clay-wa",
         other = "sb_other")
for (k in names(cls)) e[[k]] <- ifelse(is.na(u[[cls[[k]]]]), 0, u[[cls[[k]]]])
e$rf_low <- u[["rf_10cm-1m"]]; e$rf_mid <- u[["rf_1m-2m"]]; e$rf_high <- u[["rf_over_2m"]]
e$urchin_red    <- log1p(u$iv_red_urchin)
e$urchin_purple <- log1p(u$iv_purple_urchin)
e$urchin_green  <- log1p(u$iv_green_pallid_urchin)
e$canopy <- log1p(rowSums(u[c("ak_bull_kelp", "ak_giant_kelp_n", "ak_feather_boa_n")], na.rm = TRUE))
e$depth_ft[is.na(e$depth_ft)] <- median(e$depth_ft, na.rm = TRUE)
for (v in c("urchin_red", "urchin_purple", "urchin_green")) e[[v]][is.na(e[[v]])] <- 0

ten <- "rock + reef + lboulder + sboulder + pebble + sand + shell + clay + other"
measured <- paste(ten, "+ rf_low + rf_mid + rf_high + depth_zone + depth_ft + urchin_red + urchin_purple + urchin_green + canopy")
sets <- c(
  "Substrate, grouped (hard, soft, relief): the app's db-RDA" = "hard + soft + relief",
  "Substrate, all 10 classes + mean relief"                   = paste(ten, "+ relief"),
  "Substrate, all 10 classes + 4 relief classes"              = paste(ten, "+ rf_low + rf_mid + rf_high"),
  "Depth alone (zone + feet)"                                 = "depth_zone + depth_ft",
  "Grouped substrate + depth (zone + feet)"                   = "hard + soft + relief + depth_zone + depth_ft",
  "Grouped substrate + urchins (red, purple, green)"          = "hard + soft + relief + urchin_red + urchin_purple + urchin_green",
  "Grouped substrate + canopy kelp"                           = "hard + soft + relief + canopy",
  "Everything measured: classes, relief, depth, urchins, canopy" = measured,
  "Basin alone"                                               = "basin",
  "Everything measured + basin"                               = paste(measured, "+ basin"),
  "Site identity (ceiling for anything fixed at a site)"      = "site")
res <- do.call(rbind, lapply(names(sets), function(nm) {
  m <- dbrda(as.formula(paste("bc ~", sets[[nm]], "+ Condition(year)")), data = e)
  r <- RsquareAdj(m)
  data.frame(predictors = nm, df = m$CCA$qrank, r2_pct = round(100 * r$r.squared, 1),
             adj_r2_pct = round(100 * r$adj.r.squared, 1),
             axis1_pct = round(100 * m$CCA$eig[1] / m$tot.chi, 1),
             axis2_pct = round(100 * m$CCA$eig[2] / m$tot.chi, 1))
}))
print(res, row.names = FALSE)
write.csv(res, file.path(out_res, "predictor_sets.csv"), row.names = FALSE)
cat("Wrote", file.path(out_res, "predictor_sets.csv"), "\n")
