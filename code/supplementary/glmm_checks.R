## Checks on the GLMMs behind the methods write-up
## Reads:  results/metrics_transect_scale.csv, results/reef_check_cleaned.csv
## Writes: results/supplementary/glmm_checks.csv
##
## A check behind the methods, not part of the app. For the two pairs the
## write-up highlights -- sugar kelp with soft substrate, sieve kelp with hard
## substrate -- refits glmm_substrate.R's model and reports:
##   * the between- and within-site rate ratios, and the single-term one;
##   * how a single term weights the two (by precision), and a test that the
##     two slopes are equal;
##   * simulation checks: 500 data sets from the fitted model, compared with
##     the data on the share of zero counts and on the overall spread
##     (randomized quantile residuals tested against uniform);
##   * a zero-inflated version (an extra share of transects with none at all);
##   * sensitivity: transect depth in feet added; basin added as a second
##     random intercept.
##
## Run from code/supplementary. About a minute.

rm(list=ls())
suppressPackageStartupMessages({ library(dplyr); library(glmmTMB) })
setwd("../../")
results <- "results"
source(file.path("code", "substrate_classes.R"))
out_res <- file.path(results, "supplementary")
dir.create(out_res, recursive = TRUE, showWarnings = FALSE)

tr <- read.csv(file.path(results, "metrics_transect_scale.csv"), check.names = FALSE)
cl <- read.csv(file.path(results, "reef_check_cleaned.csv"), check.names = FALSE)

## the GLMMs' data, as glmm_substrate.R builds it
held_out <- c("Northern Coast", "Western Strait of Juan de Fuca", "Eastern Strait of Juan de Fuca")
tr <- tr[!gsub("_", " ", tr$basin) %in% held_out, ]
canopy  <- c("ak_bull_kelp", "ak_giant_kelp_n", "ak_feather_boa_n")
species <- setdiff(grep("^ak_", names(tr), value = TRUE), canopy)
d <- data.frame(key = tr$key, basin = tr$basin, site = tr$site, year = tr$year, depth_zone = tr$depth_zone)
for (g in names(substrate_groups)) d[[g]] <- substrate_share(tr, substrate_groups[[g]])
d$relief <- mean_relief(tr)
d <- cbind(d, cl[match(d$key, cl$key), species])
d <- d[rowSums(!is.na(d[species])) > 0 & !is.na(d$hard) & !is.na(d$relief), ]
d$site <- factor(d$site); d$year <- factor(d$year); d$basin <- factor(d$basin)
d$depth_zone <- factor(d$depth_zone, levels = c("offshore", "inshore"))
## transect depth per 10 ft from 15 ft; one missing value takes the median
d$depth10 <- (cl$depth_ft[match(d$key, cl$key)] - 15) / 10
d$depth10[is.na(d$depth10)] <- median(d$depth10, na.rm = TRUE)
for (p in c("soft", "hard")) {
  m <- ave(d[[p]], d$site, FUN = mean)
  d[[paste0(p, "_x")]] <- d[[p]] / 0.1
  d[[paste0(p, "_between")]] <- m / 0.1
  d[[paste0(p, "_within")]] <- (d[[p]] - m) / 0.1
}

fit <- function(sp, rhs, zi = ~0) {
  dd <- d[!is.na(d[[sp]]), ]; dd$y <- dd[[sp]]
  glmmTMB(as.formula(paste("y ~", rhs)), ziformula = zi, data = dd, family = nbinom2())
}
rr <- function(m, term) {
  co <- summary(m)$coefficients$cond[term, ]
  c(rate_ratio = exp(co[[1]]), lo = exp(co[[1]] - 1.96 * co[[2]]), hi = exp(co[[1]] + 1.96 * co[[2]]), p = co[[4]])
}
## randomized quantile residuals from simulations, tested against uniform
sim_check <- function(m, nsim = 500) {
  set.seed(20261019)
  sims <- as.matrix(simulate(m, nsim = nsim))
  y <- model.response(model.frame(m))
  z_sim <- colMeans(sims == 0)
  u <- rowMeans(sims < y) + runif(length(y)) * rowMeans(sims == y)
  c(zeros_observed = mean(y == 0), zeros_simulated = mean(z_sim),
    zeros_p = mean(abs(z_sim - mean(z_sim)) >= abs(mean(y == 0) - mean(z_sim))),
    uniformity_p = suppressWarnings(ks.test(u, "punif")$p.value))
}

rows <- list()
for (pp in list(c("ak_sugar_kelp", "soft"), c("ak_sieve_kelp", "hard"))) {
  sp <- pp[1]; x <- pp[2]
  nb <- paste0(x, "_between"); nw <- paste0(x, "_within")
  base_rhs <- paste(nb, "+", nw, "+ depth_zone + year + (1 | site)")
  split  <- fit(sp, base_rhs)
  single <- fit(sp, paste0(x, "_x + depth_zone + year + (1 | site)"))
  V <- vcov(split)$cond; b <- fixef(split)$cond
  w_within <- (1 / V[nw, nw]) / (1 / V[nw, nw] + 1 / V[nb, nb])
  z_diff <- (b[nb] - b[nw]) / sqrt(V[nb, nb] + V[nw, nw] - 2 * V[nb, nw])
  zinb  <- fit(sp, base_rhs, zi = ~1)
  depth <- fit(sp, paste(base_rhs, "+ depth10"))
  basin <- fit(sp, paste(base_rhs, "+ (1 | basin)"))
  rows[[sp]] <- data.frame(
    species = sp, predictor = x,
    between = rr(split, nb)[["rate_ratio"]], between_lo = rr(split, nb)[["lo"]],
    between_hi = rr(split, nb)[["hi"]], between_p = rr(split, nb)[["p"]],
    within = rr(split, nw)[["rate_ratio"]], within_lo = rr(split, nw)[["lo"]],
    within_hi = rr(split, nw)[["hi"]], within_p = rr(split, nw)[["p"]],
    single_term = rr(single, paste0(x, "_x"))[["rate_ratio"]],
    weight_on_within = w_within,
    weighted_average = exp(w_within * b[[nw]] + (1 - w_within) * b[[nb]]),
    slopes_differ_p = 2 * pnorm(-abs(z_diff[[1]])),
    t(sim_check(split)),
    zinb_aic_change = AIC(zinb) - AIC(split),
    zinb_between = rr(zinb, nb)[["rate_ratio"]], zinb_between_p = rr(zinb, nb)[["p"]],
    zinb_within = rr(zinb, nw)[["rate_ratio"]], zinb_within_p = rr(zinb, nw)[["p"]],
    depth_between = rr(depth, nb)[["rate_ratio"]], depth_within = rr(depth, nw)[["rate_ratio"]],
    depth_per_10ft = rr(depth, "depth10")[["rate_ratio"]],
    basin_between = rr(basin, nb)[["rate_ratio"]], basin_within = rr(basin, nw)[["rate_ratio"]],
    row.names = NULL)
}
out <- bind_rows(rows)
print(t(out), digits = 3, quote = FALSE)
write.csv(out, file.path(out_res, "glmm_checks.csv"), row.names = FALSE)
cat("\nzero counts by basin, sugar kelp:\n")
print(round(tapply(d$ak_sugar_kelp == 0, d$basin, mean, na.rm = TRUE), 2))
cat("Wrote", file.path(out_res, "glmm_checks.csv"), "\n")
