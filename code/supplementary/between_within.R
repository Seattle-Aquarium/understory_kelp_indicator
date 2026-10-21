## Between sites and within sites: what the GLMMs' two slopes are, and why the
## between-site ones run so much larger
## Reads:  results/metrics_transect_scale.csv, results/reef_check_cleaned.csv,
##         results/glmm_algae_terms.csv (glmm_substrate.R),
##         data/Reef_Check/UPC_Washington_raw.csv (UPC points per transect)
## Writes: figs/supplementary/between_within.png
##         results/supplementary/between_within_checks.csv
##
## A check behind the methods, not part of the app.
##
## The figure: each site's own (within-site) slope, drawn as a short line at
## the level the model estimates for that site, against the between-site slope
## across sites -- for sugar kelp and soft substrate, and sieve kelp and hard
## substrate. A single substrate coefficient would follow the short lines.
##
## The checks, for the larger between-site ratios:
##   1. step size: the spread (SD) of site means against that of within-site
##      departures, and the rate ratios per SD rather than per +10 points;
##   2. UPC sampling noise: with ~30 points a transect's share is a noisy
##      estimate, which flattens a within-site slope by its reliability --
##      the share of within-site variance that is not sampling noise;
##   3. sieve kelp across sites, by the site's share of hard substrate.
##
## Run from code/supplementary. About a minute.

rm(list=ls())
suppressPackageStartupMessages({ library(dplyr); library(ggplot2); library(glmmTMB) })
setwd("../../")
results <- "results"
source(file.path("code", "substrate_classes.R"))
out_res <- file.path(results, "supplementary"); out_fig <- file.path("figs", "supplementary")
dir.create(out_res, recursive = TRUE, showWarnings = FALSE)
dir.create(out_fig, recursive = TRUE, showWarnings = FALSE)

tr <- read.csv(file.path(results, "metrics_transect_scale.csv"), check.names = FALSE)
cl <- read.csv(file.path(results, "reef_check_cleaned.csv"), check.names = FALSE)
terms <- read.csv(file.path(results, "glmm_algae_terms.csv"))

## the GLMMs' data, as glmm_substrate.R builds it
held_out <- c("Northern Coast", "Western Strait of Juan de Fuca", "Eastern Strait of Juan de Fuca")
tr <- tr[!gsub("_", " ", tr$basin) %in% held_out, ]
canopy  <- c("ak_bull_kelp", "ak_giant_kelp_n", "ak_feather_boa_n")
species <- setdiff(grep("^ak_", names(tr), value = TRUE), canopy)
d <- data.frame(key = tr$key, basin = tr$basin, site = tr$site, year = tr$year, depth_ft = tr$depth_ft)
for (g in names(substrate_groups)) d[[g]] <- substrate_share(tr, substrate_groups[[g]])
d$relief <- mean_relief(tr)
for (nm in names(substrate_individual)) d[[nm]] <- tr[[substrate_individual[[nm]]]]
d <- cbind(d, cl[match(d$key, cl$key), species])
d <- d[rowSums(!is.na(d[species])) > 0 & !is.na(d$hard) & !is.na(d$relief) & !is.na(d$depth_ft), ]
d$site <- factor(d$site); d$year <- factor(d$year)
d$depth <- (d$depth_ft - 15) / 10   # per 10 ft from 15 ft, as in glmm_substrate.R
for (p in c("soft", "hard")) {
  m <- ave(d[[p]], d$site, FUN = mean)
  d[[paste0(p, "_between")]] <- m / 0.1; d[[paste0(p, "_within")]] <- (d[[p]] - m) / 0.1
}

## ---- the figure ------------------------------------------------------------------
dash_panel <- function(sp, x, title, xlab, col) {
  dd <- d[!is.na(d[[sp]]), ]; dd$y <- dd[[sp]]
  fit <- glmmTMB(as.formula(sprintf("y ~ %s_between + %s_within + depth + year + (1 | site)", x, x)),
                 data = dd, family = nbinom2())
  b <- fixef(fit)$cond; nb <- paste0(x, "_between"); nw <- paste0(x, "_within")
  ## a typical transect: the depths and years of the transects in the years
  ## the species was recorded
  yrs <- names(which(tapply(dd$y, dd$year, sum) > 0))
  d2 <- dd[dd$year %in% yrs, ]
  yv <- b[paste0("year", d2$year)]; yv[is.na(yv)] <- 0
  dv <- b["depth"] * d2$depth
  base <- b["(Intercept)"] + mean(yv + dv)
  u <- ranef(fit)$cond$site
  sites <- dd %>% group_by(site) %>%
    summarise(xbar = mean(.data[[x]]), lo = min(.data[[x]]), hi = max(.data[[x]]), .groups = "drop") %>%
    mutate(u = u[as.character(site), "(Intercept)"], eta = base + b[nb] * xbar / 0.1 + u)
  dashes <- do.call(rbind, lapply(seq_len(nrow(sites)), function(i) {
    s <- sites[i, ]; xs <- seq(s$lo, s$hi, length.out = 20)
    data.frame(site = s$site, share = xs, y = exp(s$eta + b[nw] * (xs - s$xbar) / 0.1) / 60)
  }))
  across <- data.frame(share = seq(min(sites$xbar), max(sites$xbar), length.out = 60))
  across$y <- exp(base + b[nb] * across$share / 0.1) / 60
  pts <- data.frame(share = dd[[x]], y = dd$y / 60)
  ggplot() +
    geom_point(data = pts, aes(100 * share, y), color = "gray72", size = 0.8, alpha = 0.5, shape = 16) +
    geom_line(data = across, aes(100 * share, y), color = "black", linewidth = 1.1, linetype = "22") +
    geom_line(data = dashes, aes(100 * share, y, group = site), color = col, linewidth = 0.9) +
    geom_point(data = sites, aes(100 * xbar, exp(eta) / 60), color = col, size = 1.8) +
    scale_y_continuous(trans = scales::pseudo_log_trans(sigma = 0.02, base = 10),
                       breaks = c(0, 0.03, 0.1, 0.3, 1, 3, 10, 30)) +
    coord_cartesian(ylim = c(0, max(pts$y) * 1.1)) +
    labs(title = title,
         subtitle = sprintf("Across sites (dashed): %.2f\u00d7 per +10 points.  Within each site (colored): %.2f\u00d7.",
                            exp(b[nb]), exp(b[nw])),
         x = xlab, y = "per m\u00b2 (transects in gray)") +
    theme_bw(base_size = 10) + theme(panel.grid.minor = element_blank())
}
p1 <- dash_panel("ak_sugar_kelp", "soft", "Sugar kelp and soft substrate",
                 "soft substrate (% of the transect's UPC points)", "#B8860B")
p2 <- dash_panel("ak_sieve_kelp", "hard", "Sieve kelp and hard substrate",
                 "hard substrate (% of the transect's UPC points)", "#2166AC")
fig <- patchwork::wrap_plots(p1, p2) + patchwork::plot_annotation(
  title = "Two questions, two slopes",
  subtitle = paste("Each colored line is one site: drawn across the range of its own transects, at the level the model estimates for that site,",
                   "with the within-site slope.\nThe dashed line runs across sites: the expected density at a typical site with that average share.",
                   "Gray: the transects themselves."),
  theme = theme(plot.title = element_text(face = "bold"), plot.subtitle = element_text(size = 9, color = "gray30")))
ggsave(file.path(out_fig, "between_within.png"), fig, width = 12, height = 5.6, dpi = 200, bg = "white")

## ---- the checks --------------------------------------------------------------------
preds <- c("hard", "mixed", "soft", "relief", names(substrate_individual))
spread <- do.call(rbind, lapply(preds, function(p) {
  scale_ <- if (p == "relief") 1 else 100
  b <- tapply(d[[p]], d$site, mean, na.rm = TRUE)
  w <- d[[p]] - ave(d[[p]], d$site, FUN = function(z) mean(z, na.rm = TRUE))
  data.frame(predictor = p, sd_between = sd(b, na.rm = TRUE) * scale_, sd_within = sd(w, na.rm = TRUE) * scale_)
}))

raw <- read.csv(file.path("data", "Reef_Check", "UPC_Washington_raw.csv"))
n_points <- median(raw$Total_Amount[raw$Category == "UPC Substrate"])
spread$reliability_within <- sapply(preds, function(p) {
  if (p == "relief") return(NA_real_)
  x <- d[[p]]; w <- x - ave(x, d$site, FUN = function(z) mean(z, na.rm = TRUE))
  1 - mean(x * (1 - x), na.rm = TRUE) / n_points / var(w, na.rm = TRUE)
})

split_terms <- terms[terms$form == "split", ]
per_sd <- split_terms %>% inner_join(spread, by = "predictor") %>%
  mutate(step = ifelse(predictor == "relief", 0.25, 10),
         sd_units = ifelse(term == "between", sd_between, sd_within) / step,
         rate_ratio_per_sd = rate_ratio^sd_units) %>%
  select(species, predictor, term, rate_ratio, rate_ratio_per_sd, q)
out <- per_sd %>% tidyr::pivot_wider(names_from = term, values_from = c(rate_ratio, rate_ratio_per_sd, q)) %>%
  left_join(spread, by = "predictor")
write.csv(out, file.path(out_res, "between_within_checks.csv"), row.names = FALSE)

cat(sprintf("UPC points per transect: %d (median)\n\nspread of site means vs departures, and reliability:\n", n_points))
print(spread, digits = 3, row.names = FALSE)
cat("\nsieve kelp, between vs within:\n")
print(as.data.frame(out %>% filter(species == "ak_sieve_kelp") %>%
                      select(predictor, rate_ratio_between, rate_ratio_within,
                             rate_ratio_per_sd_between, rate_ratio_per_sd_within)), digits = 3, row.names = FALSE)

site <- d %>% group_by(site) %>% summarise(hard = mean(hard), any = any(ak_sieve_kelp > 0, na.rm = TRUE),
                                           density = mean(ak_sieve_kelp, na.rm = TRUE) / 60, .groups = "drop")
cat("\nsieve kelp by the site's share of hard substrate:\n")
print(site %>% mutate(band = cut(100 * hard, c(-1, 20, 40, 60, 101), labels = c("<20%", "20-40%", "40-60%", ">60%"))) %>%
        group_by(band) %>% summarise(sites = n(), with_any = sum(any), median_per_m2 = round(median(density), 3)))
cat("\nWrote", file.path(out_fig, "between_within.png"), "and", file.path(out_res, "between_within_checks.csv"), "\n")
