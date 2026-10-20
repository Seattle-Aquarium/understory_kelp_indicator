## Single-species GLMMs: each understory alga's count against each substrate class
## Reads:  results/metrics_transect_scale.csv  (metrics_across_scale.R) -- the UPC shares
##         results/reef_check_cleaned.csv      (rc_full_data_clean.R)  -- the algae counts
## Writes: results/glmm_algae_terms.csv    every coefficient: rate ratio, CI, p and q
##         results/glmm_algae_curves.csv   the fitted curves the figures draw
##         results/glmm_algae_sites.csv    each site's mean share and density, per species
##         results/glmm_algae_summary.csv  per species: transects, how many with it, settings
##
## Modelled on CCR_benthic_analyses/ROV_diver_comparison/code/abundance_models.R:
## negative binomial (NB2) with a log link, so exp(coefficient) is a rate ratio,
## and every fit wrapped so that a failure or a warning is recorded in its row
## rather than stopping the loop. Fitted with glmmTMB's nbinom2, the same model
## as lme4's glmer.nb, which is faster and converges more reliably over the
## 240 fits here.
##
## ---- The models ----------------------------------------------------------------
## For each of the ten understory algae and each substrate class -- hard, mixed
## and soft substrate and mean relief (substrate_classes.R), and the eight
## individual UPC classes -- one class at a time, since the shares sum to one
## and cannot all go in one model:
##
##   count ~ x_between + x_within + depth_zone + year + (1 | site)
##
##   count      individuals on the 30 x 2 m transect (standardized to 30 m)
##   x_between  the site's mean share over all its transects and years
##   x_within   the transect's share minus its site's mean
##
## Why x is split: with a site random intercept, a single coefficient for x is
## close to a precision-weighted average of the within-site slope (some 850
## comparisons of transects at the same site) and the between-site slope (42
## site means, noisy because sites differ in many other ways). Here that is
## about 95% within-site, so one coefficient would answer only "within a site"
## while reading as the effect of substrate. Splitting asks both questions in
## one model (van de Pol & Wright 2009, Animal Behaviour 77:753-758). The
## single-coefficient fit is kept too, as form "pooled".
##
## Units: rate ratios per +10 percentage points of the transect's UPC points;
## relief per +0.25 m. p: Wald z. q: Benjamini-Hochberg across all 240 between
## and within terms (and separately across the 120 pooled ones).
##
## ---- Curves -------------------------------------------------------------------
## between: expected density at a typical site (site effect 0, the median site)
##          over the observed range of site means, averaged over depth zones and
##          over the years the species was recorded (no 2021 transect had sieve
##          kelp, which leaves that year's coefficient unbounded), weighted as in
##          the data. The mean over sites runs higher, by exp(site variance / 2).
## within:  the count relative to the site's typical transect, over the middle
##          95% of the transects' departures.

rm(list=ls())
suppressPackageStartupMessages({
  library(dplyr)
})

## relative file paths
results <- "results"

## set working directory to home folder
setwd("../")

## the substrate groups, individual classes and relief, shared with substrate_algae.R
source(file.path("code", "substrate_classes.R"))

## ---- FILE LOCATIONS AND SETTINGS --------------------------------------------
## Run on its own it reads and writes the repository's results folder. The
## Shiny app in gui/ sets these environment variables first; see
## rc_full_data_clean.R for why environment variables rather than options().
from_env <- function(var, default) {
  value <- Sys.getenv(var, unset = "")
  if (nzchar(value)) value else default
}

transect_csv <- from_env("UKELP_TRANSECT_CSV",     file.path(results, "metrics_transect_scale.csv"))
cleaned_csv  <- from_env("UKELP_CLEANED_CSV",      file.path(results, "reef_check_cleaned.csv"))
terms_csv    <- from_env("UKELP_GLMM_TERMS_CSV",   file.path(results, "glmm_algae_terms.csv"))
curves_csv   <- from_env("UKELP_GLMM_CURVES_CSV",  file.path(results, "glmm_algae_curves.csv"))
sites_csv    <- from_env("UKELP_GLMM_SITES_CSV",   file.path(results, "glmm_algae_sites.csv"))
summary_csv  <- from_env("UKELP_GLMM_SUMMARY_CSV", file.path(results, "glmm_algae_summary.csv"))

cores <- max(1, parallel::detectCores() - 2)
version <- "glmm-v1"

if (!requireNamespace("glmmTMB", quietly = TRUE)) {
  stop("The GLMMs need the glmmTMB package. Start the app with run_data_interpreter.bat ",
       "(it installs it), or install.packages(\"glmmTMB\") to run this from RStudio.")
}

outputs <- c(terms_csv, curves_csv, sites_csv, summary_csv)
for (folder in unique(dirname(outputs))) dir.create(folder, recursive = TRUE, showWarnings = FALSE)

df_tr <- read.csv(transect_csv, check.names = FALSE)
cl    <- read.csv(cleaned_csv, check.names = FALSE)

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
## The data: one row per transect with an algae survey and a UPC
## ============================================================

canopy  <- c("ak_bull_kelp", "ak_giant_kelp_n", "ak_feather_boa_n")
species <- setdiff(grep("^ak_", names(df_tr), value = TRUE), canopy)

d <- data.frame(key = df_tr$key, basin = df_tr$basin, site = df_tr$site,
                year = df_tr$year, depth_zone = df_tr$depth_zone)
for (g in names(substrate_groups)) d[[g]] <- substrate_share(df_tr, substrate_groups[[g]])
d$relief <- mean_relief(df_tr)
for (nm in names(substrate_individual)) d[[nm]] <- df_tr[[substrate_individual[[nm]]]]
missing_counts <- setdiff(species, names(cl))
if (length(missing_counts)) stop("No counts in ", cleaned_csv, " for ", paste(missing_counts, collapse = ", "))
d <- cbind(d, cl[match(d$key, cl$key), species])
d <- d[rowSums(!is.na(d[species])) > 0 & !is.na(d$hard) & !is.na(d$relief), ]
d$site <- factor(d$site)
d$year <- factor(d$year)
d$depth_zone <- factor(d$depth_zone, levels = c("offshore", "inshore"))

## predictors: display name -> a syntactic id for the formulas
grouped    <- c("hard", "mixed", "soft", "relief")
individual <- names(substrate_individual)
predictors <- c(grouped, individual)
ids        <- setNames(gsub(" ", "_", predictors), predictors)
unit_of    <- function(p) if (p == "relief") 0.25 else 0.10   # +0.25 m, or +10 points
for (p in predictors) {
  id <- ids[[p]]; u <- unit_of(p)
  site_mean <- ave(d[[p]], d$site, FUN = function(z) mean(z, na.rm = TRUE))
  d[[paste0(id, "_x")]]       <- d[[p]] / u
  d[[paste0(id, "_between")]] <- site_mean / u
  d[[paste0(id, "_within")]]  <- (d[[p]] - site_mean) / u
}
cat(sprintf("GLMMs: %d transects at %d sites, %d species x %d substrate classes\n",
            nrow(d), nlevels(d$site), length(species), length(predictors)))

## ============================================================
## Reuse the last run if nothing it depends on has changed
## ============================================================
fingerprint <- local({
  f <- tempfile(fileext = ".txt")
  on.exit(unlink(f))
  writeLines(c(version, paste(d$key, collapse = ","),
               format(as.vector(as.matrix(d[species])), digits = 15),
               format(as.vector(as.matrix(d[predictors])), digits = 15),
               as.character(d$depth_zone)), f)
  unname(tools::md5sum(f))
})
previous <- if (file.exists(summary_csv)) tryCatch(read.csv(summary_csv), error = function(e) NULL)
if (!is.null(previous) && "fingerprint" %in% names(previous) &&
    identical(as.character(previous$fingerprint[1]), fingerprint) &&
    all(file.exists(outputs)) && !identical(Sys.getenv("UKELP_NMDS_FORCE"), "1")) {
  cat("The transects and substrate are unchanged since the last run, so the GLMMs are reused:\n")
  cat(paste0("   ", outputs, "\n"), sep = "")
  quit(save = "no", status = 0)
}

## ============================================================
## Fit: every species x class, pooled and split
## ============================================================

tasks <- expand.grid(species = species, predictor = predictors, form = c("pooled", "split"),
                     stringsAsFactors = FALSE)

fit_one <- function(i) {
  t <- tasks[i, ]
  id <- ids[[t$predictor]]; u <- unit_of(t$predictor)
  dd <- droplevels(d[!is.na(d[[t$species]]) & !is.na(d[[t$predictor]]), ])
  dd$y <- dd[[t$species]]
  rhs <- if (t$form == "pooled") paste0(id, "_x") else paste0(id, "_between + ", id, "_within")
  f <- as.formula(paste("y ~", rhs, "+ depth_zone + year + (1 | site)"))
  warn <- character(0)
  fit <- withCallingHandlers(
    tryCatch(glmmTMB::glmmTMB(f, data = dd, family = glmmTMB::nbinom2()), error = function(e) e),
    warning = function(w) { warn <<- c(warn, conditionMessage(w)); invokeRestart("muffleWarning") })
  base <- data.frame(species = t$species, predictor = t$predictor,
                     set = if (t$predictor %in% grouped) "grouped" else "individual",
                     form = t$form, n = nrow(dd), n_present = sum(dd$y > 0),
                     n_sites = nlevels(dd$site))
  if (inherits(fit, "error")) {
    return(list(terms = cbind(base, term = NA_character_, estimate = NA_real_, se = NA_real_,
                              z = NA_real_, p = NA_real_, ok = FALSE,
                              note = conditionMessage(fit), site_sd = NA_real_, theta = NA_real_),
                curves = NULL))
  }
  sm <- summary(fit); co <- sm$coefficients$cond
  rows <- rownames(co)[startsWith(rownames(co), paste0(id, "_"))]
  ok <- length(warn) == 0 && all(is.finite(co[rows, "Std. Error"]))
  terms <- cbind(base, term = substring(rows, nchar(id) + 2),
                 estimate = co[rows, "Estimate"], se = co[rows, "Std. Error"],
                 z = co[rows, "z value"], p = co[rows, "Pr(>|z|)"], ok = ok,
                 note = if (length(warn)) paste(unique(warn), collapse = "; ") else NA_character_,
                 site_sd = sqrt(as.numeric(sm$varcor$cond$site)), theta = stats::sigma(fit),
                 row.names = NULL)
  curves <- NULL
  if (t$form == "split" && ok) {
    scale_x <- if (t$predictor == "relief") 1 else 100       # m, or percent
    beta <- glmmTMB::fixef(fit)$cond; V <- stats::vcov(fit)$cond
    ## depth zones and the years the species was recorded, as in the data
    yrs <- names(which(tapply(dd$y, dd$year, sum) > 0))
    combos <- as.data.frame(table(depth_zone = dd$depth_zone[dd$year %in% yrs],
                                  year = dd$year[dd$year %in% yrs]))
    combos <- combos[combos$Freq > 0, ]
    combos$w <- combos$Freq / sum(combos$Freq)
    tt <- stats::delete.response(stats::terms(stats::formula(fit, fixed.only = TRUE)))
    site_means <- tapply(dd[[paste0(id, "_between")]], dd$site, `[`, 1) * u
    grid <- seq(min(site_means), max(site_means), length.out = 60)
    between <- lapply(grid, function(g) {
      nd <- combos
      nd$depth_zone <- factor(nd$depth_zone, levels = levels(dd$depth_zone))
      nd$year <- factor(nd$year, levels = levels(dd$year))
      nd[[paste0(id, "_between")]] <- g / u
      nd[[paste0(id, "_within")]] <- 0
      X <- stats::model.matrix(tt, nd)
      xb <- colSums(X * nd$w)
      b <- beta[colnames(X)]; VV <- V[colnames(X), colnames(X)]
      eta <- sum(xb * b); se <- sqrt(drop(t(xb) %*% VV %*% xb))
      data.frame(panel = "between", x = g * scale_x, fit = exp(eta) / 60,
                 lo = exp(eta - 1.96 * se) / 60, hi = exp(eta + 1.96 * se) / 60)
    })
    dep <- dd[[paste0(id, "_within")]] * u
    dx <- seq(stats::quantile(dep, 0.025), stats::quantile(dep, 0.975), length.out = 40)
    bw <- co[paste0(id, "_within"), ]
    within <- data.frame(panel = "within", x = dx * scale_x, fit = exp(bw[1] * dx / u),
                         lo = exp((bw[1] - 1.96 * bw[2]) * dx / u),
                         hi = exp((bw[1] + 1.96 * bw[2]) * dx / u))
    curves <- cbind(species = t$species, predictor = t$predictor,
                    rbind(do.call(rbind, between), within))
  }
  list(terms = terms, curves = curves)
}

cat(sprintf("Fitting %d negative binomial GLMMs on %d cores.\n", nrow(tasks), cores))
t0 <- Sys.time()
cl_ <- parallel::makeCluster(cores)
invisible(parallel::clusterCall(cl_, function(p) { .libPaths(p); invisible(NULL) }, .libPaths()))
parallel::clusterExport(cl_, c("tasks", "d", "ids", "unit_of", "grouped"))
fits <- parallel::parLapply(cl_, seq_len(nrow(tasks)), fit_one)
parallel::stopCluster(cl_)
cat(sprintf("  done in %.0f s\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))

terms_out <- bind_rows(lapply(fits, `[[`, "terms")) %>%
  mutate(unit = ifelse(predictor == "relief", "+0.25 m mean relief", "+10 points of UPC"),
         rate_ratio = exp(estimate), lo = exp(estimate - 1.96 * se), hi = exp(estimate + 1.96 * se))
terms_out$q <- NA_real_
for (fm in c("pooled", "split")) {
  k <- terms_out$form == fm & !is.na(terms_out$p)
  terms_out$q[k] <- p.adjust(terms_out$p[k], "BH")
}
curves_out <- bind_rows(lapply(fits, `[[`, "curves"))

## each site's mean share and mean density, per species and class: the
## points the between-site panels draw
sites_out <- bind_rows(lapply(predictors, function(p) {
  bind_rows(lapply(species, function(s) {
    k <- !is.na(d[[s]]) & !is.na(d[[p]])
    d[k, ] %>% group_by(site, basin) %>%
      summarise(x = mean(.data[[p]]) * (if (p == "relief") 1 else 100),
                density = mean(.data[[s]]) / 60, n = n(), .groups = "drop") %>%
      mutate(species = s, predictor = p, .before = 1)
  }))
}))

flagged <- terms_out %>% filter(!ok) %>% distinct(species, predictor, form)
summary_out <- data.frame(
  species = species,
  n_transects = sapply(species, function(s) sum(!is.na(d[[s]]))),
  n_present = sapply(species, function(s) sum(d[[s]] > 0, na.rm = TRUE)),
  n_sites = nlevels(d$site),
  model = "count ~ x_between + x_within + depth_zone + year + (1 | site), negative binomial (glmmTMB nbinom2)",
  units = "+10 percentage points of UPC points; relief +0.25 m",
  n_models = nrow(tasks), n_models_flagged = nrow(flagged),
  glmmTMB = as.character(utils::packageVersion("glmmTMB")),
  fingerprint = fingerprint, row.names = NULL)
summary_out$prevalence <- summary_out$n_present / summary_out$n_transects

write.csv(terms_out,   terms_csv,   row.names = FALSE)
write.csv(curves_out,  curves_csv,  row.names = FALSE)
write.csv(sites_out,   sites_csv,   row.names = FALSE)
write.csv(summary_out, summary_csv, row.names = FALSE)

cat(sprintf("%d of %d fits clean; %d flagged (see the note column): %s\n",
            nrow(tasks) - nrow(flagged), nrow(tasks), nrow(flagged),
            paste(unique(sub("^ak_", "", flagged$species)), collapse = ", ")))
strong <- terms_out %>% filter(form == "split", q < 0.05) %>%
  transmute(cell = sprintf("%s ~ %s (%s) %.2fx", sub("^ak_", "", species), predictor, term, rate_ratio))
cat(sprintf("%d between/within terms with q < 0.05, e.g.\n  %s\n", nrow(strong),
            paste(head(strong$cell, 8), collapse = "\n  ")))
cat("Wrote", paste(basename(outputs), collapse = ", "), "\n")
