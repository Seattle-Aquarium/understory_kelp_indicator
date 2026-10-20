## Checks the results against INVARIANTS.md. Run it from the repository root
## after step 2 (and after any change to code/), on the default settings --
## withheld basins out, five-year trend window:
##
##   Rscript code/check_invariants.R
##
## Each check names the invariant it tests. It prints one line per check and
## stops with an error at the end if any failed. It reads results/ only; it
## changes nothing.

suppressPackageStartupMessages(library(dplyr))
source(file.path("code", "substrate_classes.R"))

rd <- function(f) read.csv(file.path("results", f), check.names = FALSE)
failed <- character(0)
check <- function(id, what, ok) {
  ok <- isTRUE(all(ok))
  cat(sprintf("%-5s %-4s %s\n", id, if (ok) "ok" else "FAIL", what))
  if (!ok) failed <<- c(failed, id)
}
## equal within a relative tolerance (the saved tables keep 6 significant
## figures), with blanks in the same places
near <- function(a, b, tol = 1e-5) {
  a <- as.numeric(unlist(a)); b <- as.numeric(unlist(b))
  both <- !is.na(a) & !is.na(b)
  length(a) == length(b) && all(is.na(a) == is.na(b)) &&
    all(abs(a[both] - b[both]) <= tol * pmax(1, abs(b[both])))
}

cl <- rd("reef_check_cleaned.csv")
tr <- rd("metrics_transect_scale.csv")
si <- rd("metrics_site_scale.csv")
ba <- rd("metrics_basin_scale.csv")
pu <- rd("metrics_puget_sound_scale.csv")

held_out <- c("Northern Coast", "Western Strait of Juan de Fuca", "Eastern Strait of Juan de Fuca")
canopy   <- c("ak_bull_kelp", "ak_giant_kelp_n", "ak_feather_boa_n")
species  <- setdiff(grep("^ak_", names(tr), value = TRUE), canopy)

## ---- D: cleaning ----------------------------------------------------------------
check("D-4", "withheld basins absent", !any(gsub("_", " ", cl$basin) %in% held_out))
check("D-5", "one survey per site, transect and year", !anyDuplicated(cl[c("site", "transect", "year")]))
check("D-8", "depth zone follows the transect number",
      identical(as.character(cl$depth_zone),
                ifelse(cl$transect %in% 1:3, "offshore", ifelse(cl$transect %in% 4:6, "inshore", NA_character_))))
counts <- unlist(cl[grep("^(ak|iv|fs)_", names(cl))], use.names = FALSE)
counts <- counts[!is.na(counts)]
check("D-11", "counts are whole and non-negative", all(counts >= 0 & counts == round(counts)))
for (pre in c("sb_", "cv_", "rf_")) {
  m <- as.matrix(cl[grep(paste0("^", pre), names(cl))])
  has <- rowSums(!is.na(m)) > 0
  check("D-13", sprintf("UPC %s shares sum to 1 on every transect with a UPC", pre),
        abs(rowSums(m[has, , drop = FALSE], na.rm = TRUE) - 1) < 1e-9)
}
check("D-15", "one row per key", !anyDuplicated(cl$key))

## ---- M: density and spread --------------------------------------------------------
check("M-1", "transect density = count / 60",
      near(as.matrix(tr[species]), as.matrix(cl[match(tr$key, cl$key), species]) / 60))
site_mean <- tr %>% group_by(site, year) %>%
  summarise(across(all_of(species), ~ if (all(is.na(.x))) NA_real_ else mean(.x, na.rm = TRUE)),
            .groups = "drop")
k <- match(paste(site_mean$site, site_mean$year), paste(si$site, si$year))
check("M-3", "site mean = mean of the site's transects that year",
      near(as.matrix(site_mean[species]), as.matrix(si[k, paste0(species, "_avg")])))
site_sd <- tr %>% group_by(site, year) %>% summarise(across(all_of(species), ~ sd(.x, na.rm = TRUE)), .groups = "drop")
check("M-3", "site SD uses n - 1", near(as.matrix(site_sd[species]), as.matrix(si[k, paste0(species, "_sd")])))
basin_mean <- si %>% group_by(basin, year) %>%
  summarise(across(all_of(paste0(species, "_avg")), ~ if (all(is.na(.x))) NA_real_ else mean(.x, na.rm = TRUE)),
            .groups = "drop")
k <- match(paste(basin_mean$basin, basin_mean$year), paste(ba$basin, ba$year))
check("M-4", "basin mean = mean of its site means",
      near(as.matrix(basin_mean[paste0(species, "_avg")]), as.matrix(ba[k, paste0(species, "_avg")])))
inner <- ba[!gsub("_", " ", ba$basin) %in% held_out, ]
ps_mean <- inner %>% group_by(year) %>%
  summarise(across(all_of(paste0(species, "_avg")), ~ if (all(is.na(.x))) NA_real_ else mean(.x, na.rm = TRUE)))
ps <- pu[pu$region == "Puget_Sound", ]
check("M-5", "Puget Sound mean = mean of the inner basins' means",
      near(as.matrix(ps_mean[paste0(species, "_avg")]), as.matrix(ps[match(ps_mean$year, ps$year), paste0(species, "_avg")])))

## ---- T: temporal trend ------------------------------------------------------------
ts <- rd("trend_site_scale.csv")
tb <- rd("trend_basin_scale.csv")
status_of <- function(cur, base) ifelse(cur > 0 & base > 0, "present", ifelse(cur == 0 & base > 0, "lost",
                                        ifelse(cur > 0 & base == 0, "new", "absent")))
check("T-2", "every value has 1-5 baseline years", ts$n_baseline_years >= 1 & ts$n_baseline_years <= 5)
check("T-3", "status follows current and baseline", ts$status == status_of(ts$current, ts$baseline))
check("T-3", "K = 100 x current / baseline for present and lost; NA otherwise",
      near(ts$trend_pct, ifelse(ts$status %in% c("present", "lost"), 100 * ts$current / ts$baseline, NA_real_), 1e-4))
win_ok <- mapply(function(y, b) { b <- as.integer(strsplit(b, ",")[[1]]); all(b >= y - 5 & b < y) },
                 ts$year, as.character(ts$baseline_years))
check("T-2", "baseline years lie in the five years before", win_ok)
agg <- ts %>% group_by(basin, category, year) %>%
  summarise(current = mean(current), baseline = mean(baseline), .groups = "drop")
k <- match(paste(agg$basin, agg$category, agg$year), paste(tb$basin, tb$category, tb$year))
check("T-4", "basin trend = ratio of the mean current to the mean baseline over the same sites",
      near(agg$current, tb$current[k], 1e-4) && near(agg$baseline, tb$baseline[k], 1e-4))

## ---- N: NMDS ------------------------------------------------------------------------
np <- rd("nmds_algae_points.csv")
ns <- rd("nmds_algae_stress.csv")
nsp <- rd("nmds_algae_species.csv")
check("N-1", "the ten understory species, no canopy kelps", setequal(nsp$category, species) && !any(canopy %in% nsp$category))
check("N-2", "every placed transect has understory algae", rowSums(np[species]) > 0)
check("N-2", "placed + empty + unsurveyed = all transects",
      ns$n_transects[1] + ns$n_no_understory_algae[1] + ns$n_no_algae_survey[1] == nrow(tr))
check("N-3", "transformation and distance as specified",
      all(ns$transformation == "wisconsin(sqrt(density))") && all(ns$distance == "bray"))
check("N-5", "stress falls as dimensions are added", all(diff(ns$stress[order(ns$k)]) < 0))

## ---- S: substrate -------------------------------------------------------------------
ef <- rd("nmds_algae_envfit.csv")
check("S-1", "groups cover each substrate class once",
      !anyDuplicated(unlist(substrate_groups)) && all(substrate_individual %in% unlist(substrate_groups)))
check("S-2", "individual classes in size order",
      identical(names(substrate_individual), c("reef", "large boulder", "small boulder", "rock",
                                               "cobble", "pebble", "sand", "shell hash")))
upc <- tr[match(np$key, tr$key), ]
check("S-4", "mean relief = shares x midpoints (0.05, 0.55, 1.5, 2.5 m)",
      near(mean_relief(upc), as.vector(as.matrix(upc[c("rf_0-10cm", "rf_10cm-1m", "rf_1m-2m", "rf_over_2m")]) %*%
                                                 c(0.05, 0.55, 1.5, 2.5))))
check("S-6", "envfit: 4 grouped and 9 individual vectors, 999 permutations",
      sum(ef$set == "grouped") == 4 && sum(ef$set == "individual") == 9 && all(ef$permutations == 999))
dbs <- rd("dbrda_algae_summary.csv")
check("S-7", "db-RDA model and scaling as specified",
      dbs$model[1] == "dbrda(Bray-Curtis ~ hard + soft + relief + Condition(year))" && dbs$scaling[1] == 1)
vp <- rd("dbrda_algae_varpart.csv")
check("S-10", "eight variation-partitioning fractions", nrow(vp) == 8)

## ---- G: GLMMs -------------------------------------------------------------------------
gt <- rd("glmm_algae_terms.csv")
gb <- rd("glmm_algae_bins.csv")
gs <- rd("glmm_algae_summary.csv")
check("G-2", "12 classes x 10 species", length(unique(gt$predictor)) == 12 && length(unique(gt$species)) == 10)
check("G-5", "240 split terms and 120 pooled", sum(gt$form == "split") == 240 && sum(gt$form == "pooled") == 120)
check("G-5", "rate ratio = exp(estimate), CI = exp(estimate +/- 1.96 SE)",
      near(gt$rate_ratio, exp(gt$estimate)) && near(gt$lo, exp(gt$estimate - 1.96 * gt$se)) &&
        near(gt$hi, exp(gt$estimate + 1.96 * gt$se)))
q_split <- p.adjust(gt$p[gt$form == "split"], "BH")
check("G-5", "q = Benjamini-Hochberg across the 240 split terms", near(q_split, gt$q[gt$form == "split"]))
check("G-8", "bins: at least 10 transects and 3 sites, at most 10 per fit",
      all(gb$n >= 10 & gb$n_sites >= 3) && all(table(paste(gb$species, gb$predictor)) <= 10))
check("G-10", "GLMM settings recorded", all(grepl("nbinom2", gs$model)))

cat(if (length(failed)) sprintf("\n%d check(s) failed: %s\n", length(failed), paste(unique(failed), collapse = ", "))
    else "\nAll checks passed.\n")
if (length(failed)) quit(save = "no", status = 1)
