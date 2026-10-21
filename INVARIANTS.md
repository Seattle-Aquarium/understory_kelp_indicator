# Invariants

The rules every data step, metric, analysis and figure in this repository must keep. They are written to be checked, not read for background: the plain-language methods are in the methods document, and the reasons behind each rule are in the header comments of the script that applies it.

## How to use this file

- **Every change to `code/` is checked against this file.** A change that keeps every invariant needs nothing more. Run `Rscript code/check_invariants.R` from the repository root after step 2; it tests the IDs marked ✓ against `results/` and must print `All checks passed.`
- **A change that breaks an invariant is a decision, not a fix.** Make it deliberately, in one commit:
  1. edit the invariant here, saying what changed and why (see *Change log*);
  2. change the code;
  3. bump the affected script's version string or fingerprint tag (R-3), so cached results are rebuilt rather than reused;
  4. update `code/check_invariants.R`, the methods document and `gui/gui_operation.md` where they describe it;
  5. re-run steps 1–3 and commit the regenerated `results/` and `figs/`.
- **Where this file and the code disagree, one of them is wrong.** Resolve it explicitly; never let the code silently win.
- IDs are stable. A retired invariant keeps its ID, marked *retired*, so old references stay meaningful.

## Notation

| Symbol | Meaning |
|---|---|
| *s* | a category: an alga, invertebrate or fish species, or a UPC class |
| *j* | a transect (1–6) |
| *i* | a site |
| *b* | a basin |
| *t* | a survey year |
| *N*<sub>s,i,t,j</sub> | the count of *s* on transect *j* (after D-11) |
| *d*<sub>s,i,t,j</sub> | its density, *N* / 60 (individuals per m²) |
| *p*<sub>k,j</sub> | the share of transect *j*'s UPC points in class *k* |
| *T*<sub>i,t</sub> | the transects at site *i* in year *t* with a non-blank value for the category in question |
| *S*<sub>b,t</sub> | the sites in basin *b* with a non-blank site mean in year *t* |
| NA | blank: the category was not on that survey's datasheet (never zero) |

## Pipeline

Each script runs as its own R process from `code/` (`setwd("../")`), reads only the files named in its header, and takes every path from a `UKELP_*` environment variable with the repository default as fallback. The order is fixed; each step reads only what earlier steps wrote.

| Step | Script | Reads | Writes (`results/`) |
|---|---|---|---|
| 1 | `rc_full_data_clean.R` | `data/Reef_Check/{Algae,UPC,Invert,Fish}_Washington_raw.csv` | `reef_check_cleaned.csv` |
| 2 | `metrics_across_scale.R` | cleaned | `metrics_{transect,site,basin,puget_sound}_scale.csv` |
| 2 | `temporal_trend.R` | site scale | `trend_{site,basin,puget_sound}_scale.csv` |
| 2 | `nmds_algae.R` | transect scale | `nmds_algae_{points,species,stress}.csv` |
| 2 | `substrate_algae.R` | transect scale, NMDS points | `nmds_algae_envfit.csv`, `dbrda_algae_{points,species,terms,summary,varpart}.csv` |
| 2 | `glmm_substrate.R` | transect scale, cleaned | `glmm_algae_{terms,curves,sites,bins,summary}.csv` |
| 3 | `visualizations.R` (with `figure_functions.R`) | all of the above | `figs/` |

`code/substrate_classes.R` holds the substrate definitions (S-1 to S-4) and is sourced by `substrate_algae.R`, `glmm_substrate.R` and `check_invariants.R`; nothing else may define them.

## D: cleaning (`rc_full_data_clean.R`)

- **D-1 Inputs.** The four raw exports; column names are lower-cased with spaces turned to underscores. After the basin join, spaces in every text value become underscores, so basin and site names in all outputs use underscores (`San_Juan_Islands`).
- **D-2 Canadian sites** are removed: 7751 Reef, Ogden Point, Spring Bay, Cates Park, Lions Gate, Ferguson Point, South Bowyer Island, Christie Islet, Whytecliff Park.
- **D-3 Site-name corrections** run before the basin join: `Elliot Bay Marina` → `Elliott Bay Marina`. A correction applied after the join would drop the site silently.
- **D-4 Withheld basins.** ✓ Northern Coast, Western Strait of Juan de Fuca and Eastern Strait of Juan de Fuca are excluded from every output unless `UKELP_INCLUDE_BASINS` names them (comma separated). Every script re-applies the exclusion (search the code for `EXCLUDED BASINS`).
- **D-5 One survey per site per year.** ✓ The winter 2025 dives are dropped: Elliott Bay Marina on 2025-01-28 and Sirens of Spring on 2025-01-27. After cleaning, (site, transect, year) is unique.
- **D-6 Basin lookup.** Each site takes `basin`, `basin_id` and `site_id` from the fixed table in the script (57 sites, 10 basins). A site not in the table is dropped (inner-join semantics). A new site needs a new row there.
- **D-7 Coordinates** come from the UPC, invertebrate and fish files, one pair per `site_id`; the algae file's coordinates are discarded.
- **D-8 Depth zone.** ✓ Transects 1–3 are `offshore`, 4–6 `inshore`; any other number is NA.
- **D-9 Fish.** Fish `depth_ft` is replaced by the algae/UPC/invertebrate depth for the same (basin_id, site_id, transect, year, date) where one exists. Only fish transects 1–6 are kept.
- **D-10 Dates** are parsed as `%Y-%m-%d` or `%m/%d/%Y`, whichever the file uses, and written as `YYYY-MM-DD`.
- **D-11 Partial transects.** ✓ For algae and invertebrates counted over `distance` < 30 m: *N* ← round(*N* × 30 / distance), with `round()`, never truncation. Every count is a whole number ≥ 0.
- **D-12 Canopy kelp.** Giant kelp and feather boa kelp are summed per transect as individuals (`ak_giant_kelp_n`, `ak_feather_boa_n`) and stipes (`ak_giant_kelp_stipe`, `ak_feather_boa_stipe`). Every other algae class becomes `ak_` + its lower-case class code.
- **D-13 UPC shares.** ✓ *p*<sub>k,j</sub> = points in class *k* ÷ points actually read on the transect (`total_amount`), unrounded. Substrate (`sb_`), cover (`cv_`) and relief (`rf_`) each sum to 1 on every transect with a UPC; superlayer (`sp_`) does not. Class codes follow the recode table in the script; a code it does not know is given its category's prefix and named in the log, never dropped.
- **D-14 Prefixes.** `ak_` algae counts, `iv_` invertebrate counts, `fs_` fish counts, `cv_` `rf_` `sb_` `sp_` UPC shares. Every later script selects columns by prefix, so every category column carries exactly one of these.
- **D-15 Survey key.** ✓ `key` = `{basin_id}_{site_id}_{transect}_{yy}`, suffixed `_A`, `_B`, `_C` (by date, then depth) only when a (site, transect, year) has several surveys. It is built once across all four datasets, so one dive has one key everywhere; a row that matches no key stops the script. The output has one row per key.
- **D-16 Zero versus blank.** Within each survey (`site_id`, `date`), every category recorded anywhere in that survey gets 0 on each of its transects where it was not seen. A category absent from that survey stays NA. Nothing fills NA with 0 afterwards (`pivot_wider` has no `values_fill`).
- **D-17 Output.** `reef_check_cleaned.csv`: the metadata columns `key, basin, basin_id, site, site_id, transect, depth_zone, latitude, longitude, year, date, depth_ft`, then every category; rows ordered by `site_id`, `year` descending, `transect`.

## M: density and spread (`metrics_across_scale.R`)

- **M-1 Density.** ✓ For `ak_` (except the two stipe columns), `iv_` and `fs_`: *d* = *N* / 60. UPC shares are carried unchanged. Stipe columns stay in the cleaned table only.
- **M-2 Transect scale** is the cleaned table in density units: one row per transect, no aggregation.
- **M-3 Site scale.** ✓ One row per (site, year). For each category:

  $$\bar d_{s,i,t} = \frac{1}{|T_{i,t}|}\sum_{j \in T_{i,t}} d_{s,i,t,j}, \qquad SD_{s,i,t} = \sqrt{\frac{1}{|T_{i,t}|-1}\sum_{j \in T_{i,t}} \left(d_{s,i,t,j} - \bar d_{s,i,t}\right)^2}, \qquad V = SD^2$$

  Blanks are left out of both sum and count, never read as 0; a category with no values is NA; with one value, SD and variance are NA. `n_transects` counts rows; `n_transects_{algae,upc,invert,fish}` count the transects each dataset covered.
- **M-4 Basin scale.** ✓ One row per (basin, year): mean, SD and variance of the site means *d̄*<sub>s,i,t</sub> over *S*<sub>b,t</sub>, each site weighted equally, with the same NA rules. `n_sites` counts them.
- **M-5 Puget Sound.** ✓ One row per year: the same over the basin means of the seven inner basins, each weighted equally (`region = Puget_Sound`). `Washington_wide`, over every basin present, is written only when an outer basin is in the data.
- **M-6 Precision.** Nothing is rounded between scales; tables are written with `signif(x, 6)` on the statistic columns only (never latitude or longitude).

## T: temporal trend (`temporal_trend.R`)

- **T-1 Input.** Every `_avg` column of the site table (all prefixes), as one row per (site, category, year) with a non-blank annual mean *D*<sub>s,i,t</sub>.
- **T-2 Baseline.** ✓ For each row, the baseline is the mean of the site's annual means in the window *t*−5 … *t*−1 (`UKELP_TREND_WINDOW`, default 5) over the years with a value; a row needs at least 1 baseline year (`UKELP_TREND_MIN_BASELINE_YEARS`, default 1). `n_baseline_years` and `baseline_years` record them.
- **T-3 Status and value.** ✓ With *C* the current value and *B* the baseline:

  | status | condition | `trend_pct` |
  |---|---|---|
  | present | *C* > 0, *B* > 0 | 100 · *C* / *B* |
  | lost | *C* = 0, *B* > 0 | 0 |
  | new | *C* > 0, *B* = 0 | NA |
  | absent | *C* = 0, *B* = 0 | NA |

- **T-4 Basin: a ratio of means over matched sites.** ✓ Over the site rows that exist for (basin, category, year) — sites with both a current value and a baseline — *C*<sub>b</sub> = mean *C*<sub>i</sub> and *B*<sub>b</sub> = mean *B*<sub>i</sub>, each site weighted equally; status and value then follow T-3. The mean of the finite site percentages is written alongside as `trend_pct_mean_of_sites`, a diagnostic only, never the headline. `n_sites_{present,lost,new,absent}` count the members.
- **T-5 Puget Sound** repeats T-4 over the basin rows of the inner basins, each basin weighted equally; `Washington_wide` as in M-5.
- **T-6 Precision.** `current`, `baseline` and the percentages are written with `signif(x, 6)`.

## N: NMDS (`nmds_algae.R`)

- **N-1 Species.** ✓ Every `ak_` column except the canopy kelps `ak_bull_kelp`, `ak_giant_kelp_n` and `ak_feather_boa_n` (the stipe columns are already gone): the ten understory species.
- **N-2 Transects.** ✓ A transect is placed if it was surveyed for algae (any species non-blank) and holds at least one understory alga. Blanks within a placed transect become 0. The numbers left out, `n_no_understory_algae` and `n_no_algae_survey`, are written; placed + both = all transects.
- **N-3 Transformation and distance.** ✓ *X* = `wisconsin(sqrt(D))`: square root, then each species divided by its maximum, then each transect by its total. Bray–Curtis dissimilarity on *X*.
- **N-4 Fitting.** `metaMDS(X, distance = "bray", k, trymax, autotransform = FALSE, wascores = TRUE, parallel = cores)`, with `set.seed(20261019 + k)` before each *k*. *k* = 2 (the figures): `trymax = 100`, then one more round from `previous.best` if the best was not repeated. *k* = 3: `trymax = 50` (scores stored). *k* = 1 and 4: `trymax = 30` (stress profile only). `cores = detectCores() − 2`.
- **N-5 Stress.** ✓ Stress is written for *k* = 1–4 and falls as *k* rises. The figures use *k* = 2; its stress is printed on every NMDS figure.
- **N-6 Scores.** Site scores for *k* = 2 and 3; species scores are weighted averages of the site scores (`wascores`). `metaMDS`'s default post-processing (centring, rotation to principal components and half-change scaling) is kept.

## S: substrate (`substrate_classes.R`, `substrate_algae.R`)

- **S-1 Groups.** ✓ Each substrate class belongs to exactly one group:
  - hard = `sb_reef`, `sb_l_boulder-wa`, `sb_s_boulder-wa`, `sb_rock`, legacy `sb_boulder`;
  - mixed = `sb_cobble-wa`, `sb_pebble-wa`, legacy `sb_cobble`;
  - soft = `sb_sand`, `sb_clay-wa`, `sb_shell_hash-wa`.

  `sb_other` is in no group. Rock stays in hard by decision of 2026-10-20 (moving it to mixed raised the db-RDA R² from 18.8% to 19.6%); revisit only deliberately.
- **S-2 Individual classes and their order.** ✓ reef, large boulder, small boulder, rock, cobble, pebble, sand, shell hash: largest first, then the sediments. Every table, model set and figure uses this order. Clay and other are too scarce to model alone.
- **S-3 Group share** = the row sum of its classes' shares, blanks counting as 0; NA only when none of its classes was recorded (no UPC on the transect).
- **S-4 Mean relief.** ✓ relief = 0.05 · *p*<sub>0–10 cm</sub> + 0.55 · *p*<sub>10 cm–1 m</sub> + 1.5 · *p*<sub>1–2 m</sub> + 2.5 · *p*<sub>>2 m</sub>, in metres.
- **S-5 Community.** The NMDS's placed transects and *X* (N-2, N-3): the Wisconsin standardization is done on all of them, then transects without a complete hard/mixed/soft/relief are dropped, so Bray–Curtis distances match the NMDS.
- **S-6 Substrate vectors (`envfit`).** ✓ Two sets, told apart by `set`: `grouped` (hard, mixed, soft, relief) and `individual` (the eight classes and relief, each fitted on its own). Each vector is tested twice, 999 permutations, `set.seed(20261019)` before each call:
  - within sites: transect scores against transect values, permutations restricted to blocks of one site (`how(blocks = site)`);
  - between sites: site centroids (mean scores) against site means (each transect equal weight, all years), free permutations.

  `r2` is across transects; written arrow coordinates are `scores(fit, "vectors")` (unit direction × √r²).
- **S-7 db-RDA.** ✓ `dbrda(BC ~ hard + soft + relief + Condition(year))`: mixed is the reference, so the three shares are never all in one model. Species scores via `sppscores(model) <- X`. Scores at `scaling = 1`; transect scores are weighted averages (`wa`). R² and adjusted R² are reported, not tested, at this level.
- **S-8 Between sites.** Site means of the raw densities → `wisconsin(sqrt())` → Bray–Curtis; `dbrda(~ hard + soft + relief)` on the site means of the predictors; `anova(by = "margin")` and global, free permutations.
- **S-9 Within sites.** `dbrda(BC ~ hard + soft + relief + Condition(site + year))`; `anova(by = "margin")` and global, permutations within site blocks. `within_share_of_within_site` = constrained inertia ÷ (total − conditioned).
- **S-10 Variation partitioning.** ✓ `varpart(BC, ~ hard + soft + relief, ~ basin, ~ year)`; the eight fractions [a]–[h] written in that order with their adjusted R².
- **S-11 Run settings.** 999 permutations; seed 20261019; `cores = detectCores() − 2`; cluster workers are given this R's library paths.

## G: GLMMs (`glmm_substrate.R`)

- **G-1 Data.** One row per transect with any non-blank understory count and non-blank hard share, relief and `depth_ft`. `site` and `year` are factors. Depth enters as the continuous `depth` = (`depth_ft` − 15) / 10 (per 10 ft, centred on 15 ft), never as `depth_zone`: at many sites the inshore and offshore transects run on into each other, so the two are not distinct categories. Recorded depths are used as they are, including the 14 under 3 ft.
- **G-2 Predictors.** ✓ 12 classes (hard, mixed, soft, relief; the eight individual classes) × 10 species. Unit *u* = 0.10 for a share, 0.25 m for relief. For each class *x*, with *x̄*<sub>i</sub> its mean over all of site *i*'s rows in the data (all years):
  - `_between` = *x̄*<sub>i</sub> / *u*;
  - `_within` = (*x* − *x̄*<sub>i</sub>) / *u*;
  - `_x` = *x* / *u* (pooled form).
- **G-3 Model.** For each (species, class), on the rows where both are non-blank, `glmmTMB(y ~ between + within + depth + year + (1 | site), family = nbinom2)` (split form) and `y ~ x + depth + year + (1 | site)` (pooled form). No GLMM uses `depth_zone`. The depth rate ratio per 10 ft and its p are written beside every term (`depth_rate_ratio`, `depth_p`). *y* is the transect's count (D-11), never a density or a ratio.
- **G-4 Fit status.** Warnings are captured, never stop the loop; `ok` = no warning and finite standard errors. A fit that is not ok draws no curve and no bins.
- **G-5 Reporting.** ✓ `rate_ratio` = exp(estimate); `lo`, `hi` = exp(estimate ± 1.96 SE); `p` = Wald; `q` = Benjamini–Hochberg within each form, across all its non-blank p (240 split terms, 120 pooled).
- **G-6 Curves.** Between: 60 points over the observed range of site means; the expected density (÷ 60) at site effect 0 (the median site), exp of the linear predictor averaged over the depths and years of the transects in the years the species was recorded, each transect counting once; 95% band by the delta method. Within: exp(*β*<sub>W</sub> · departure / *u*) over 40 points spanning the 2.5–97.5% quantiles of the departures, band from *β*<sub>W</sub> ± 1.96 SE.
- **G-7 Site means** (`glmm_algae_sites.csv`): per (species, class, site), mean share (×100, or metres for relief), mean density (count ÷ 60) and *n* transects.
- **G-8 Bins** (`glmm_algae_bins.csv`). ✓ For each ok split fit:
  1. expected<sub>j</sub> = exp(*η*<sub>j</sub> − *β*<sub>W</sub> · within<sub>j</sub>), where *η* is the fitted linear predictor including depth, year and the site effect;
  2. edges = 11 equally spaced points from the 2.5% to the 97.5% quantile of the departures; transects outside are not binned;
  3. ratio = Σ *y* ÷ Σ expected over the bin's transects, plotted at their mean departure;
  4. interval = 2.5% and 97.5% quantiles of 999 resamples of the bin's sites with replacement (site sums), `set.seed(20261020 + task index)`;
  5. a bin with fewer than 10 transects or 3 sites is dropped.
- **G-9 Display rules.** Bins are not drawn for a species on fewer than 5% of transects (torn kelp), whose few transects with large expected counts decide every bin; the caption says so.
- **G-10 Settings.** ✓ `version = "glmm-v3"`; `cores = detectCores() − 2`; the summary records the model, units, glmmTMB version and fingerprint.

## F: figures (`figure_functions.R`, `visualizations.R`, `gui/app.R`)

- **F-1 One definition.** Every figure is built by `make_figure()` in `figure_functions.R`; the app's preview and Run's saved file are the same figure by construction. Nothing else draws a figure. The preview adds hover layers (F-9); with hover off, every layer is the plain ggplot2 one, so a saved PNG is byte-identical to Run's.
- **F-2 Size.** 12 × 7 inches at 300 dpi, white background.
- **F-3 Paths.** `figs/<analysis>/<scale>/<view>/<data type>/<stem>.png`, with the stems `figure_file()` defines; the ordinations and GLMMs use scale `transect` and data type `algae`.
- **F-4 Exclusions and orders.** The canopy kelps are never drawn. GLMM species run from hard-substrate to soft-bottom along the db-RDA's first axis; classes follow S-2.
- **F-5 Density.** Blue = the mean at the chosen scale; gray = its members. With standard deviation: a ±1 SD band among the members replaces the gray lines, its lower edge clamped at 0; no band in the composite view. A blank year breaks a line. The subtitle gives members per year; facet titles give "(*n* of *m* members)" with the category.
- **F-6 Trend.** log₂ axis centred on 100%; values outside 6.25%–1,600% drawn at the limit, blue ones labelled with their value; lost and new as triangles on the bottom and top edges, with member counts at the basin and Puget Sound scales; lines join consecutive `present` years only.
- **F-7 Ordinations.** Basin colours come from one palette over every basin, so a basin keeps its colour when one is highlighted. Ellipses: 95% normal-theory, groups of ≥ 5 transects. Species arrows sit at their scores on the NMDS, and on the db-RDA are stretched by one shared factor to reach 90% of the points' extent; substrate arrows are stretched by one shared factor to reach 75%.
- **F-8 GLMM.** Heatmap fill = log₂(rate ratio) clamped to ±2 (¼× to 4×), the same scale in every view; stars for q < 0.05, 0.01, 0.001; † for fits that are not ok. Views: each set between sites, within sites, and both stacked; one species × one class. Within panels: log₂ axis spanning at least ¼×–4× and at most 1/16×–16×; bins beyond it are open triangles on the edge they passed.
- **F-9 Hover (app preview only).** With `options(ukelp.hover = TRUE)` and ggiraph present, `hover_layer()` swaps the member lines and points, the composite lines and the ordination points for their ggiraph twins, and `hover_widget()` draws the figure with ggiraph at the PNG's 12 × 7 in. Run and Save never turn hover on.
  - Lines (density, standard deviation, trend): a member line's id is its member (site, basin or transect), so hovering it highlights that member in Reef Check's light blue (#00C4B3) in every panel, leaving the other lines as they are; its points name the member, year and value. Blue points name the place, year and value (and SD).
  - Composite: a category's line thickens in its own colour and names the category.
  - Ordinations: a point's id is its site, so hovering one transect outlines every transect of that site; its label gives site, basin, year, transect number and depth in feet (and the sized species' density). Hovering a legend entry (key or name) picks out that group. Only the ordinations fade what is not hovered, to 20% opacity.
  - GLMM, one species × one class: on the between-site panel a site's point fills light blue and names the site, basin, mean share (or relief), mean density and transect count; on the within-site panel a bin's point gives its ratio and interval, transects, sites and mean departure. The heatmaps carry no hover layers.

## R: reproducibility

- **R-1 Environment.** R 4.6.1 with packages from the CRAN snapshot of 2026-10-01 (`gui/launcher/runtime.cfg`: `R_VERSION`, `R_SHA256`, `CRAN_SNAPSHOT`, `LIBRARY_REV`). Versions at the time of writing: vegan 2.7.6, glmmTMB 1.1.15.2, TMB 1.9.25, dplyr 1.2.1, tidyr 1.3.2, ggplot2 4.0.3, patchwork 1.3.2, ggrepel 0.9.8, scales 1.4.0, shiny 1.14.0, bslib 0.12.0, ggiraph 0.9 (hover; `LIBRARY_REV` 4). A package added to `install_packages.R` bumps `LIBRARY_REV`.
- **R-2 Seeds.** NMDS 20261019 + *k*; permutation tests 20261019; GLMM bins 20261020 + task index. Never removed or varied between runs.
- **R-3 Caching.** `nmds_algae.R`, `substrate_algae.R` and `glmm_substrate.R` fingerprint their inputs and settings (an md5 written to their summary file) and reuse the last results when it matches; `UKELP_NMDS_FORCE=1` forces a re-run of all three. A change to a method must change its fingerprint: the settings it hashes, the `"v2"` tag in `substrate_algae.R`, or `version` in `glmm_substrate.R`.
- **R-4 Determinism.** Every result is reproducible on the same computer. The NMDS draws its random starts in batches of `cores`, so a computer with a different core count can settle on an equally good but slightly different map; the committed results are the reference. Permutation p-values are reproducible given the seed and 999 permutations; their smallest possible value is 0.001.
- **R-5 Precision.** Results are written at full precision except where M-6 and T-6 say otherwise.

## Change log

| Date | IDs | Change |
|---|---|---|
| 2026-10-22 | F-9 | Hover highlight in light blue; line plots no longer fade the other lines; GLMM single-species figures hover. |
| 2026-10-21 | F-1, F-9, R-1 | Hover in the app's preview (ggiraph, added to the app's packages; `LIBRARY_REV` 4). Saved PNGs unchanged. |
| 2026-10-21 | G-1, G-3, G-6, G-8, G-10 | Depth enters the GLMMs as continuous depth in feet instead of the inshore/offshore depth zone, which is not a true category at many sites. 894 transects (three lack a depth). Rate ratios moved by a median of 0.2%; 35 terms are significant before and after (32 in both); no significant term changed direction. |
| 2026-10-20 | all | First version, written from the code at commit 959bd1a plus the within-site bins (G-8, G-9). |
