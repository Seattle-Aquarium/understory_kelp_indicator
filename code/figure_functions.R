## Shared figure definitions for the Understory Kelp Indicator Data Interpreter.
##
## Everything here builds a ggplot and nothing here writes a file or decides
## what to draw. Two things use it:
##
##   code/visualizations.R   loops over every selection and saves the PNGs
##   gui/app.R               renders one figure at a time for the preview
##
## Keeping the builders in one place is the point: the preview and the saved
## file are then the same figure by construction rather than by two
## definitions being kept in step by hand.
##
## ---- What a figure is ------------------------------------------------------
## Every figure is four choices, which are also the four dropdowns in the app:
##
##   analysis  trend       the temporal trend metric (issue #2)
##             density     average density (algae) or proportion (UPC)
##             density_sd  the same with a +/- 1 SD band
##   scale     site        transects -> one site
##             basin       sites -> one basin
##             puget       basins -> Puget Sound (or Washington-wide)
##   view      within      one place, one panel per species
##             across      one species, one panel per place
##             composite   one place, every species on one set of axes
##   group     algae, cover, relief, substrate, superlayer
##
## make_figure() takes those four, plus which place (`where`) or which species
## (`category`), and returns list(ok = TRUE, plot = <ggplot>) or, for a
## combination with no honest figure, list(ok = FALSE, reason = "<why>").
## figure_file() says where that figure belongs under figs/.
##
## The builders read the tables from their own environment -- df_transect,
## df_site, df_basin, df_puget for density and tr_site, tr_basin, tr_puget for
## the trend -- rather than taking them as arguments. load_figure_tables()
## reads them all from csv and puts them there:
##
##   fig <- new.env(); sys.source("code/figure_functions.R", envir = fig)
##   fig$load_figure_tables(list(site = "results/metrics_site_scale.csv", ...))
##   fig$make_figure("density", "basin", "within", fig$metric_groups[[1]],
##                   where = "Hood_Canal")$plot
##
## Sourcing it into the global environment, as visualizations.R does, works the
## same way.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})


my.theme = theme(panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 panel.background = element_blank(),
                 axis.line = element_line(colour = "black"),
                 axis.title=element_text(size=16),
                 axis.text=element_text(size=14),
                 plot.title = element_text(size=16))

## size (inches) and resolution of the exported figures; the on-screen device is
## opened at the same size, so what's drawn matches what's written to figs/.
## A little taller than the old 12 x 6 to make room for the two-line facet
## titles and the caption that says what the lines and bands are.
fig_width <- 12
fig_height <- 7
fig_dpi <- 300

## ---- METRIC GROUPS ---------------------------------------------------------
## Every figure set below is built once per group, into its own subfolder.
## "ak_" is the algae data: counts on a 30 x 2 m transect divided by its 60 m^2,
## so individuals per square metre. The other four are the UPC point-intercept
## categories, which are proportions of the points sampled along a transect --
## a different quantity entirely, hence the separate y-axis label and the
## separate folder.
##
## "species" is used throughout the code below as the generic name for whatever
## a group's categories are, since the algae case came first; for the UPC groups
## read it as "category".
## ----------------------------------------------------------------------------
algae_y_lab <- expression("Average density (individuals per m"^2*")")
upc_y_lab <- "Average proportion of transect points"

## `order` fixes the sequence categories appear in -- facet layout, legend order
## and colour assignment all follow it, so a category sits in the same place in
## every figure of its group. Groups without an `order` are sorted alphabetically
## by their displayed label. Relief runs smallest to largest, substrate runs
## finest to coarsest and then the non-sediment classes.
##
## Column names are the post-read.csv form, where hyphens have become periods.
## sb_cobble_avg and sb_boulder_avg are listed but no longer exist -- both were
## recorded only in Northern Coast, one of the EXCLUDED BASINS -- so they are
## skipped silently and drop back into their declared slot if that basin returns.
metric_groups <- list(
  list(prefix = "ak_", folder = "algae",      label = "Algae",      y_lab = algae_y_lab),
  list(prefix = "cv_", folder = "cover",      label = "Cover",      y_lab = upc_y_lab),
  list(prefix = "rf_", folder = "relief",     label = "Relief",     y_lab = upc_y_lab,
       order = c("rf_0.10cm_avg", "rf_10cm.1m_avg", "rf_1m.2m_avg",
                 "rf_over_2m_avg")),
  list(prefix = "sb_", folder = "substrate",  label = "Substrate",  y_lab = upc_y_lab,
       order = c("sb_clay.wa_avg", "sb_sand_avg", "sb_pebble.wa_avg",
                 "sb_shell_hash.wa_avg", "sb_cobble.wa_avg", "sb_cobble_avg",
                 "sb_rock_avg", "sb_s_boulder.wa_avg", "sb_l_boulder.wa_avg",
                 "sb_boulder_avg", "sb_reef_avg", "sb_other_avg")),
  list(prefix = "sp_", folder = "superlayer", label = "Superlayer", y_lab = upc_y_lab)
)

group_by_folder <- function(folder) {
  hit <- Filter(function(g) identical(g$folder, folder), metric_groups)
  if (length(hit)) hit[[1]] else NULL
}

## categories excluded from every composite visualization (algae only; the
## setdiff below is a no-op for the four UPC groups). These three are the
## canopy-forming kelps -- outside an understory indicator.
exclude_species <- c("ak_bull_kelp_avg", "ak_feather_boa_n_avg", "ak_giant_kelp_n_avg")

## the "_avg" columns belonging to one group, shared by the site-, basin- and
## sound-scale tables, returned in the group's display order. Anything the group
## declares but that isn't in the data is skipped; anything in the data that the
## group doesn't declare is appended alphabetically rather than dropped, so a new
## category added upstream still shows up somewhere rather than vanishing
group_cols <- function(grp) {
  cols <- grep(paste0("^", grp$prefix, ".*_avg$"), names(df_basin), value = TRUE)
  cols <- setdiff(cols, exclude_species)

  alphabetical <- function(x) x[order(vapply(x, species_label, character(1)))]

  if (is.null(grp$order)) return(alphabetical(cols))
  declared <- intersect(grp$order, cols)
  c(declared, alphabetical(setdiff(cols, declared)))
}

## "ak_bull_kelp_avg" -> "bull kelp"; drops the "_n" used for count-based
## species. read.csv() converts hyphens in column names to periods, so periods
## are turned back into hyphens and only underscores become spaces -- without
## that, "rf_0.10cm" would read "0 10cm" rather than "0-10cm"
species_label <- function(col) {
  label <- sub("^(ak|cv|rf|sb|sp)_", "", col)
  label <- sub("_avg$", "", label)
  label <- sub("_n$", "", label)
  gsub("_", " ", gsub("[.]", "-", label))
}

## scientific name for each retained kelp species, keyed by the column name.
## The UPC categories have no scientific names; facet_labels_for() falls through
## to the common name alone for anything missing here.
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

## adjust this to resize the facet strip titles
facet_label_size <- 9

## Each facet's title as a plotmath expression string (parsed by label_parsed),
## with the scientific name in italics. On one line, "common name (scientific
## name)" is wider than a quarter of the figure for half the kelps -- broad-
## ribbed, three-ribbed, sieve and woody kelp all ran off the edge of their
## strip -- so facet strips put the scientific name on a second line with
## atop(). Titles, which have the whole width, keep the one-line form.
##
## `note`, when given, rides on the first line after the common name -- the
## "(2 of 3 sites)" count, which used to sit inside the panel where the trend's
## edge triangles and the highest points kept landing on it.
facet_labels_for <- function(cols, two_lines = TRUE, note = NULL) {
  common <- vapply(cols, species_label, character(1))
  sci <- species_sci_names[cols]
  has_sci <- !is.na(sci) & sci != ""
  first <- paste0('"', common, '"')
  if (!is.null(note)) first <- ifelse(nzchar(note), paste0(first, '~"', note, '"'), first)
  unname(ifelse(!has_sci,
                first,
                if (two_lines) paste0('atop(', first, ', italic("', sci, '"))')
                else paste0(first, '~"("*italic("', sci, '")*")"')))
}

## basin names aren't guaranteed to be file-path safe (e.g. the "/" in
## "Saratoga/Whidbey_Basin"), so collapse anything outside [A-Za-z0-9_-] to "_"
safe_file_name <- function(x) gsub("[^A-Za-z0-9_-]+", "_", x)

## how a site, basin or region name reads on a figure
place_label <- function(x) {
  x <- as.character(x)
  ifelse(x %in% names(REGION_LABELS), REGION_LABELS[x], gsub("_", " ", x))
}

## the composite average's line colour, and the faded fill used for its standard
## deviation band -- same hue, dropped to a low alpha
composite_color <- "#1963b0"
composite_fill_alpha <- 0.25
member_color <- "gray70"

## ---- REGIONS ---------------------------------------------------------------
## The basins carved out of Puget Sound proper. Deliberately independent of the
## EXCLUDED BASINS filter, which empties when a run is asked to include them --
## this carve-out has to hold either way.
non_puget_basins <- c("Northern Coast",
                      "Western Strait of Juan de Fuca",
                      "Eastern Strait of Juan de Fuca")

#: region -> how it reads in a title
REGION_LABELS <- c(Puget_Sound = "Puget Sound",
                   Washington_wide = "Washington-wide")

#: Which basins belong to a region, given everything present in df_basin.
region_basins <- function(all_basins, focal_region) {
  if (identical(focal_region, "Puget_Sound")) {
    all_basins[!gsub("_", " ", as.character(all_basins)) %in% non_puget_basins]
  } else {
    all_basins
  }
}

## ---- THE CHOICES -------------------------------------------------------------
## label -> value, in the order the app offers them

ANALYSES <- c("Temporal trend"                       = "trend",
              "Average density"                      = "density",
              "Average density with standard deviation" = "density_sd",
              "NMDS Algae"                           = "nmds_algae")

SCALES <- c("Site"        = "site",
            "Basin"       = "basin",
            "Puget Sound" = "puget")

VIEWS <- c("Species within geography"           = "within",
           "Species across geography"           = "across",
           "Species composite within geography" = "composite")

## The NMDS places individual transects, so it has one scale of its own, and
## its third choice is what colours the points rather than how the species are
## laid out
NMDS_SCALES <- c("Transect" = "transect")
NMDS_COLOR_BY <- c("Basin"      = "basin",
                   "Site"       = "site",
                   "Year"       = "year",
                   "Depth zone" = "depth_zone")

is_nmds <- function(analysis) identical(analysis, "nmds_algae")

## The folder each choice writes into under figs/
ANALYSIS_FOLDER <- c(trend = "temporal_trend", density = "density",
                     density_sd = "density_sd", nmds_algae = "nmds_algae")
SCALE_FOLDER <- c(site = "site", basin = "basin", puget = "puget_sound",
                  transect = "transect")

## NULL when the combination makes a figure; otherwise why it does not
view_problem <- function(analysis, scale, view) {
  if (is_nmds(analysis)) {
    if (!identical(scale, "transect"))
      return("the NMDS places individual transects, so its scale is the transect")
    if (!view %in% NMDS_COLOR_BY)
      return("the NMDS colors its points by basin, site, year or depth zone")
    return(NULL)
  }
  if (identical(scale, "transect"))
    return("only the NMDS is drawn transect by transect")
  ## a colouring left over from the NMDS, for the moment the app takes to swap
  ## its controls back, is not a view
  if (!view %in% VIEWS) return("that view belongs to the NMDS")
  if (!scale %in% SCALES) return("that scale belongs to the NMDS")
  if (identical(view, "across") && identical(scale, "puget"))
    return(paste("Puget Sound is a single geography, so there is nothing to lay",
                 "a species out across -- the basin scale shows one species",
                 "across every basin"))
  if (identical(view, "composite") && identical(analysis, "density_sd"))
    return(paste("a standard-deviation band for every species on one set of axes",
                 "would bury them all -- 'Species within geography' gives each",
                 "species its own panel and band"))
  NULL
}

## ---- THE TABLES THE BUILDERS READ ---------------------------------------------
## NULL until load_figure_tables() (or a caller) assigns them.
df_transect <- NULL
df_site     <- NULL
df_basin    <- NULL
df_puget    <- NULL
tr_site     <- NULL
tr_basin    <- NULL
tr_puget    <- NULL
nmds_points  <- NULL
nmds_species <- NULL
nmds_stress  <- NULL

## Reads every table the builders use and puts them where the builders look.
## `paths` is a list with any of: transect, site, basin, puget, trend_site,
## trend_basin, trend_puget, nmds_points, nmds_species, nmds_stress. A missing
## file leaves its table NULL -- the builders
## then say what is missing -- so the density figures still draw before a trend
## table exists. `exclude` names basins to leave out (the EXCLUDED BASINS).
load_figure_tables <- function(paths, exclude = character(0)) {
  rd <- function(key) {
    p <- paths[[key]]
    if (is.null(p) || !nzchar(p) || !file.exists(p)) return(NULL)
    d <- read.csv(p)
    if ("basin" %in% names(d)) d <- d[!gsub("_", " ", d$basin) %in% exclude, , drop = FALSE]
    d
  }
  ## the trend tables carry the category as a value ("ak_broad-ribbed_kelp");
  ## `col` is the matching column name in the density tables after read.csv()
  ## ("ak_broad.ribbed_kelp_avg"), which is what every builder keys on
  with_col <- function(d) {
    if (is.null(d)) return(NULL)
    d$col <- make.names(paste0(d$category, "_avg"))
    d
  }
  df_transect <<- rd("transect")
  df_site     <<- rd("site")
  df_basin    <<- rd("basin")
  df_puget    <<- rd("puget")
  tr_site     <<- with_col(rd("trend_site"))
  tr_basin    <<- with_col(rd("trend_basin"))
  tr_puget    <<- with_col(rd("trend_puget"))
  nmds_points  <<- rd("nmds_points")
  nmds_species <<- rd("nmds_species")
  nmds_stress  <<- rd("nmds_stress")
  invisible(TRUE)
}

## ---- LONG FORMS --------------------------------------------------------------
## Every figure is drawn from two long tables:
##
##   focal    one value per unit (the site, basin or region the scale names),
##            year and column -- the blue line
##   members  what that value was built from -- transects behind a site, sites
##            behind a basin, basins behind a region -- the gray lines
##
## Both carry `status` for the trend (present / lost / new / absent; NA for
## density) so the trend's special cases can be drawn as what they are.

unit_column <- c(site = "site", basin = "basin", puget = "region")
member_noun <- c(site = "transects", basin = "sites", puget = "basins")

focal_long <- function(analysis, scale, cols) {
  key <- unit_column[[scale]]
  if (identical(analysis, "trend")) {
    tbl <- switch(scale, site = tr_site, basin = tr_basin, puget = tr_puget)
    if (is.null(tbl)) return(NULL)
    ## how many members a lost or new value stands for -- sites behind a basin,
    ## basins behind a region -- printed beside its triangle
    unit_word <- if (identical(scale, "puget")) "basins" else "sites"
    n_new  <- if (identical(scale, "site")) NA_integer_ else tbl[[paste0("n_", unit_word, "_new")]]
    n_lost <- if (identical(scale, "site")) NA_integer_ else tbl[[paste0("n_", unit_word, "_lost")]]
    return(tbl %>%
             mutate(.n_new = n_new, .n_lost = n_lost) %>%
             filter(col %in% cols) %>%
             transmute(unit = as.character(.data[[key]]), year = as.integer(year),
                       col, value = trend_pct, sd = NA_real_, status,
                       n_mark = case_when(status == "new" ~ as.integer(.n_new),
                                          status == "lost" ~ as.integer(.n_lost),
                                          TRUE ~ NA_integer_)))
  }
  tbl <- switch(scale, site = df_site, basin = df_basin, puget = df_puget)
  if (is.null(tbl)) return(NULL)
  sd_cols <- intersect(sub("_avg$", "_sd", cols), names(tbl))
  tbl %>%
    transmute(unit = as.character(.data[[key]]), year = as.integer(year),
              across(all_of(c(cols, sd_cols)))) %>%
    pivot_longer(-c(unit, year), names_to = c("col", ".value"),
                 names_pattern = "^(.*)_(avg|sd)$") %>%
    mutate(col = paste0(col, "_avg"), value = avg, status = NA_character_) %>%
    select(unit, year, col, value, sd, status)
}

## basins under each region the regional table holds
by_region <- function(df) {
  regions <- unique(c(if (!is.null(df_puget)) as.character(df_puget$region),
                      if (!is.null(tr_puget)) as.character(tr_puget$region)))
  if (!length(regions)) regions <- "Puget_Sound"
  bind_rows(lapply(regions, function(r)
    df %>%
      filter(basin %in% region_basins(unique(df$basin), r)) %>%
      mutate(unit = r)))
}

member_long <- function(analysis, scale, cols) {
  if (identical(analysis, "trend")) {
    if (identical(scale, "site")) return(NULL)
    if (identical(scale, "basin")) {
      if (is.null(tr_site)) return(NULL)
      return(tr_site %>%
               filter(col %in% cols) %>%
               transmute(unit = as.character(basin), member = as.character(site),
                         year = as.integer(year), col, value = trend_pct, status))
    }
    if (is.null(tr_basin)) return(NULL)
    return(by_region(tr_basin %>%
                       filter(col %in% cols) %>%
                       transmute(basin = as.character(basin), member = basin,
                                 year = as.integer(year), col,
                                 value = trend_pct, status)) %>%
             select(unit, member, year, col, value, status))
  }

  if (identical(scale, "site")) {
    if (is.null(df_transect)) return(NULL)
    raw <- intersect(sub("_avg$", "", cols), names(df_transect))
    return(df_transect %>%
             transmute(unit = as.character(site),
                       member = paste("transect", transect),
                       year = as.integer(year), across(all_of(raw))) %>%
             pivot_longer(all_of(raw), names_to = "col", values_to = "value") %>%
             mutate(col = paste0(col, "_avg"), status = NA_character_))
  }
  if (identical(scale, "basin")) {
    if (is.null(df_site)) return(NULL)
    return(df_site %>%
             transmute(unit = as.character(basin), member = as.character(site),
                       year = as.integer(year), across(all_of(cols))) %>%
             pivot_longer(all_of(cols), names_to = "col", values_to = "value") %>%
             mutate(status = NA_character_))
  }
  if (is.null(df_basin)) return(NULL)
  by_region(df_basin %>%
              transmute(basin = as.character(basin), member = basin,
                        year = as.integer(year), across(all_of(cols))) %>%
              pivot_longer(all_of(cols), names_to = "col", values_to = "value") %>%
              mutate(status = NA_character_)) %>%
    select(unit, member, year, col, value, status)
}

## ---- DRAWING HELPERS ---------------------------------------------------------

## Lines are drawn only between consecutive drawable years: a year with no value
## (not on that year's datasheet) or, for the trend, one that is absent, lost or
## new starts a new segment rather than being bridged. `keys` names the columns
## that identify one line; `.grp` comes back as the group to draw it by.
with_runs <- function(df, keys, drawable) {
  if (is.null(df) || !nrow(df)) return(df)
  df <- df %>%
    group_by(across(all_of(keys))) %>%
    arrange(year, .by_group = TRUE) %>%
    mutate(.run = cumsum(!drawable(status, value))) %>%
    ungroup() %>%
    filter(drawable(status, value))
  df$.grp <- do.call(paste, c(unname(as.list(df[keys])), list(df$.run, sep = "|")))
  ## a one-year run is a point, not a line; geom_path() complains about those
  df$.n <- ave(seq_along(df$.grp), df$.grp, FUN = length)
  df
}

paths_of <- function(df) if (is.null(df)) df else df[df$.n >= 2, , drop = FALSE]

density_drawable <- function(status, value) !is.na(value)
trend_drawable <- function(status, value) status %in% "present"

## "2 of 3 sites" in each panel: members that ever had the category, out of the
## members surveyed for it at all
presence_counts <- function(members, facet_var, analysis) {
  if (is.null(members) || !nrow(members)) return(NULL)
  surveyed <- if (identical(analysis, "trend")) !is.na(members$status) else !is.na(members$value)
  present <- if (identical(analysis, "trend")) {
    members$status %in% c("present", "lost", "new")
  } else !is.na(members$value) & members$value > 0
  members %>%
    mutate(.surveyed = surveyed, .present = present) %>%
    filter(.surveyed) %>%
    group_by(across(all_of(facet_var))) %>%
    summarise(n_all = n_distinct(member),
              n_present = n_distinct(member[.present]),
              .groups = "drop")
}

## "Sites per year: 2022 (1) - 2023 (2)" -- how many members stand behind each
## year's value. The membership changes from year to year (most sites joined the
## survey in 2023-2024), so a basin's line partly reflects which sites were
## surveyed, and this says so on the figure.
per_year_line <- function(members, noun) {
  if (is.null(members) || !nrow(members)) return(NULL)
  n <- members %>%
    filter(!is.na(value) | !is.na(status)) %>%
    group_by(year) %>%
    summarise(n = n_distinct(member), .groups = "drop") %>%
    arrange(year)
  paste0(toupper(substring(noun, 1, 1)), substring(noun, 2), " per year: ",
         paste0(n$year, " (", n$n, ")", collapse = " \u00b7 "))
}

## Trend values are ratios, so they are drawn on a doubling (log2) axis: 50%
## and 200% sit the same distance either side of 100%. Zero ("lost") and
## infinity ("new") have no place on that axis, so they are drawn as triangles
## pinned beyond the finite values -- about a doubling and a half past them,
## which puts them between two labelled ticks rather than on one, where a "lost"
## triangle could be read as, say, 25%.
edge_gap <- 2^1.5

## Site ratios reach 15,600% when a baseline is a single individual, and an axis
## stretched to hold them squeezes every other line flat. Values beyond four
## doublings either side of 100% are drawn at those limits -- and a blue point
## drawn there is labelled with its real value -- so the axis stays readable.
trend_cap <- c(100 / 16, 100 * 16)
squish <- function(v) pmin(pmax(v, trend_cap[1]), trend_cap[2])

trend_edges <- function(values) {
  v <- squish(values[is.finite(values) & values > 0])
  c(lo = min(c(v, 100)) / edge_gap, hi = max(c(v, 100)) * edge_gap)
}

## ticks only where finite values can be -- never out in the margin the lost
## and new triangles occupy
trend_breaks <- function(limits) {
  lo <- max(limits[1], trend_cap[1]); hi <- min(limits[2], trend_cap[2])
  k <- seq(floor(log2(lo / 100)), ceiling(log2(hi / 100)))
  b <- 100 * 2^k
  b <- b[b >= lo * 0.999 & b <= hi * 1.001]
  ## a long run of doublings gets thinned, keeping 100% and every other doubling
  if (length(b) > 6) b <- b[(log2(b / 100) %% 2) == 0]
  b
}

## "1,600%", "12.5%", "6.25%", "0.86%" -- whole numbers from 100% up, three
## significant figures below, never "1.5625%"
pct_label <- function(x) {
  out <- vapply(x, function(v) {
    if (is.na(v)) return(NA_character_)
    ## the axis hands its breaks back through 2^log2(), so 6.25 arrives as
    ## 6.2500000000000009; trimming that first keeps it 6.25
    v <- signif(v, 6)
    num <- if (v >= 100) formatC(round(v), format = "d", big.mark = ",")
           else format(signif(v, 3), trim = TRUE)
    paste0(num, "%")
  }, character(1))
  unname(out)
}

## ggplot2 3.5 renamed scale_*_continuous(trans =) to transform =; the app's own
## environment has a current ggplot2, but the scripts are also run from RStudio
## on whatever a person has installed
log2_y <- function(...) {
  arg <- if (utils::packageVersion("ggplot2") >= "3.5.0") "transform" else "trans"
  do.call(scale_y_continuous, c(setNames(list("log2"), arg), list(...)))
}

trend_y_lab <- "Percent of the preceding years' mean (log scale)"

## The edges for each panel -- or one pair for every panel when the y-axis is
## locked -- and each row's y position: finite values where they are, lost and
## new at the edges.
trend_edge_table <- function(focal, members, y_lock) {
  both <- bind_rows(focal %>% select(.facet, value, status),
                    if (!is.null(members)) members %>% select(.facet, value, status)) %>%
    filter(status == "present")
  facets <- levels(focal$.facet)
  facets <- facets[facets %in% as.character(focal$.facet)]
  if (isTRUE(y_lock)) {
    e <- trend_edges(both$value)
    return(tibble(.facet = factor(facets, levels = levels(focal$.facet)),
                  lo = e[["lo"]], hi = e[["hi"]]))
  }
  bind_rows(lapply(facets, function(f) {
    e <- trend_edges(both$value[as.character(both$.facet) == f])
    tibble(.facet = factor(f, levels = levels(focal$.facet)), lo = e[["lo"]], hi = e[["hi"]])
  }))
}

place_trend <- function(df, edges) {
  if (is.null(df) || !nrow(df)) return(df)
  df %>%
    left_join(edges, by = ".facet") %>%
    mutate(y = case_when(status == "present" ~ squish(value),
                         status == "lost" ~ lo,
                         status == "new" ~ hi,
                         TRUE ~ NA_real_),
           .off = status %in% "present" & !is.na(value) &
             (value < trend_cap[1] | value > trend_cap[2]))
}

## the real value beside a blue point that had to be drawn at the axis limit
off_scale_labels <- function(df, color = composite_color) {
  off <- if (is.null(df)) NULL else df[df$.off %in% TRUE, , drop = FALSE]
  if (is.null(off) || !nrow(off)) return(NULL)
  off$.lab <- pct_label(signif(off$value, 2))
  off$.vj <- ifelse(off$value > 100, -0.9, 1.9)
  geom_text(data = off, aes(x = .x, y = y, label = .lab, vjust = .vj),
            size = 2.8, color = color, fontface = 2)
}

## what the blue line, gray lines and band are, in words, at the foot of the
## figure -- a figure that travels without its caption still says what it shows
caption_for <- function(analysis, scale, view, squished = FALSE) {
  mean_of <- switch(scale,
    site  = "the site's mean over its transects",
    basin = "the basin's mean of its site means",
    puget = "the mean of the basin means, each basin weighted equally")
  gray <- switch(scale, site = "individual transects", basin = "individual sites",
                 puget = "individual basins")
  if (identical(view, "composite")) {
    what <- if (identical(analysis, "trend")) "temporal trend" else mean_of
    line <- sprintf("Each line is one category's %s.", what)
  } else if (identical(analysis, "density")) {
    line <- sprintf("Blue: %s. Gray: %s.", mean_of, gray)
  } else if (identical(analysis, "density_sd")) {
    line <- sprintf("Blue: %s. Band: \u00b1 1 standard deviation among %s (lower edge clamped at 0).",
                    mean_of, member_noun[[scale]])
  } else {
    line <- switch(scale,
      site  = "Line: the site's density as a percentage of its own mean over up to five preceding years.",
      basin = paste("Blue: the basin's density as a percentage of the same sites' mean over up to five",
                    "preceding years. Gray: individual sites."),
      puget = paste("Blue: the region's density (its basins weighted equally) as a percentage of",
                    "the preceding years' mean. Gray: individual basins."))
  }
  if (identical(analysis, "trend")) {
    line <- paste(c(line, "Dashed: 100% (no change).",
                    "Triangles on the bottom edge: lost (in the baseline, not this year);",
                    "on the top edge: new (not in the baseline).",
                    if (!identical(scale, "site"))
                      sprintf("The number beside a triangle is how many %s it stands for.",
                              if (identical(scale, "puget")) "basins" else "sites")),
                  collapse = " ")
    if (isTRUE(squished)) {
      line <- paste(line, sprintf("Values beyond %s-%s are drawn at those limits%s.",
                                  pct_label(trend_cap[1]), pct_label(trend_cap[2]),
                                  if (identical(view, "composite")) ", labelled with their value"
                                  else "; blue ones are labelled with their value"))
    }
  }
  if (!identical(view, "composite") && !(identical(analysis, "trend") && identical(scale, "site"))) {
    line <- paste(line, sprintf("'(2 of 3 %s)': %s where the category was recorded at least once, of those surveyed.",
                                member_noun[[scale]], member_noun[[scale]]))
  }
  paste(strwrap(line, width = 175), collapse = "\n")
}

analysis_label <- function(analysis) names(ANALYSES)[match(analysis, ANALYSES)]

## ---- ONE FIGURE -----------------------------------------------------------------

#: Build one figure. `where` is the site, basin or region for the within and
#: composite views, and the basin whose sites are compared for the across view
#: at the site scale. `category` is the species column for the across view.
#:
#: For the NMDS, `view` is what colours the points (NMDS_COLOR_BY), `where` the
#: basin whose sites are coloured when that is "site", and `category` the
#: species whose density sizes the points (or "total_density"); `arrows` and
#: `ellipses` switch the species arrows and the group ellipses.
make_figure <- function(analysis, scale, view, grp, where = NULL, category = NULL,
                        y_lock = FALSE, arrows = TRUE, ellipses = TRUE) {
  fail <- function(...) list(ok = FALSE, reason = paste0(...))

  problem <- view_problem(analysis, scale, view)
  if (!is.null(problem)) return(fail(problem))
  if (is_nmds(analysis)) {
    return(draw_nmds(view, basin = where, size_by = category, arrows = arrows,
                     ellipses = ellipses))
  }
  if (is.null(df_basin)) return(fail("the basin-scale table is missing -- run step 2"))

  tbl_missing <- if (identical(analysis, "trend")) {
    is.null(switch(scale, site = tr_site, basin = tr_basin, puget = tr_puget))
  } else {
    is.null(switch(scale, site = df_site, basin = df_basin, puget = df_puget))
  }
  if (tbl_missing) {
    return(fail(sprintf("the %s-scale %s table is missing -- run step 2",
                        tolower(names(SCALES)[match(scale, SCALES)]),
                        if (identical(analysis, "trend")) "trend" else "metrics")))
  }

  cols <- if (identical(view, "across")) category else group_cols(grp)
  if (!length(cols) || any(!nzchar(cols))) return(fail("no categories to draw"))

  focal <- focal_long(analysis, scale, cols)
  members <- member_long(analysis, scale, cols)

  if (identical(view, "across")) {
    units <- if (identical(scale, "site")) {
      sort(unique(as.character(df_site$site[as.character(df_site$basin) == where])))
    } else {
      sort(unique(as.character(df_basin$basin)))
    }
    focal <- focal %>% filter(unit %in% units)
    if (!is.null(members)) members <- members %>% filter(unit %in% units)
    if (!nrow(focal)) {
      return(fail("nothing recorded for ", species_label(category),
                  if (!is.null(where)) paste(" in", place_label(where))))
    }
    return(list(ok = TRUE, plot = draw_facets(
      analysis, scale, "across", focal, members,
      facet_var = "unit", levels = units,
      labels = function(note) ifelse(nzchar(note), paste(place_label(units), note),
                                     place_label(units)),
      parsed = FALSE,
      title = parse(text = facet_labels_for(category, two_lines = FALSE)),
      grp = grp, y_lock = y_lock)))
  }

  focal <- focal %>% filter(unit == where)
  if (!is.null(members)) members <- members %>% filter(unit == where)
  if (!nrow(focal) || all(is.na(focal$value) & is.na(focal$status))) {
    return(fail("nothing recorded for ", place_label(where),
                if (identical(analysis, "trend"))
                  " -- a trend needs at least one earlier survey to compare with" else ""))
  }

  if (identical(view, "composite")) {
    return(list(ok = TRUE, plot = draw_composite(analysis, scale, focal, members, cols,
                                                 grp, title = place_label(where))))
  }
  list(ok = TRUE, plot = draw_facets(
    analysis, scale, "within", focal, members,
    facet_var = "col", levels = cols,
    labels = function(note) facet_labels_for(cols, note = note), parsed = TRUE,
    title = place_label(where), grp = grp, y_lock = y_lock))
}

## One panel per species (within) or per place (across). `labels` is a function
## of each panel's "(2 of 3 sites)" note, in `levels` order, returning the strip
## titles.
draw_facets <- function(analysis, scale, view, focal, members, facet_var, levels,
                        labels, parsed, title, grp, y_lock) {
  trend <- identical(analysis, "trend")
  noun <- member_noun[[scale]]

  counts <- presence_counts(members, facet_var, analysis)
  note <- rep("", length(levels))
  if (!is.null(counts)) {
    at <- match(levels, counts[[facet_var]])
    note[!is.na(at)] <- sprintf("(%d of %d %s)", counts$n_present[at[!is.na(at)]],
                                counts$n_all[at[!is.na(at)]], noun)
  }
  labels <- labels(note)
  subtitle <- paste(analysis_label(analysis), "\u00b7",
    if (identical(view, "across")) {
      sprintf("one panel per %s", if (identical(scale, "site")) "site" else "basin")
    } else if (trend && identical(scale, "site")) {
      "each year compared with the site's earlier surveys"
    } else per_year_line(members, noun))

  ## the SD band takes the gray lines' place, so the band isn't lost behind them
  members_drawn <- if (identical(analysis, "density_sd")) NULL else members

  as_facet <- function(df) {
    if (is.null(df) || !nrow(df)) return(NULL)
    df %>% mutate(.facet = factor(.data[[facet_var]], levels = levels, labels = labels))
  }
  focal <- as_facet(focal)
  members_drawn <- as_facet(members_drawn)

  yrs <- sort(unique(c(focal$year, members_drawn$year)))
  with_x <- function(df) if (is.null(df)) NULL else mutate(df, .x = factor(year, levels = yrs))
  focal <- with_x(focal)
  members_drawn <- with_x(members_drawn)

  p <- ggplot()

  if (trend) {
    edges <- trend_edge_table(focal, members_drawn, y_lock)
    focal <- place_trend(focal, edges)
    members_drawn <- place_trend(members_drawn, edges)
    blank <- edges %>% pivot_longer(c(lo, hi), values_to = "y")

    p <- p +
      geom_blank(data = blank, aes(y = y)) +
      geom_hline(yintercept = 100, linetype = "dashed", linewidth = 0.5, color = "gray45")
    if (!is.null(members_drawn)) {
      member_lines <- with_runs(members_drawn, c(".facet", "member", "col"), trend_drawable)
      ## every site lost (or new) in the same year lands on the same spot, so
      ## one triangle there stood for anything from one site to many; it is
      ## drawn once, with the number of sites beside it
      member_marks <- members_drawn %>%
        filter(status %in% c("lost", "new")) %>%
        count(.facet, .x, status, y, name = "n")
      p <- p +
        geom_path(data = paths_of(member_lines), aes(x = .x, y = y, group = .grp),
                  color = member_color, linewidth = 0.6, alpha = 0.6) +
        geom_point(data = member_lines, aes(x = .x, y = y),
                   color = member_color, size = 1, alpha = 0.7) +
        geom_point(data = member_marks, aes(x = .x, y = y, shape = status),
                   color = "gray55", fill = member_color, size = 1.8, alpha = 0.8) +
        geom_text(data = member_marks, aes(x = .x, y = y, label = n),
                  nudge_x = 0.1, hjust = 0, size = 2.8, color = "gray25")
    }
    focal_lines <- with_runs(focal, c(".facet", "unit", "col"), trend_drawable)
    focal_marks <- filter(focal, status %in% c("lost", "new"))
    p <- p +
      geom_path(data = paths_of(focal_lines), aes(x = .x, y = y, group = .grp),
                color = composite_color, linewidth = 1.3) +
      geom_point(data = focal_lines, aes(x = .x, y = y), color = composite_color, size = 2) +
      geom_point(data = focal_marks, aes(x = .x, y = y, shape = status),
                 color = composite_color, fill = composite_color, size = 2.8) +
      off_scale_labels(focal) +
      scale_shape_manual(values = c(lost = 25, new = 24), guide = "none") +
      log2_y(breaks = trend_breaks, labels = pct_label, expand = expansion(mult = 0.06))
    y_lab <- trend_y_lab
    squished <- any(focal$.off %in% TRUE) || any(members_drawn$.off %in% TRUE)
  } else {
    if (identical(analysis, "density_sd")) {
      band <- focal %>%
        mutate(lower = pmax(value - coalesce(sd, 0), 0), upper = value + coalesce(sd, 0))
      band <- with_runs(band, c(".facet", "unit", "col"), density_drawable)
      p <- p + geom_ribbon(data = band, aes(x = .x, ymin = lower, ymax = upper, group = .grp),
                           fill = composite_color, alpha = composite_fill_alpha)
    } else if (!is.null(members_drawn)) {
      member_lines <- with_runs(members_drawn, c(".facet", "member", "col"), density_drawable)
      p <- p +
        geom_path(data = paths_of(member_lines), aes(x = .x, y = value, group = .grp),
                  color = member_color, linewidth = 0.6, alpha = 0.6) +
        geom_point(data = member_lines, aes(x = .x, y = value),
                   color = member_color, size = 1, alpha = 0.6)
    }
    focal_lines <- with_runs(focal, c(".facet", "unit", "col"), density_drawable)
    ## every panel's axis starts at 0, including a category that was never
    ## recorded, which would otherwise get a meaningless -0.05..0.05 axis
    floor_blank <- focal %>% distinct(.facet) %>% mutate(y = 0)
    p <- p +
      geom_blank(data = floor_blank, aes(y = y)) +
      geom_path(data = paths_of(focal_lines), aes(x = .x, y = value, group = .grp),
                color = composite_color, linewidth = 1.3) +
      geom_point(data = focal_lines, aes(x = .x, y = value), color = composite_color, size = 2) +
      scale_y_continuous(expand = expansion(mult = c(0.02, 0.08)))
    y_lab <- grp$y_lab
    squished <- FALSE
  }

  p +
    scale_x_discrete(drop = FALSE) +
    facet_wrap(~.facet, scales = if (isTRUE(y_lock)) "fixed" else "free_y", ncol = 4,
               labeller = if (parsed) label_parsed else label_value) +
    my.theme +
    labs(y = y_lab, title = title, subtitle = subtitle,
         caption = caption_for(analysis, scale, view, squished)) +
    theme(
      axis.text.x = element_text(angle = 0, size = 9, hjust = 0.5),
      axis.title.x = element_blank(),
      axis.text.y = element_text(size = 9),
      axis.title.y = element_text(size = 12, hjust = 0.5),
      strip.text = element_text(size = facet_label_size, lineheight = 0.9,
                                margin = margin(3, 2, 3, 2)),
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5, size = 10.5, colour = "gray30"),
      plot.caption = element_text(hjust = 0, size = 8.5, colour = "gray35",
                                  lineheight = 1.05),
      plot.caption.position = "plot",
      legend.position = "none")
}

## every category for one place on a single set of axes, one coloured line each,
## so the categories can be compared directly against each other
draw_composite <- function(analysis, scale, focal, members, cols, grp, title) {
  trend <- identical(analysis, "trend")
  ## common name only in this legend -- the faceted figures pair it with the
  ## scientific name, but ten of those make the key too wide to sit beside a
  ## single panel. Named by column, so labels match the levels by name.
  lab <- vapply(cols, species_label, character(1))
  yrs <- sort(unique(focal$year))
  focal <- focal %>% mutate(col = factor(col, levels = cols), .x = factor(year, levels = yrs))

  p <- ggplot()
  squished <- FALSE
  if (trend) {
    e <- trend_edges(focal$value[focal$status == "present"])
    focal <- focal %>%
      mutate(y = case_when(status == "present" ~ squish(value), status == "lost" ~ e[["lo"]],
                           status == "new" ~ e[["hi"]], TRUE ~ NA_real_),
             .off = status %in% "present" & !is.na(value) &
               (value < trend_cap[1] | value > trend_cap[2]))
    squished <- any(focal$.off)
    lines <- with_runs(focal, "col", trend_drawable)
    ## Every category lost (or new) in the same year would land on one spot and
    ## hide all but the last, so they are spread sideways around their year,
    ## each with the number of sites (or basins) it stands for beside it.
    marks <- focal %>%
      filter(status %in% c("lost", "new")) %>%
      group_by(.x, status) %>%
      mutate(.xn = as.numeric(.x) + (rank(as.integer(col)) - (n() + 1) / 2) * 0.17) %>%
      ungroup()
    p <- p +
      geom_blank(data = tibble(y = unname(e)), aes(y = y)) +
      geom_hline(yintercept = 100, linetype = "dashed", linewidth = 0.5, color = "gray45") +
      geom_path(data = paths_of(lines), aes(x = .x, y = y, group = .grp, color = col),
                linewidth = 1.1) +
      geom_point(data = lines, aes(x = .x, y = y, color = col), size = 2.4) +
      geom_point(data = marks, aes(x = .xn, y = y, color = col, fill = col, shape = status),
                 size = 2.8) +
      (if (any(!is.na(marks$n_mark)))
        geom_text(data = filter(marks, !is.na(n_mark)), aes(x = .xn, y = y, label = n_mark),
                  nudge_x = 0.06, hjust = 0, size = 2.7, color = "gray25")) +
      off_scale_labels(focal, color = "gray20") +
      scale_shape_manual(values = c(lost = 25, new = 24), guide = "none") +
      scale_fill_brewer(palette = "Paired", labels = lab, guide = "none", drop = FALSE) +
      log2_y(breaks = trend_breaks, labels = pct_label, expand = expansion(mult = 0.06))
    y_lab <- trend_y_lab
  } else {
    lines <- with_runs(focal, "col", density_drawable)
    p <- p +
      geom_path(data = paths_of(lines), aes(x = .x, y = value, group = .grp, color = col),
                linewidth = 1.1) +
      geom_point(data = lines, aes(x = .x, y = value, color = col), size = 2.4) +
      expand_limits(y = 0)
    y_lab <- grp$y_lab
  }
  subtitle <- paste(analysis_label(analysis), "\u00b7",
                    if (trend && identical(scale, "site"))
                      "each year compared with the site's earlier surveys"
                    else per_year_line(members, member_noun[[scale]]))

  p +
    scale_x_discrete(drop = FALSE) +
    scale_color_brewer(palette = "Paired", labels = lab, drop = FALSE) +
    my.theme +
    labs(y = y_lab, title = title, subtitle = subtitle, color = NULL,
         caption = caption_for(analysis, scale, "composite", squished)) +
    theme(
      axis.title.x = element_blank(),
      axis.text = element_text(size = 11),
      axis.title.y = element_text(size = 13),
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5, size = 10.5, colour = "gray30"),
      plot.caption = element_text(hjust = 0, size = 8.5, colour = "gray35"),
      plot.caption.position = "plot",
      legend.text = element_text(size = 10),
      legend.key.width = unit(1.4, "lines"),
      legend.position = "right")
}

## ---- NMDS --------------------------------------------------------------------------
## Drawn in the style of the ROV-diver comparison report
## (CCR_benthic_analyses/ROV_diver_comparison/code/NMDS_visualization_functions.R):
## faded points coloured by group, a 95% ellipse per group, black arrows to the
## species scores with repelled white labels, and NMDS1 and NMDS2 on one scale
## (coord_fixed) so distances read the same in both directions.

## A palette for n groups: Dark2 while it lasts, then Paired, then evenly spaced
## hues for the largest basins' site lists
qualitative <- function(n) {
  if (n <= 8) RColorBrewer::brewer.pal(max(3, n), "Dark2")[seq_len(n)]
  else if (n <= 12) RColorBrewer::brewer.pal(n, "Paired")
  else scales::hue_pal(l = 55)(n)
}

## the transect column a size choice refers to: a species ("ak_sugar_kelp",
## or the "_avg" form the other figures use) or the total of all of them
nmds_size_column <- function(size_by) {
  if (is.null(size_by) || !nzchar(size_by)) return(NULL)
  if (identical(size_by, "total_density")) return("total_density")
  make.names(sub("_avg$", "", size_by))
}

nmds_size_label <- function(size_by) {
  if (identical(size_by, "total_density")) "all understory algae" else species_label(size_by)
}

## The species a NMDS figure can be sized by, label -> value, for the app
nmds_size_choices <- function() {
  if (is.null(nmds_species)) return(character(0))
  spp <- as.character(nmds_species$category)
  c(setNames(spp, vapply(spp, species_label, character(1))),
    "All understory algae (total)" = "total_density")
}

## the scale the size legend reads in: densities per m^2, square-root spaced
## so a transect with a little of the species is still visibly bigger than one
## with none
size_scale <- function(name) {
  arg <- if (utils::packageVersion("ggplot2") >= "3.5.0") "transform" else "trans"
  do.call(scale_size, c(setNames(list("sqrt"), arg),
                        list(range = c(0.4, 6.5), name = name)))
}

draw_nmds <- function(color_by, basin = NULL, size_by = NULL, arrows = TRUE,
                      ellipses = TRUE) {
  fail <- function(...) list(ok = FALSE, reason = paste0(...))
  if (is.null(nmds_points) || is.null(nmds_species) || is.null(nmds_stress))
    return(fail("the NMDS tables are missing -- run step 2"))

  pts <- nmds_points
  stress2 <- nmds_stress$stress[nmds_stress$k == 2]
  info <- nmds_stress[nmds_stress$k == 2, , drop = FALSE]

  focus <- rep(TRUE, nrow(pts))
  if (identical(color_by, "basin")) {
    pts$.group <- place_label(pts$basin)
    legend_title <- "Basin"
  } else if (identical(color_by, "site")) {
    if (is.null(basin) || !basin %in% pts$basin)
      return(fail("choose the basin whose sites to color"))
    focus <- as.character(pts$basin) == basin
    pts$.group <- ifelse(focus, place_label(pts$site), NA_character_)
    legend_title <- paste0("Site (", place_label(basin), ")")
  } else if (identical(color_by, "year")) {
    pts$.group <- as.character(pts$year)
    legend_title <- "Year"
  } else {
    pts$.group <- c(offshore = "Offshore (transects 1-3)",
                    inshore  = "Inshore (transects 4-6)")[as.character(pts$depth_zone)]
    legend_title <- "Depth zone"
  }

  groups <- sort(unique(pts$.group[focus & !is.na(pts$.group)]))
  if (identical(color_by, "depth_zone")) groups <- intersect(c("Offshore (transects 1-3)",
                                                              "Inshore (transects 4-6)"), groups)
  colours <- switch(color_by,
    year = setNames(scales::viridis_pal(end = 0.88)(length(groups)), groups),
    ## blue deeper, orange shallower -- the ROV-diver report's convention
    depth_zone = setNames(c("#3182BD", "#E6550D")[seq_along(groups)], groups),
    setNames(qualitative(length(groups)), groups))
  fg <- pts[focus, , drop = FALSE]
  fg$.group <- factor(fg$.group, levels = groups)

  size_col <- nmds_size_column(size_by)
  if (!is.null(size_col) && !size_col %in% names(pts))
    return(fail("no density column for ", nmds_size_label(size_by)))

  p <- ggplot(mapping = aes(x = NMDS1, y = NMDS2))

  ## the other basins' transects stay in view, in gray, so a basin's sites are
  ## seen against the whole ordination
  if (!all(focus)) {
    p <- p + geom_point(data = pts[!focus, , drop = FALSE], color = "gray78",
                        alpha = 0.45, size = 1.1, shape = 16)
  }

  if (is.null(size_col)) {
    p <- p + geom_point(data = fg, aes(color = .group), size = 1.6, alpha = 0.4, shape = 16)
  } else {
    ## largest on top would hide the rest; smallest on top keeps every point visible
    fg <- fg[order(-fg[[size_col]]), , drop = FALSE]
    p <- p +
      geom_point(data = fg, aes(color = .group, size = .data[[size_col]]),
                 alpha = 0.45, shape = 16) +
      size_scale(paste0(nmds_size_label(size_by), "\n(per m\u00b2)"))
  }

  ## ellipses for groups with enough transects to estimate one
  if (isTRUE(ellipses)) {
    ell <- fg %>% group_by(.group) %>% filter(n() >= 5) %>% ungroup()
    if (nrow(ell)) {
      ## normal-theory rather than stat_ellipse()'s default multivariate t: the
      ## t fit is iterative and does not always converge on these groups
      p <- p + stat_ellipse(data = ell, aes(color = .group, group = .group),
                            type = "norm", level = 0.95, linewidth = 1)
    }
  }

  if (isTRUE(arrows)) {
    spp <- nmds_species
    spp$.label <- vapply(as.character(spp$category), species_label, character(1))
    p <- p +
      geom_segment(data = spp, aes(x = 0, y = 0, xend = NMDS1, yend = NMDS2),
                   linewidth = 0.8, color = "black",
                   arrow = arrow(length = unit(0.2, "cm"))) +
      ggrepel::geom_label_repel(data = spp, aes(x = NMDS1, y = NMDS2, label = .label),
                                size = 3.2, fontface = "bold", fill = "white",
                                max.overlaps = Inf, min.segment.length = 0,
                                segment.color = "gray40", seed = 42)
  }

  n_out <- info$n_no_understory_algae + info$n_no_algae_survey
  subtitle <- paste0("Bray-Curtis on Wisconsin-standardized square-root densities \u00b7 ",
                     sprintf("2-D stress %.3f", stress2), " \u00b7 ",
                     format(info$n_transects, big.mark = ","), " transects",
                     if (!all(focus)) paste0(" \u00b7 sites of ", place_label(basin),
                                             " in color, the rest in gray"))
  ## c() then collapse, not paste(): paste() turns a skipped (NULL) sentence
  ## into "" and leaves a double space where it would have been
  caption <- paste(c(
    sprintf("Each point is one 30 x 2 m transect; %d with no understory algae and %d without an algae survey cannot be placed and are left out.",
            info$n_no_understory_algae, info$n_no_algae_survey),
    if (isTRUE(arrows)) "Arrows point to each species' score: the weighted average of the transects it occurs on.",
    if (isTRUE(ellipses)) "Ellipses: 95% normal-theory ellipses for each group of 5 or more transects.",
    if (!is.null(size_col)) sprintf("Point size: %s density on the transect.", nmds_size_label(size_by)),
    "Stress under 0.2 gives a usable two-dimensional picture (Clarke 1993); fine detail should not be over-read."),
    collapse = " ")
  caption <- paste(strwrap(caption, width = 175), collapse = "\n")

  p <- p +
    scale_color_manual(values = colours, name = legend_title, drop = FALSE) +
    guides(color = guide_legend(override.aes = list(alpha = 1, size = 3, linewidth = 0),
                                order = 1)) +
    coord_fixed() +
    labs(x = "NMDS1", y = "NMDS2", title = "Transect-level NMDS: understory algae",
         subtitle = subtitle, caption = caption) +
    my.theme +
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          plot.title = element_text(size = 15),
          plot.subtitle = element_text(size = 10.5, colour = "gray30"),
          plot.caption = element_text(hjust = 0, size = 8.5, colour = "gray35",
                                      lineheight = 1.05),
          plot.caption.position = "plot",
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 11),
          legend.key = element_blank())
  list(ok = TRUE, plot = p)
}

## The NMDS figures Run writes: every colouring, plain; each basin's sites; and
## the basin colouring sized by each species and by the total
nmds_targets <- function(view) {
  if (is.null(nmds_points)) return(list())
  switch(view,
    basin = c(list(list(where = NULL, category = NULL)),
              lapply(unname(nmds_size_choices()), function(s) list(where = NULL, category = s))),
    site = lapply(sort(unique(as.character(nmds_points$basin))),
                  function(b) list(where = b, category = NULL)),
    list(list(where = NULL, category = NULL)))
}

## ---- WHERE A FIGURE BELONGS ------------------------------------------------------
## figs/<analysis>/<scale>/<view>/<group>/<name>.png, e.g.
##   figs/density/basin/within/algae/Hood_Canal.png
##   figs/temporal_trend/puget_sound/composite/algae/Puget_Sound.png
##   figs/density_sd/basin/across/algae/sugar_kelp.png
##   figs/density/site/across/algae/Hood_Canal_sugar_kelp.png
##   figs/nmds_algae/transect/basin/algae/all_sized_by_sugar_kelp.png
figure_file <- function(analysis, scale, view, grp, where = NULL, category = NULL) {
  if (is_nmds(analysis)) {
    stem <- if (identical(view, "site")) safe_file_name(where) else "all"
    if (!is.null(category) && nzchar(category)) {
      stem <- paste0(stem, "_sized_by_", safe_file_name(nmds_size_label(category)))
    }
    return(file.path(ANALYSIS_FOLDER[[analysis]], SCALE_FOLDER[[scale]], view,
                     grp$folder, paste0(stem, ".png")))
  }
  stem <- if (identical(view, "across")) {
    species <- safe_file_name(species_label(category))
    if (identical(scale, "site")) paste0(safe_file_name(where), "_", species) else species
  } else {
    safe_file_name(where)
  }
  file.path(ANALYSIS_FOLDER[[analysis]], SCALE_FOLDER[[scale]], view, grp$folder,
            paste0(stem, ".png"))
}

## Every (where, category) a given figure set covers, as a list of
## list(where =, category =) -- what visualizations.R loops over and what the
## app counts to say how many files a selection will write.
figure_targets <- function(analysis, scale, view, grp) {
  if (!is.null(view_problem(analysis, scale, view))) return(list())
  if (is_nmds(analysis)) return(if (identical(grp$folder, "algae")) nmds_targets(view) else list())
  if (is.null(df_basin)) return(list())
  places <- switch(scale,
    site  = if (is.null(df_site)) character(0) else sort(unique(as.character(df_site$site))),
    basin = sort(unique(as.character(df_basin$basin))),
    puget = if (is.null(df_puget)) character(0) else unique(as.character(df_puget$region)))
  if (!identical(view, "across")) {
    return(lapply(places, function(w) list(where = w, category = NULL)))
  }
  cols <- group_cols(grp)
  if (identical(scale, "site")) {
    basins <- sort(unique(as.character(df_basin$basin)))
    out <- list()
    for (b in basins) for (cc in cols) out[[length(out) + 1]] <- list(where = b, category = cc)
    return(out)
  }
  lapply(cols, function(cc) list(where = NULL, category = cc))
}

## The (scale, view) pairs Run draws for an analysis, given the scale and view
## boxes ticked. The NMDS has a fixed set of its own -- the transect scale and
## every colouring -- whatever those boxes say, since neither applies to it.
figure_sets <- function(analysis, scales, views) {
  if (is_nmds(analysis)) {
    return(lapply(unname(NMDS_COLOR_BY), function(v) list(scale = "transect", view = v)))
  }
  out <- list()
  for (s in scales) for (v in views) {
    if (is.null(view_problem(analysis, s, v))) out[[length(out) + 1]] <- list(scale = s, view = v)
  }
  out
}
