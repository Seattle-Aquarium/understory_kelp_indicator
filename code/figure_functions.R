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
##             nmds_algae, dbrda_algae
##                         the two ordinations of the algae community, one
##                         point per transect -- see ORDINATIONS below
##             glmm_algae  each alga's count against each substrate class --
##                         see GLMMs below
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

## ---- HOVER ------------------------------------------------------------------
## In the app, the lines and points behind a figure say what they are when the
## mouse is over them, and the line or site under it stands out. The app turns
## this on with options(ukelp.hover = TRUE) and draws through ggiraph; Run
## never does, so the saved PNGs are the plain figures, layer for layer.
hover_on <- function() isTRUE(getOption("ukelp.hover")) && requireNamespace("ggiraph", quietly = TRUE)

## a ggplot2 layer, or its ggiraph twin carrying `hover` (tooltip, data_id)
## when hover is on: `fun` is the ggplot2 name, "geom_path" or "geom_point"
hover_layer <- function(fun, mapping, hover, ...) {
  if (!hover_on()) return(getExportedValue("ggplot2", fun)(mapping = mapping, ...))
  both <- c(unclass(mapping), unclass(hover))
  class(both) <- class(mapping)
  getExportedValue("ggiraph", paste0(fun, "_interactive"))(mapping = both, ...)
}

## each value to 3 significant figures on its own ("1.9", not "1.90000")
hover_num <- function(v) vapply(signif(v, 3), function(z) format(z, trim = TRUE), character(1))

## "0.42 per m\u00b2", "12% of points" or "180% of the preceding years' mean"
hover_value <- function(analysis, grp, v) {
  ifelse(is.na(v), "no value",
         if (identical(analysis, "trend")) paste(pct_label(signif(v, 3)), "of the preceding years' mean")
         else if (identical(grp$prefix, "ak_")) paste(hover_num(v), "per m\u00b2")
         else paste0(hover_num(100 * v), "% of points"))
}
## The app's interactive figure, the size of the saved PNG. `kind`:
##   lines       a hovered gray line and its points turn Reef Check's light
##               blue; the other lines stay as they are
##   composite   a hovered category's line thickens in its own colour
##   ordination  a hovered point outlines every transect of its site in their
##               own colour, a hovered legend entry picks out its group, and
##               everything else fades to 20%
##   glmm        a hovered site on the between-site panel fills light blue
hover_colour <- "#00C4B3"   # Reef Check's light blue, Pantone 3265 C, as in the app
hover_widget <- function(p, kind = c("lines", "composite", "ordination", "glmm")) {
  kind <- match.arg(kind)
  css <- if (kind == "ordination") {
    ggiraph::girafe_css(css = "", point = "stroke:#111111;stroke-width:1.4px;stroke-opacity:1;fill-opacity:1;r:4.5px;")
  } else if (kind == "glmm") {
    ggiraph::girafe_css(css = "", point = sprintf(
      "fill:%s;stroke:#003A70;stroke-width:1.5px;fill-opacity:1;stroke-opacity:1;", hover_colour))
  } else if (kind == "composite") {
    ggiraph::girafe_css(css = "", line = "stroke-width:3.5px;stroke-opacity:1;",
                        point = "fill-opacity:1;stroke-opacity:1;r:4px;")
  } else {
    ggiraph::girafe_css(css = "",
      line = sprintf("stroke:%s;stroke-width:3px;stroke-opacity:1;", hover_colour),
      point = sprintf("fill:%s;stroke:%s;fill-opacity:1;stroke-opacity:1;r:3.5px;", hover_colour, hover_colour))
  }
  tooltip <- paste0("background:#ffffff;color:#1f2933;border:1px solid #c9d2dc;border-radius:6px;",
                    "padding:6px 9px;font-family:'Segoe UI',Arial,sans-serif;font-size:13px;",
                    "line-height:1.35;box-shadow:0 2px 8px rgba(0,0,0,.18);")
  g <- ggiraph::girafe(ggobj = p, width_svg = fig_width, height_svg = fig_height,
                  options = list(ggiraph::opts_hover(css = css),
                                 ggiraph::opts_hover_inv(css = if (kind == "ordination") "opacity:0.2;" else ""),
                                 ggiraph::opts_hover_key(css = "stroke:#111111;stroke-width:1px;"),
                                 ggiraph::opts_tooltip(css = tooltip, use_fill = FALSE, offx = 14, offy = 14),
                                 ggiraph::opts_sizing(rescale = TRUE),
                                 ggiraph::opts_toolbar(hidden = c("selection", "zoom", "misc"))))
  ## ggiraph keeps legend keys apart from the plot's points, so a legend
  ## entry (key or name) is tied to its group's points here: hovering it
  ## brings them up, outlined, and fades the rest
  htmlwidgets::onRender(g, "function(el) {
    const svg = el.querySelector('svg'); if (!svg) return;
    const marks = [...svg.querySelectorAll('[data-id]')];
    const clear = () => marks.forEach(m => { m.style.opacity = ''; m.style.fillOpacity = '';
      m.style.stroke = ''; m.style.strokeWidth = ''; m.style.strokeOpacity = ''; });
    svg.querySelectorAll('[key-id]').forEach(k => {
      k.style.cursor = 'pointer';
      k.addEventListener('mouseenter', () => {
        const id = k.getAttribute('key-id');
        marks.forEach(m => {
          if (m.getAttribute('data-id') === id) { m.style.fillOpacity = '1'; m.style.stroke = '#111111';
            m.style.strokeWidth = '1px'; m.style.strokeOpacity = '1'; }
          else m.style.opacity = '0.2';
        });
      });
      k.addEventListener('mouseleave', clear);
    });
  }")
}

## one tooltip per row: the name in bold, then a line for each of `...`
hover_tip <- function(name, ...) {
  out <- paste0("<b>", name, "</b>")
  for (line in Filter(Negate(is.null), list(...))) out <- paste0(out, "<br>", line)
  out
}

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
              "NMDS Algae"                           = "nmds_algae",
              "db-RDA Algae ~ Substrate"             = "dbrda_algae",
              "GLMM Algae ~ Substrate"               = "glmm_algae")

SCALES <- c("Site"        = "site",
            "Basin"       = "basin",
            "Puget Sound" = "puget")

VIEWS <- c("Species within geography"           = "within",
           "Species across geography"           = "across",
           "Species composite within geography" = "composite")

## The ordinations -- the NMDS of the algae community, and the db-RDA of the
## same community constrained by substrate -- place individual transects, so
## they have one scale of their own, and their third choice is what colours the
## points rather than how the species are laid out
ORDINATION_SCALES <- c("Transect" = "transect")
ORDINATION_COLOR_BY <- c("Basin"      = "basin",
                         "Site"       = "site",
                         "Year"       = "year",
                         "Depth zone" = "depth_zone")

## The GLMMs model individual transects too; their third choice is which
## figure. Each grid of rate ratios comes between sites, within sites, or the
## two stacked: one half on its own is easier to take in, on a slide say.
GLMM_VIEWS <- c("Rate ratios, individual UPC classes: between sites"     = "rates_individual_between",
                "Rate ratios, individual UPC classes: within sites"      = "rates_individual_within",
                "Rate ratios, individual UPC classes: between and within" = "rates_individual",
                "Rate ratios, grouped classes: between sites"            = "rates_grouped_between",
                "Rate ratios, grouped classes: within sites"             = "rates_grouped_within",
                "Rate ratios, grouped classes: between and within"       = "rates_grouped",
                "One species, one substrate class"                       = "pair")

is_nmds       <- function(analysis) identical(analysis, "nmds_algae")
is_dbrda      <- function(analysis) identical(analysis, "dbrda_algae")
is_ordination <- function(analysis) is_nmds(analysis) || is_dbrda(analysis)
is_glmm       <- function(analysis) identical(analysis, "glmm_algae")
## the analyses drawn transect by transect, with the algae alone
is_transect_scale <- function(analysis) is_ordination(analysis) || is_glmm(analysis)

## what the third choice offers for an analysis, and what it is called
views_for <- function(analysis) {
  if (is_ordination(analysis)) ORDINATION_COLOR_BY else if (is_glmm(analysis)) GLMM_VIEWS else VIEWS
}
view_heading <- function(analysis) if (is_ordination(analysis)) "Color by" else "View"

## The folder each choice writes into under figs/
ANALYSIS_FOLDER <- c(trend = "temporal_trend", density = "density",
                     density_sd = "density_sd", nmds_algae = "nmds_algae",
                     dbrda_algae = "dbrda_algae", glmm_algae = "glmm_algae")
SCALE_FOLDER <- c(site = "site", basin = "basin", puget = "puget_sound",
                  transect = "transect")

## NULL when the combination makes a figure; otherwise why it does not
view_problem <- function(analysis, scale, view) {
  if (is_ordination(analysis)) {
    if (!identical(scale, "transect"))
      return("the ordinations place individual transects, so their scale is the transect")
    if (!view %in% ORDINATION_COLOR_BY)
      return("the ordinations color their points by basin, site, year or depth zone")
    return(NULL)
  }
  if (is_glmm(analysis)) {
    if (!identical(scale, "transect"))
      return("the GLMMs model individual transects, so their scale is the transect")
    if (!view %in% GLMM_VIEWS)
      return("the GLMMs draw rate ratios, or one species against one substrate class")
    return(NULL)
  }
  if (identical(scale, "transect"))
    return("only the ordinations and the GLMMs are drawn transect by transect")
  ## a choice left over from a transect-scale analysis, for the moment the app
  ## takes to swap its controls back, is not a view
  if (!view %in% VIEWS) return("that view belongs to another analysis")
  if (!scale %in% SCALES) return("that scale belongs to another analysis")
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
nmds_points   <- NULL
nmds_species  <- NULL
nmds_stress   <- NULL
nmds_envfit   <- NULL
dbrda_points  <- NULL
dbrda_species <- NULL
dbrda_terms   <- NULL
dbrda_summary <- NULL
dbrda_varpart <- NULL
glmm_terms    <- NULL
glmm_curves   <- NULL
glmm_sites    <- NULL
glmm_bins     <- NULL
glmm_summary  <- NULL

## Reads every table the builders use and puts them where the builders look.
## `paths` is a list with any of: transect, site, basin, puget, trend_site,
## trend_basin, trend_puget, nmds_points, nmds_species, nmds_stress,
## nmds_envfit, dbrda_points, dbrda_species, dbrda_terms, dbrda_summary,
## dbrda_varpart, glmm_terms, glmm_curves, glmm_sites, glmm_bins, glmm_summary. A missing
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
  nmds_points   <<- rd("nmds_points")
  nmds_species  <<- rd("nmds_species")
  nmds_stress   <<- rd("nmds_stress")
  nmds_envfit   <<- rd("nmds_envfit")
  dbrda_points  <<- rd("dbrda_points")
  dbrda_species <<- rd("dbrda_species")
  dbrda_terms   <<- rd("dbrda_terms")
  dbrda_summary <<- rd("dbrda_summary")
  dbrda_varpart <<- rd("dbrda_varpart")
  glmm_terms    <<- rd("glmm_terms")
  glmm_curves   <<- rd("glmm_curves")
  glmm_sites    <<- rd("glmm_sites")
  glmm_bins     <<- rd("glmm_bins")
  glmm_summary  <<- rd("glmm_summary")
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
#: For the ordinations, `view` is what colours the points (ORDINATION_COLOR_BY),
#: `where` the basin whose sites are coloured when that is "site", and
#: `category` the species whose density sizes the points (or "total_density");
#: `arrows` and `ellipses` switch the species arrows and the group ellipses, and
#: `substrate` the substrate vectors on the NMDS. (The db-RDA always draws its
#: substrate arrows: they are what it is constrained by.)
make_figure <- function(analysis, scale, view, grp, where = NULL, category = NULL,
                        y_lock = FALSE, arrows = TRUE, ellipses = TRUE, substrate = FALSE) {
  fail <- function(...) list(ok = FALSE, reason = paste0(...))

  problem <- view_problem(analysis, scale, view)
  if (!is.null(problem)) return(fail(problem))
  if (is_nmds(analysis)) {
    return(draw_nmds(view, basin = where, size_by = category, arrows = arrows,
                     ellipses = ellipses, substrate = substrate))
  }
  if (is_dbrda(analysis)) {
    return(draw_dbrda(view, basin = where, size_by = category, arrows = arrows,
                      ellipses = ellipses))
  }
  ## the GLMMs: `category` is the species and `where` the substrate class
  if (is_glmm(analysis)) {
    if (identical(view, "pair")) return(draw_glmm_pair(species = category, predictor = where))
    ## rates_<set>, or rates_<set>_<between|within> for one half on its own
    parts <- strsplit(sub("^rates_", "", view), "_", fixed = TRUE)[[1]]
    return(draw_glmm_rates(parts[1], panel = if (length(parts) > 1) parts[2] else "both"))
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
      member_lines <- member_lines %>%
        mutate(.id = member, .name = place_label(member),
               .tip = hover_tip(.name, paste(year, "\u00b7", hover_value(analysis, grp, value))))
      p <- p +
        hover_layer("geom_path", aes(x = .x, y = y, group = .grp), aes(tooltip = .name, data_id = .id),
                    data = paths_of(member_lines), color = member_color, linewidth = 0.6, alpha = 0.6) +
        hover_layer("geom_point", aes(x = .x, y = y), aes(tooltip = .tip, data_id = .id),
                    data = member_lines, color = member_color, size = 1, alpha = 0.7) +
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
      hover_layer("geom_point", aes(x = .x, y = y),
                  aes(tooltip = hover_tip(place_label(unit), paste(year, "\u00b7", hover_value(analysis, grp, value)))),
                  data = focal_lines, color = composite_color, size = 2) +
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
      member_lines <- with_runs(members_drawn, c(".facet", "member", "col"), density_drawable) %>%
        mutate(.id = member, .name = place_label(member),
               .tip = hover_tip(.name, paste(year, "\u00b7", hover_value(analysis, grp, value))))
      p <- p +
        hover_layer("geom_path", aes(x = .x, y = value, group = .grp), aes(tooltip = .name, data_id = .id),
                    data = paths_of(member_lines), color = member_color, linewidth = 0.6, alpha = 0.6) +
        hover_layer("geom_point", aes(x = .x, y = value), aes(tooltip = .tip, data_id = .id),
                    data = member_lines, color = member_color, size = 1, alpha = 0.6)
    }
    focal_lines <- with_runs(focal, c(".facet", "unit", "col"), density_drawable)
    ## every panel's axis starts at 0, including a category that was never
    ## recorded, which would otherwise get a meaningless -0.05..0.05 axis
    floor_blank <- focal %>% distinct(.facet) %>% mutate(y = 0)
    p <- p +
      geom_blank(data = floor_blank, aes(y = y)) +
      geom_path(data = paths_of(focal_lines), aes(x = .x, y = value, group = .grp),
                color = composite_color, linewidth = 1.3) +
      hover_layer("geom_point", aes(x = .x, y = value),
                  aes(tooltip = hover_tip(place_label(unit), paste(year, "\u00b7", hover_value(analysis, grp, value)),
                                          if (identical(analysis, "density_sd")) paste("SD", hover_num(sd)) else NULL)),
                  data = focal_lines, color = composite_color, size = 2) +
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
  focal <- focal %>% mutate(col = factor(col, levels = cols), .x = factor(year, levels = yrs),
                            .id = as.character(col), .name = unname(lab[as.character(col)]),
                            .tip = hover_tip(.name, paste(year, "\u00b7", hover_value(analysis, grp, value))))

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
      hover_layer("geom_path", aes(x = .x, y = y, group = .grp, color = col), aes(tooltip = .name, data_id = .id),
                  data = paths_of(lines), linewidth = 1.1) +
      hover_layer("geom_point", aes(x = .x, y = y, color = col), aes(tooltip = .tip, data_id = .id),
                  data = lines, size = 2.4) +
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
      hover_layer("geom_path", aes(x = .x, y = value, group = .grp, color = col), aes(tooltip = .name, data_id = .id),
                  data = paths_of(lines), linewidth = 1.1) +
      hover_layer("geom_point", aes(x = .x, y = value, color = col), aes(tooltip = .tip, data_id = .id),
                  data = lines, size = 2.4) +
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

## ---- ORDINATIONS -------------------------------------------------------------------
## The NMDS and the db-RDA are drawn alike, in the style of the ROV-diver
## comparison report
## (CCR_benthic_analyses/ROV_diver_comparison/code/NMDS_visualization_functions.R):
## faded points coloured by group, a 95% ellipse per group, black arrows to the
## species scores with repelled white labels, and both axes on one scale
## (coord_fixed) so distances read the same in both directions. Substrate is
## drawn in blue: the db-RDA's constraints, or the same gradients fitted onto
## the NMDS (envfit). substrate_algae.R says how both were computed and tested.

## A palette for n groups: Dark2 while it lasts, then Paired, then evenly spaced
## hues for the largest basins' site lists
qualitative <- function(n) {
  if (n <= 8) RColorBrewer::brewer.pal(max(3, n), "Dark2")[seq_len(n)]
  else if (n <= 12) RColorBrewer::brewer.pal(n, "Paired")
  else scales::hue_pal(l = 55)(n)
}

substrate_colour <- "#1F5FA8"

depth_zone_labels <- c(offshore = "Offshore (transects 1-3)",
                       inshore  = "Inshore (transects 4-6)")

## the transects an ordination placed
ordination_points <- function(analysis) {
  if (is_nmds(analysis)) nmds_points else if (is_dbrda(analysis)) dbrda_points
}

## the transect column a size choice refers to: a species ("ak_sugar_kelp",
## or the "_avg" form the other figures use) or the total of all of them
point_size_column <- function(size_by) {
  if (is.null(size_by) || !nzchar(size_by)) return(NULL)
  if (identical(size_by, "total_density")) return("total_density")
  make.names(sub("_avg$", "", size_by))
}

point_size_label <- function(size_by) {
  if (identical(size_by, "total_density")) "all understory algae" else species_label(size_by)
}

## The species an ordination's points can be sized by, label -> value, for the
## app -- the same ten understory algae in both
point_size_choices <- function() {
  spp_table <- if (!is.null(nmds_species)) nmds_species else dbrda_species
  if (is.null(spp_table)) return(character(0))
  spp <- as.character(spp_table$category)
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

## Which transects are in colour, the group each belongs to, and the colours
## and legend title for the groups. NULL when colouring by site without a
## basin to take the sites from. Colouring by basin, `basin` picks one to
## show in colour against the rest in gray; it keeps the colour it has when
## every basin is shown, so the two views read alike.
ordination_groups <- function(pts, color_by, basin = NULL) {
  focus <- rep(TRUE, nrow(pts))
  if (identical(color_by, "basin")) {
    group <- place_label(pts$basin)
    title <- "Basin"
    if (!is.null(basin) && nzchar(basin)) {
      if (!basin %in% pts$basin) return(NULL)
      focus <- as.character(pts$basin) == basin
    }
  } else if (identical(color_by, "site")) {
    if (is.null(basin) || !basin %in% pts$basin) return(NULL)
    focus <- as.character(pts$basin) == basin
    group <- ifelse(focus, place_label(pts$site), NA_character_)
    title <- paste0("Site (", place_label(basin), ")")
  } else if (identical(color_by, "year")) {
    group <- as.character(pts$year)
    title <- "Year"
  } else {
    group <- unname(depth_zone_labels[as.character(pts$depth_zone)])
    title <- "Depth zone"
  }
  groups <- sort(unique(group[focus & !is.na(group)]))
  if (identical(color_by, "depth_zone")) groups <- intersect(unname(depth_zone_labels), groups)
  ## a basin's colour comes from the palette over every basin
  palette_over <- if (identical(color_by, "basin")) sort(unique(group)) else groups
  colours <- switch(color_by,
    year = setNames(scales::viridis_pal(end = 0.88)(length(groups)), groups),
    ## blue deeper, orange shallower -- the ROV-diver report's convention
    depth_zone = setNames(c("#3182BD", "#E6550D")[seq_along(groups)], groups),
    setNames(qualitative(length(palette_over)), palette_over)[groups])
  list(focus = focus, group = unname(group), groups = groups, colours = colours,
       title = title)
}

## The subtitle's note when only some points are in colour: "sites of Hood
## Canal in color, the rest in gray" for a basin's sites, or "Hood Canal in
## color, ..." for one basin picked out; nothing when every point is.
highlight_note <- function(color_by, basin, focus) {
  if (all(focus)) return(NULL)
  paste0(" \u00b7 ", if (identical(color_by, "site")) "sites of " else "",
         place_label(basin), " in color, the rest in gray")
}

## Which substrate vectors an NMDS draws: none, the groups (hard, mixed,
## soft, relief) or the individual classes. TRUE, from before there was a
## choice, means the groups.
substrate_mode <- function(substrate) {
  if (isTRUE(substrate)) return("grouped")
  if (is.character(substrate) && length(substrate) == 1 &&
      substrate %in% c("grouped", "individual")) return(substrate)
  "none"
}

## Everything an ordination figure has before its arrows: the transects as
## points (those not in colour in gray, behind), sized by a species' density
## if asked, and the group ellipses. `x` and `y` name the score columns; the
## points are drawn at .x and .y, which the arrow layers share.
ordination_base <- function(pts, x, y, color_by, basin, size_by, ellipses) {
  fail <- function(...) list(ok = FALSE, reason = paste0(...))
  g <- ordination_groups(pts, color_by, basin)
  if (is.null(g)) return(fail("choose the basin whose sites to color"))
  size_col <- point_size_column(size_by)
  if (!is.null(size_col) && !size_col %in% names(pts))
    return(fail("no density column for ", point_size_label(size_by)))

  pts$.x <- pts[[x]]
  pts$.y <- pts[[y]]
  ## hovering a transect names it, with its depth from the transect table, and
  ## picks out every transect of its site
  depth_ft <- if (is.null(df_transect)) rep(NA_real_, nrow(pts)) else
    df_transect$depth_ft[match(pts$key, df_transect$key)]
  pts$.id <- as.character(pts$site)
  pts$.tip <- hover_tip(place_label(pts$site),
                        paste(place_label(pts$basin), "\u00b7", pts$year),
                        ifelse(is.na(depth_ft), paste("transect", pts$transect),
                               paste0("transect ", pts$transect, ", ", hover_num(depth_ft), " ft")),
                        if (!is.null(size_col) && size_col %in% names(pts))
                          paste(hover_num(pts[[size_col]]), "per m\u00b2"))
  fg <- pts[g$focus, , drop = FALSE]
  fg$.group <- factor(g$group[g$focus], levels = g$groups)
  tip <- aes(tooltip = .tip, data_id = .id)

  p <- ggplot(mapping = aes(x = .x, y = .y))

  ## hovering a legend entry picks out that group: an unseen copy of the
  ## coloured points, under the visible ones (so it never takes the mouse from
  ## them), carries the group's id, which the hover makes visible
  if (hover_on()) {
    p <- p + if (is.null(size_col)) {
      ggiraph::geom_point_interactive(data = fg, aes(color = .group, data_id = paste0("group:", .group)),
                                      alpha = 0, shape = 16, size = 1.6)
    } else {
      ggiraph::geom_point_interactive(data = fg, aes(color = .group, size = .data[[size_col]],
                                                     data_id = paste0("group:", .group)),
                                      alpha = 0, shape = 16)
    }
  }

  ## the other basins' transects stay in view, in gray, so a basin's sites are
  ## seen against the whole ordination
  if (!all(g$focus)) {
    p <- p + hover_layer("geom_point", aes(), tip, data = pts[!g$focus, , drop = FALSE], color = "gray78",
                         alpha = 0.45, size = 1.1, shape = 16)
  }

  if (is.null(size_col)) {
    p <- p + hover_layer("geom_point", aes(color = .group), tip, data = fg, size = 1.6, alpha = 0.4, shape = 16)
  } else {
    ## largest on top would hide the rest; smallest on top keeps every point visible
    fg <- fg[order(-fg[[size_col]]), , drop = FALSE]
    p <- p +
      hover_layer("geom_point", aes(color = .group, size = .data[[size_col]]), tip, data = fg,
                  alpha = 0.45, shape = 16) +
      size_scale(paste0(point_size_label(size_by), "\n(per m\u00b2)"))
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

  p <- p +
    (if (hover_on()) ggiraph::scale_color_manual_interactive(
       values = g$colours, name = g$title, drop = FALSE,
       data_id = function(breaks) paste0("group:", breaks), tooltip = function(breaks) breaks,
       labels = function(breaks) lapply(breaks, function(b)
         ggiraph::label_interactive(b, data_id = paste0("group:", b), tooltip = b)))
     else scale_color_manual(values = g$colours, name = g$title, drop = FALSE)) +
    guides(color = (if (hover_on()) ggiraph::guide_legend_interactive else guide_legend)(
                     override.aes = list(alpha = 1, size = 3, linewidth = 0),
                                order = 1))
  list(ok = TRUE, plot = p, pts = pts, focus = g$focus, size_col = size_col)
}

## How far to stretch a set of arrows so the longest reaches `fill` of the way
## to the edge of the points in its direction: vegan's ordiArrowMul(), with
## the points' extent in place of the plot's
arrow_mul <- function(ax, ay, px, py, fill = 0.75) {
  r <- c(min(px) / min(ax), max(px) / max(ax), min(py) / min(ay), max(py) / max(ay))
  r <- r[is.finite(r) & r > 0]
  if (length(r)) fill * min(r) else 1
}

## One set of arrows from the origin, and its labels' rows: `d` has the arrow
## tips in `x` and `y` (already stretched) and the labels in `labels`
arrow_set <- function(x, y, labels, colour) {
  data.frame(.x = x, .y = y, .label = labels, .colour = colour)
}

arrow_layer <- function(d, linewidth = 0.8) {
  geom_segment(data = d, aes(x = 0, y = 0, xend = .x, yend = .y),
               linewidth = linewidth, color = d$.colour,
               arrow = arrow(length = unit(0.2, "cm")))
}

## Every arrow's label in one repelled layer, so the species and substrate
## labels keep clear of each other as well as of themselves
arrow_labels <- function(d) {
  ggrepel::geom_label_repel(data = d, aes(x = .x, y = .y, label = .label),
                            colour = d$.colour, size = 3.2, fontface = "bold",
                            fill = "white", max.overlaps = Inf, min.segment.length = 0,
                            segment.color = "gray40", seed = 42)
}

species_names <- function(categories) vapply(as.character(categories), species_label, character(1))

## "p = 0.004"; 999 permutations cannot give less than 0.001
p_text <- function(p) sprintf("p = %.3f", p)

## what the substrate gradients are, for the captions
substrate_definitions <- paste(
  "Hard = rock, boulder and reef; soft = sand, clay and shell hash; mixed = pebble and cobble --",
  "each a share of the transect's UPC points; mean relief of those points in m.")
class_definitions <- paste(
  "Each class is a share of the transect's UPC points: reef (bedrock), large boulder (50 cm\u20131 m),",
  "small boulder (25\u201350 cm), rock (15\u201325 cm), cobble (5\u201315 cm), pebble (0.5\u20135 cm), sand and",
  "shell hash; mean relief of those points in m.")

## "hard 0.37 (0.001 / 0.059); ..." -- each vector's r^2 across transects,
## then its permutation p between sites / within sites
vector_stats <- function(ef) {
  paste(sprintf("%s %.2f (%.3f / %.3f)", sub(" substrate$", "", ef$label), ef$r2,
                ef$p_between_sites, ef$p_within_sites), collapse = "; ")
}

## "hard p = 0.001, soft p = 0.265, ..." for a column of p-values
term_p_list <- function(labels, p) {
  paste(sprintf("%s %s", sub(" substrate$", "", labels), p_text(p)), collapse = ", ")
}

## The title and subtitle run from the figure's left edge rather than the
## panel's: coord_fixed() can leave the panel much narrower than the figure,
## and a subtitle aligned to it then runs off the right-hand side.
ordination_theme <- function() {
  my.theme +
    theme(axis.title = element_text(size = 14),
          axis.text = element_text(size = 12),
          plot.title = element_text(size = 15),
          plot.title.position = "plot",
          plot.subtitle = element_text(size = 10.5, colour = "gray30"),
          plot.caption = element_text(hjust = 0, size = 8.5, colour = "gray35",
                                      lineheight = 1.05),
          plot.caption.position = "plot",
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 11),
          legend.key = element_blank())
}

## the caption's sentences, wrapped. c() then collapse, not paste(): paste()
## turns a skipped (NULL) sentence into "" and leaves a double space where it
## would have been
caption_text <- function(...) {
  paste(strwrap(paste(c(...), collapse = " "), width = 175), collapse = "\n")
}

## ---- the NMDS ----

draw_nmds <- function(color_by, basin = NULL, size_by = NULL, arrows = TRUE,
                      ellipses = TRUE, substrate = FALSE) {
  fail <- function(...) list(ok = FALSE, reason = paste0(...))
  if (is.null(nmds_points) || is.null(nmds_species) || is.null(nmds_stress))
    return(fail("the NMDS tables are missing -- run step 2"))
  vectors <- substrate_mode(substrate)
  if (vectors != "none" && is.null(nmds_envfit))
    return(fail("the substrate vectors are missing -- run step 2"))

  info <- nmds_stress[nmds_stress$k == 2, , drop = FALSE]
  base <- ordination_base(nmds_points, "NMDS1", "NMDS2", color_by, basin, size_by, ellipses)
  if (!isTRUE(base$ok)) return(base)
  p <- base$plot

  ## species at their scores, the weighted averages of the transects they are
  ## on; substrate as fitted, stretched together to fit the points
  sets <- list()
  if (isTRUE(arrows)) {
    sets$species <- arrow_set(nmds_species$NMDS1, nmds_species$NMDS2,
                              species_names(nmds_species$category), "black")
  }
  ef <- NULL
  if (vectors != "none") {
    ## a table from before the individual classes holds the groups alone
    ef <- if ("set" %in% names(nmds_envfit)) nmds_envfit[nmds_envfit$set == vectors, , drop = FALSE]
          else if (vectors == "grouped") nmds_envfit else nmds_envfit[0, ]
    if (!nrow(ef)) return(fail("no vectors for the individual classes yet -- run step 2"))
    m <- arrow_mul(ef$NMDS1, ef$NMDS2, base$pts$.x, base$pts$.y)
    sets$substrate <- arrow_set(ef$NMDS1 * m, ef$NMDS2 * m, as.character(ef$label),
                                substrate_colour)
  }
  for (d in sets) p <- p + arrow_layer(d, linewidth = if (identical(d$.colour[1], "black")) 0.8 else 1)
  if (length(sets)) p <- p + arrow_labels(do.call(rbind, sets))

  n_out <- info$n_no_understory_algae + info$n_no_algae_survey
  subtitle <- paste0("Bray-Curtis on Wisconsin-standardized square-root densities \u00b7 ",
                     sprintf("2-D stress %.3f", info$stress), " \u00b7 ",
                     format(info$n_transects, big.mark = ","), " transects",
                     highlight_note(color_by, basin, base$focus))
  caption <- caption_text(
    sprintf("Each point is one 30 x 2 m transect; %d with no understory algae and %d without an algae survey cannot be placed and are left out.",
            info$n_no_understory_algae, info$n_no_algae_survey),
    if (isTRUE(arrows)) "Black arrows point to each species' score: the weighted average of the transects it occurs on.",
    if (vectors != "none") c(
      sprintf("Blue arrows: %s fitted onto the ordination (envfit), each pointing the way it increases, its length in proportion to its correlation with the ordination.",
              if (vectors == "grouped") "the substrate groups and mean relief"
              else "each UPC substrate class, one at a time, and mean relief"),
      if (vectors == "grouped") substrate_definitions else class_definitions,
      sprintf("r\u00b2 across transects, then permutation p (%d) between sites (site means against site centroids) / within sites (transects shuffled only among their own site's): %s.",
              ef$permutations[1], vector_stats(ef))),
    if (isTRUE(ellipses)) "Ellipses: 95% normal-theory ellipses for each group of 5 or more transects.",
    if (!is.null(base$size_col)) sprintf("Point size: %s density on the transect.", point_size_label(size_by)),
    "Stress under 0.2 gives a usable two-dimensional picture (Clarke 1993); fine detail should not be over-read.")

  p <- p +
    coord_fixed() +
    labs(x = "NMDS1", y = "NMDS2", title = "Transect-level NMDS: understory algae",
         subtitle = subtitle, caption = caption) +
    ordination_theme()
  list(ok = TRUE, plot = p)
}

## ---- the db-RDA ----

draw_dbrda <- function(color_by, basin = NULL, size_by = NULL, arrows = TRUE,
                       ellipses = TRUE) {
  fail <- function(...) list(ok = FALSE, reason = paste0(...))
  if (is.null(dbrda_points) || is.null(dbrda_species) || is.null(dbrda_terms) ||
      is.null(dbrda_summary))
    return(fail("the db-RDA tables are missing -- run step 2"))

  s <- dbrda_summary[1, ]
  tm <- dbrda_terms
  base <- ordination_base(dbrda_points, "dbRDA1", "dbRDA2", color_by, basin, size_by, ellipses)
  if (!isTRUE(base$ok)) return(base)
  p <- base$plot
  px <- base$pts$.x
  py <- base$pts$.y

  ## species and substrate scores are on scales of their own, so each set is
  ## stretched separately to fit the points
  sets <- list()
  if (isTRUE(arrows)) {
    spp <- dbrda_species
    m <- arrow_mul(spp$dbRDA1, spp$dbRDA2, px, py, fill = 0.9)
    sets$species <- arrow_set(spp$dbRDA1 * m, spp$dbRDA2 * m, species_names(spp$category),
                              "black")
  }
  m <- arrow_mul(tm$dbRDA1, tm$dbRDA2, px, py)
  sets$substrate <- arrow_set(tm$dbRDA1 * m, tm$dbRDA2 * m, as.character(tm$label),
                              substrate_colour)
  for (d in sets) p <- p + arrow_layer(d, linewidth = if (identical(d$.colour[1], "black")) 0.8 else 1)
  p <- p + arrow_labels(do.call(rbind, sets))

  vp <- dbrda_varpart
  frac <- function(name) if (is.null(vp)) NA_real_ else vp$adj_r2[vp$fraction == name]

  ## two lines: the result, then what it was computed on
  subtitle <- paste0(
    sprintf("Substrate explains %.1f%% of the variation in the algae community (adjusted %.1f%%)",
            100 * s$r2, 100 * s$adj_r2), "\n",
    "Bray-Curtis on Wisconsin-standardized square-root densities \u00b7 year partialled out \u00b7 ",
    format(s$n_transects, big.mark = ","), " transects",
    highlight_note(color_by, basin, base$focus))
  caption <- caption_text(
    sprintf("Each point is one 30 x 2 m transect, placed by its algae (weighted-average scores, scaling 1: distances between transects are kept). The second axis holds %.1f%% of the variation to the first's %.1f%%: read the figure left to right.",
            100 * s$axis2_share, 100 * s$axis1_share),
    "Blue arrows: the substrate the axes are constrained by (mixed is left out as the reference, since the three shares sum to one).",
    substrate_definitions,
    if (isTRUE(arrows)) "Black arrows: species scores.",
    "Each set of arrows is stretched to fit the points.",
    sprintf("Permutation tests (%d) between sites: site-mean substrate explains %.1f%% of the differences between site means (adjusted R\u00b2, %s); given the other two terms, %s.",
            s$permutations, 100 * s$between_adj_r2, p_text(s$between_p),
            term_p_list(tm$label, tm$p_between_sites)),
    sprintf("Within sites: %.1f%% of the variation left among transects once site and year are accounted for (%s); %s.",
            100 * s$within_share_of_within_site, p_text(s$within_p),
            term_p_list(tm$label, tm$p_within_sites)),
    if (!is.null(vp)) sprintf("Variation partitioning (adjusted R\u00b2): substrate alone %.1f%%, shared with basin %.1f%%, basin alone %.1f%%, unexplained %.1f%%.",
                              100 * frac("substrate alone"), 100 * frac("substrate and basin together"),
                              100 * frac("basin alone"), 100 * frac("unexplained")),
    if (isTRUE(ellipses)) "Ellipses: 95% normal-theory ellipses for each group of 5 or more transects.",
    if (!is.null(base$size_col)) sprintf("Point size: %s density on the transect.", point_size_label(size_by)))

  p <- p +
    coord_fixed() +
    labs(x = sprintf("dbRDA1 (%.1f%%)", 100 * s$axis1_share),
         y = sprintf("dbRDA2 (%.1f%%)", 100 * s$axis2_share),
         title = "Transect-level db-RDA: understory algae constrained by substrate",
         subtitle = subtitle, caption = caption) +
    ordination_theme()
  list(ok = TRUE, plot = p)
}

## The figures Run writes for an ordination: every colouring, plain, and each
## basin's sites. The NMDS coloured by basin is also drawn with each set of
## substrate vectors, and sized by each species and by the total. (One basin
## picked out against the rest is for Explore data, not written by Run: each
## basin's sites already show it.)
ordination_targets <- function(analysis, view) {
  pts <- ordination_points(analysis)
  if (is.null(pts)) return(list())
  plain <- list(where = NULL, category = NULL)
  if (identical(view, "site")) {
    return(lapply(sort(unique(as.character(pts$basin))),
                  function(b) list(where = b, category = NULL)))
  }
  if (identical(view, "basin") && is_nmds(analysis)) {
    has_classes <- !is.null(nmds_envfit) && "set" %in% names(nmds_envfit) &&
      any(nmds_envfit$set == "individual")
    return(c(list(plain),
             if (!is.null(nmds_envfit)) list(list(where = NULL, category = NULL, substrate = "grouped")),
             if (has_classes) list(list(where = NULL, category = NULL, substrate = "individual")),
             lapply(unname(point_size_choices()), function(s) list(where = NULL, category = s))))
  }
  list(plain)
}

## ---- GLMMs ---------------------------------------------------------------------------
## Each understory alga's count against each substrate class, one class at a
## time, split into a between-site and a within-site rate ratio -- see the
## header of glmm_substrate.R for the model and why it is split.

## how a class reads on a figure: the Washington UPC size ranges, so the
## individual classes read as the size gradient they are
glmm_class_label <- c(
  "hard"          = "hard substrate",
  "mixed"         = "mixed substrate",
  "soft"          = "soft substrate",
  "relief"        = "mean relief",
  "reef"          = "reef",
  "large boulder" = "large boulder (50 cm\u20131 m)",
  "small boulder" = "small boulder (25\u201350 cm)",
  "cobble"        = "cobble (5\u201315 cm)",
  "rock"          = "rock (15\u201325 cm)",
  "pebble"        = "pebble (0.5\u20135 cm)",
  "sand"          = "sand",
  "shell hash"    = "shell hash")
glmm_class_members <- c(
  "hard"  = "reef, boulders and rock",
  "mixed" = "cobble and pebble",
  "soft"  = "sand, clay and shell hash")

glmm_label <- function(p) {
  l <- unname(glmm_class_label[p]); ifelse(is.na(l), p, l)
}

## the classes in the order glmm_substrate.R wrote them (substrate_classes.R's
## order), as label -> value, grouped for the app's menu
glmm_predictor_choices <- function() {
  if (is.null(glmm_terms)) return(list())
  sets <- split(as.character(glmm_terms$predictor), glmm_terms$set)
  out <- lapply(sets, function(p) { p <- unique(p); setNames(p, glmm_label(p)) })
  setNames(out[c("grouped", "individual")], c("Grouped", "Individual UPC classes"))
}

glmm_species_choices <- function() {
  if (is.null(glmm_summary)) return(character(0))
  s <- as.character(glmm_summary$species)
  setNames(s, vapply(s, species_label, character(1)))
}

## the species from hard-substrate (top) to soft-bottom (bottom) along the
## db-RDA's substrate axis when that table is there; else by how common they are
glmm_species_order <- function() {
  s <- as.character(glmm_summary$species)
  if (!is.null(dbrda_species) && all(s %in% dbrda_species$category)) {
    return(as.character(dbrda_species$category[order(dbrda_species$dbRDA1)]))
  }
  s[order(glmm_summary$prevalence)]
}

glmm_stars <- function(q) {
  ifelse(is.na(q), "", ifelse(q < 0.001, "***", ifelse(q < 0.01, "**", ifelse(q < 0.05, "*", ""))))
}

## Every species against every class of one set, between sites (top) and
## within sites (bottom), or either half on its own: the rate ratio in each
## cell, coloured on a doubling scale. Both halves share that scale, so they
## can be set side by side.
draw_glmm_rates <- function(set = c("individual", "grouped"), panel = c("both", "between", "within")) {
  set <- match.arg(set)
  panel <- match.arg(panel)
  one <- panel != "both"
  fail <- function(...) list(ok = FALSE, reason = paste0(...))
  if (is.null(glmm_terms) || is.null(glmm_summary))
    return(fail("the GLMM tables are missing -- run step 2"))
  tm <- glmm_terms[glmm_terms$form == "split" & glmm_terms$set == set & !is.na(glmm_terms$term), ]
  if (one) tm <- tm[tm$term == panel, ]
  if (!nrow(tm)) return(fail("no GLMM results for the ", set, " classes"))

  preds <- unique(as.character(tm$predictor))
  prev <- setNames(glmm_summary$prevalence, glmm_summary$species)
  row_label <- function(s) sprintf("%s  (%s%%)", vapply(s, species_label, character(1)),
                                   formatC(100 * prev[s], format = "f", digits = 0))
  col_label <- function(p) sub(" \\(", "\n(", glmm_label(p))
  order_sp <- glmm_species_order()
  tm$row <- factor(row_label(as.character(tm$species)), levels = row_label(order_sp))
  tm$col <- factor(col_label(as.character(tm$predictor)), levels = col_label(preds))
  ## two lines each: on one, the within-site label ran past the panel's end
  panels <- c(between = "Between sites\n(site means)",
              within  = "Within sites\n(transect vs its site's mean)")
  tm$panel <- factor(unname(panels[tm$term]), levels = unname(panels))
  tm$l2 <- pmax(-2, pmin(2, log2(tm$rate_ratio)))
  tm$lab <- paste0(ifelse(tm$rate_ratio >= 9.95, sprintf("%.0f", tm$rate_ratio), sprintf("%.2f", tm$rate_ratio)),
                   glmm_stars(tm$q), ifelse(tm$ok, "", "\u2020"))

  n_tr <- max(glmm_summary$n_transects); n_si <- glmm_summary$n_sites[1]
  caption <- caption_text(
    sprintf("Negative binomial GLMMs of each species' count per 30 x 2 m transect, one class at a time: count ~ between + within + depth (ft) + year + (1 | site); %s transects at %d sites.",
            format(n_tr, big.mark = ","), n_si),
    switch(panel,
      both    = "Between: the site's mean share over all its transects and years. Within: the transect's share minus its site's mean.",
      between = "Between sites: each site's mean share over all its transects and years, so each cell compares sites.",
      within  = "Within sites: each transect's share minus its site's mean, so each cell compares transects at the same site."),
    "Species run from hard-substrate (top) to soft-bottom (bottom) along the db-RDA's substrate axis; in brackets, the share of transects with any.",
    "* q < 0.05, ** q < 0.01, *** q < 0.001 (Benjamini-Hochberg across all between and within terms); \u2020 the fit warned, too few transects with the species to trust.",
    if (set == "individual") c(
      if (panel != "within") "Between-site ratios for the boulder classes are per +10 points of a site's mean share, which few sites reach (most site means are under 10%): read those large values as direction, not size.",
      "Clay (first recorded in 2023, on 2% of transects since) and 'other' are too scarce to model."),
    if (set == "grouped") sprintf("Hard = %s; mixed = %s; soft = %s.", glmm_class_members[["hard"]],
                                  glmm_class_members[["mixed"]], glmm_class_members[["soft"]]))

  what <- if (set == "individual") "UPC substrate class" else "substrate group"
  relief_note <- if ("relief" %in% preds) "(relief: per +0.25 m of mean relief)"
  subtitle <- switch(panel,
    both    = paste("Rate ratio: how many times as many individuals per +10 percentage points of the class",
                    if (is.null(relief_note)) "on the transect's UPC points" else relief_note),
    between = paste("Rate ratio: how many times as many individuals per +10 percentage points of the class",
                    "in the site's mean", relief_note),
    within  = paste("Rate ratio: how many times as many individuals per +10 percentage points of the class",
                    "above the site's mean", relief_note))

  ## one half on its own has room for larger type
  p <- ggplot(tm, aes(col, row, fill = l2)) +
    geom_tile(color = "white", linewidth = 0.6) +
    geom_text(aes(label = lab, color = abs(l2) > 1.25), size = if (one) 3.6 else 3) +
    scale_color_manual(values = c("black", "white"), guide = "none") +
    scale_fill_gradient2(low = "#B2182B", mid = "white", high = "#2166AC", midpoint = 0,
                         limits = c(-2, 2), breaks = -2:2,
                         labels = c("\u00bc\u00d7", "\u00bd\u00d7", "1\u00d7", "2\u00d7", "4\u00d7"),
                         name = "Rate ratio") +
    (if (!one) facet_grid(panel ~ .)) +
    labs(x = NULL, y = NULL,
         title = if (one) sprintf("%s: each understory alga against each %s",
                                  if (panel == "between") "Between sites" else "Within sites", what)
                 else sprintf("Each understory alga against each %s", what),
         subtitle = subtitle, caption = caption) +
    theme_minimal(base_size = 11) +
    theme(panel.grid = element_blank(),
          strip.text.y = element_text(face = "bold", size = 10),
          axis.text.x = element_text(size = if (one) 10.5 else 9.5, lineheight = 0.95),
          axis.text.y = element_text(size = if (one) 11 else 10),
          plot.title = element_text(size = 15),
          plot.subtitle = element_text(size = 10.5, colour = "gray30"),
          plot.title.position = "plot",
          plot.caption = element_text(hjust = 0, size = 8.5, colour = "gray35", lineheight = 1.05),
          plot.caption.position = "plot")
  list(ok = TRUE, plot = p)
}

## One species against one class: between sites (the site means, and the
## expected density at a typical site) beside within sites (how a transect's
## count changes with more of the class than its site's average, with the
## transects binned behind the line)
draw_glmm_pair <- function(species, predictor) {
  fail <- function(...) list(ok = FALSE, reason = paste0(...))
  if (is.null(glmm_terms) || is.null(glmm_curves) || is.null(glmm_sites))
    return(fail("the GLMM tables are missing -- run step 2"))
  if (is.null(species) || is.null(predictor)) return(fail("choose a species and a substrate class"))
  tm <- glmm_terms[glmm_terms$species == species & glmm_terms$predictor == predictor &
                     glmm_terms$form == "split", ]
  if (!nrow(tm)) return(fail("no GLMM for ", species_label(species), " and ", predictor))
  cv <- glmm_curves[glmm_curves$species == species & glmm_curves$predictor == predictor, ]
  st <- glmm_sites[glmm_sites$species == species & glmm_sites$predictor == predictor, ]

  relief <- identical(predictor, "relief")
  sp <- species_label(species)
  cls <- glmm_label(predictor)
  col <- "#1F5FA8"
  est <- function(term) {
    r <- tm[tm$term == term, ]
    if (!nrow(r) || is.na(r$estimate)) return("not estimated")
    p <- if (!is.na(r$p) && r$p < 1e-4) "p < 0.0001" else paste("p =", format.pval(r$p, digits = 2))
    sprintf("%.2f\u00d7 (95%% CI %.2f\u2013%.2f), %s%s", r$rate_ratio, r$lo, r$hi, p,
            if (!is.na(r$q) && r$q < 0.05) paste0(", q = ", signif(r$q, 2)) else "")
  }
  per <- if (relief) "per +0.25 m" else "per +10 points"
  flagged <- !all(tm$ok)

  cb <- cv[cv$panel == "between", ]
  ## hovering a site names it (preview only; see HOVER)
  st$.tip <- hover_tip(place_label(st$site), place_label(st$basin),
                       if (relief) paste(hover_num(st$x), "m mean relief") else paste0(hover_num(st$x), "% ", cls),
                       paste(hover_num(st$density), "per m\u00b2"),
                       paste(st$n, "transects"))
  pb <- ggplot() +
    { if (nrow(cb)) geom_ribbon(data = cb, aes(x, ymin = lo, ymax = hi), fill = col, alpha = 0.18) } +
    { if (nrow(cb)) geom_line(data = cb, aes(x, fit), color = col, linewidth = 1) } +
    hover_layer("geom_point", aes(x, density, size = n), aes(tooltip = .tip, data_id = site),
                data = st, shape = 21, fill = col, color = "white", alpha = 0.85) +
    scale_y_continuous(trans = scales::pseudo_log_trans(sigma = 0.02, base = 10),
                       breaks = c(0, 0.03, 0.1, 0.3, 1, 3, 10, 30)) +
    scale_size_area(max_size = 5, name = "transects behind\nthe site's mean") +
    labs(title = "Between sites", subtitle = paste0(per, " of the site's mean:\n", est("between")),
         x = if (relief) "site's mean relief (m)" else paste0("site's mean share of ", cls, " (%)"),
         y = paste(sp, "per m\u00b2 at the site")) +
    theme_bw(base_size = 11) + theme(panel.grid.minor = element_blank())

  ## the transects behind the within line, binned (glmm_substrate.R): the
  ## axis stretches past 1/4x-4x to hold every bin, up to 1/16x-16x; a bin
  ## beyond that -- or with none of the species, which a log axis cannot
  ## place -- is an open triangle on the edge it passed, as in the trend figures
  cw <- cv[cv$panel == "within", ]
  bn <- if (is.null(glmm_bins)) NULL else
    glmm_bins[glmm_bins$species == species & glmm_bins$predictor == predictor, ]
  ## a species on under 5% of transects is too patchy to bin: a handful of
  ## transects where the model expects large counts would decide every bin
  rare <- tm$n_present[1] / tm$n[1] < 0.05
  has_bins <- !is.null(bn) && nrow(bn) > 0 && !rare
  cap <- c(1 / 16, 16)
  pos   <- if (has_bins) bn[bn$ratio >= cap[1] & bn$ratio <= cap[2], ] else NULL
  below <- if (has_bins) bn[bn$ratio < cap[1], ] else NULL
  above <- if (has_bins) bn[bn$ratio > cap[2], ] else NULL
  lim <- c(0.25, 4)
  if (!is.null(pos) && nrow(pos)) {
    lim <- c(max(cap[1], min(lim[1], min(pos$ratio) / 1.25)), min(cap[2], max(lim[2], max(pos$ratio) * 1.25)))
  }
  if (!is.null(below) && nrow(below)) lim[1] <- cap[1]
  if (!is.null(pos) && nrow(pos)) {
    pos$.tip <- hover_tip(paste0(hover_num(pos$ratio), "\u00d7 (95% interval ", hover_num(pos$lo), "\u2013", hover_num(pos$hi), ")"),
                          paste0(pos$n, " transects at ", pos$n_sites, " sites"),
                          paste0("mean departure ", hover_num(pos$x), if (relief) " m" else " points"))
  }
  if (!is.null(above) && nrow(above)) lim[2] <- cap[2]
  breaks <- 2^(ceiling(log2(lim[1])):floor(log2(lim[2])))
  pw <- ggplot() +
    geom_hline(yintercept = 1, color = "gray50", linetype = 2) +
    { if (nrow(cw)) geom_ribbon(data = cw, aes(x, ymin = pmin(lo, hi), ymax = pmax(lo, hi)), fill = col, alpha = 0.18) } +
    { if (nrow(cw)) geom_line(data = cw, aes(x, fit), color = col, linewidth = 1) } +
    { if (!is.null(pos) && nrow(pos))
        geom_linerange(data = pos, aes(x, ymin = pmax(lo, lim[1]), ymax = pmin(hi, lim[2])),
                       color = "gray35", linewidth = 0.5) } +
    { if (!is.null(pos) && nrow(pos))
        hover_layer("geom_point", aes(x, ratio, size = n), aes(tooltip = .tip),
                    data = pos, shape = 21, fill = col, color = "white", alpha = 0.9) } +
    { if (!is.null(below) && nrow(below))
        geom_point(data = below, aes(x, lim[1] * 1.08), shape = 6, size = 2.5, color = col) } +
    { if (!is.null(above) && nrow(above))
        geom_point(data = above, aes(x, lim[2] / 1.08), shape = 2, size = 2.5, color = col) } +
    scale_y_continuous(trans = "log2", breaks = breaks,
                       labels = format(breaks, drop0trailing = TRUE, trim = TRUE)) +
    scale_size_area(max_size = 5, name = "transects\nin the bin") +
    coord_cartesian(ylim = lim) +
    labs(title = "Within sites", subtitle = paste0(per, " above the site's own mean:\n", est("within")),
         x = if (relief) "transect's mean relief minus its site's mean (m)"
             else "transect's share minus its site's mean (percentage points)",
         y = "count relative to the site's typical transect") +
    theme_bw(base_size = 11) + theme(panel.grid.minor = element_blank())

  n_tr <- tm$n[1]; n_si <- tm$n_sites[1]
  caption <- caption_text(
    sprintf("Negative binomial GLMM: count ~ between + within + depth (ft) + year + (1 | site), %s transects at %d sites (%d with %s).",
            format(n_tr, big.mark = ","), n_si, tm$n_present[1], sp),
    "Between sites: each point is one site, at its mean over all its transects and years, sized by how many transects that mean averages; the line is the expected density at a typical site, over the range of site means, averaged over the depths and years of the transects in the years the species was recorded.",
    "Within sites: the line is the change in a transect's count with more of the class than its site's average, over the middle 95% of transects.",
    if (has_bins) "Each point pools the transects in one of ten equal steps of that departure: their total count over the total the model expects for their sites' typical transects, sized by how many transects, with a 95% interval from resampling sites.",
    if (has_bins && (nrow(below) || nrow(above)))
      paste0("Open triangles mark steps off the axis: ",
             paste(c(if (nrow(below)) sprintf("on the bottom edge, under 1/16\u00d7 or no %s at all", sp),
                     if (nrow(above)) "on the top edge, over 16\u00d7"), collapse = "; "), "."),
    if (!is.na(tm$q[1])) "q: Benjamini-Hochberg across all between and within terms.",
    if (flagged) paste0("This fit warned (", tm$note[!is.na(tm$note)][1], "), so no curve is drawn: too few transects with the species to trust it.")
    else if (rare)
      sprintf("With %s on only %d transects, the curves rest on a handful of sites: read them as direction at most. Its transects are too few to bin.",
              sp, tm$n_present[1]))

  p <- patchwork::wrap_plots(pb, pw) + patchwork::plot_annotation(
    title = sprintf("%s and %s", paste0(toupper(substr(sp, 1, 1)), substring(sp, 2)), cls),
    caption = caption,
    theme = theme(plot.title = element_text(size = 15),
                  plot.caption = element_text(hjust = 0, size = 8.5, colour = "gray35", lineheight = 1.05)))
  list(ok = TRUE, plot = p)
}

## The GLMM figures Run writes: every rate-ratio grid (each set between sites,
## within sites, and the two stacked), and every species against every class
glmm_targets <- function(view) {
  if (is.null(glmm_terms)) return(list())
  if (!identical(view, "pair")) return(list(list(where = NULL, category = NULL)))
  out <- list()
  for (s in unname(glmm_species_choices())) for (p in unname(unlist(glmm_predictor_choices()))) {
    out[[length(out) + 1]] <- list(where = p, category = s)
  }
  out
}

## ---- WHERE A FIGURE BELONGS ------------------------------------------------------
## figs/<analysis>/<scale>/<view>/<group>/<name>.png, e.g.
##   figs/density/basin/within/algae/Hood_Canal.png
##   figs/temporal_trend/puget_sound/composite/algae/Puget_Sound.png
##   figs/density_sd/basin/across/algae/sugar_kelp.png
##   figs/density/site/across/algae/Hood_Canal_sugar_kelp.png
##   figs/nmds_algae/transect/basin/algae/all_sized_by_sugar_kelp.png
##   figs/nmds_algae/transect/basin/algae/all_with_substrate.png
##   figs/nmds_algae/transect/basin/algae/all_with_substrate_classes.png
##   figs/nmds_algae/transect/basin/algae/Hood_Canal.png   (one basin picked out)
##   figs/dbrda_algae/transect/site/algae/Hood_Canal.png
##   figs/glmm_algae/transect/rates_individual/algae/all.png
##   figs/glmm_algae/transect/rates_individual_between/algae/all.png
##   figs/glmm_algae/transect/pair/algae/sieve_kelp_vs_hard.png
figure_file <- function(analysis, scale, view, grp, where = NULL, category = NULL,
                        substrate = FALSE) {
  if (is_glmm(analysis)) {
    stem <- if (identical(view, "pair")) {
      paste0(safe_file_name(species_label(category)), "_vs_", safe_file_name(where))
    } else "all"
    return(file.path(ANALYSIS_FOLDER[[analysis]], SCALE_FOLDER[[scale]], view,
                     grp$folder, paste0(stem, ".png")))
  }
  if (is_ordination(analysis)) {
    ## a basin: whose sites are coloured, or the one basin picked out
    stem <- if (!is.null(where) && nzchar(where)) safe_file_name(where) else "all"
    if (is_nmds(analysis)) {
      stem <- paste0(stem, switch(substrate_mode(substrate), grouped = "_with_substrate",
                                  individual = "_with_substrate_classes", ""))
    }
    if (!is.null(category) && nzchar(category)) {
      stem <- paste0(stem, "_sized_by_", safe_file_name(point_size_label(category)))
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
## list(where =, category =) -- plus `substrate = TRUE` for the NMDS drawn with
## its substrate vectors -- what visualizations.R loops over and what the app
## counts to say how many files a selection will write.
figure_targets <- function(analysis, scale, view, grp) {
  if (!is.null(view_problem(analysis, scale, view))) return(list())
  if (is_ordination(analysis)) {
    return(if (identical(grp$folder, "algae")) ordination_targets(analysis, view) else list())
  }
  if (is_glmm(analysis)) {
    return(if (identical(grp$folder, "algae")) glmm_targets(view) else list())
  }
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
## boxes ticked. The ordinations and the GLMMs have fixed sets of their own --
## the transect scale and every colouring or figure -- whatever those boxes say,
## since neither applies to them.
figure_sets <- function(analysis, scales, views) {
  if (is_transect_scale(analysis)) {
    return(lapply(unname(views_for(analysis)), function(v) list(scale = "transect", view = v)))
  }
  out <- list()
  for (s in scales) for (v in views) {
    if (is.null(view_problem(analysis, s, v))) out[[length(out) + 1]] <- list(scale = s, view = v)
  }
  out
}
