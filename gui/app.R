## Understory Kelp Indicator: Data Interpreter -- desktop application
##
## A front end to the repository's R scripts. It does not reimplement any step:
## it collects the file locations a step needs, starts that step's scripts as
## their own R process, and shows what comes back as it happens.
##
##   1. Clean data      code/rc_full_data_clean.R
##   2. Build metrics   code/metrics_across_scale.R, then code/temporal_trend.R
##   3. Figures         code/visualizations.R
##   Run everything     all three in order, stopping at the first failure
##
## Start it by double-clicking run_data_interpreter.bat in the repository
## folder, which sets up a private copy of R and the packages the first time and
## starts straight away after that. From R or RStudio it also runs as
##   shiny::runApp("gui")        from the repository folder
## or with RStudio's Run App button, or by selecting all of this file and
## running it.
##
## Design follows ReefCheck_BasicLogoGuide.pdf. That guide fixes two colors --
## #003A70 and #00C4B3 -- and two licensed typefaces, and says nothing about
## surfaces or states, so everything beyond the two colors is derived here and
## the derivation is written down rather than left to taste.

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
})

## ---------------------------------------------------------------------------
## Brand
## ---------------------------------------------------------------------------

NAVY       <- "#003A70"   # Pantone 654 C -- the wordmark and the reversed ground
TEAL       <- "#00C4B3"   # Pantone 3265 C -- the diver and "WORLDWIDE"

## Teal measures 2.2:1 on white, which fails WCAG AA for body copy at any size,
## so on light grounds it is a fill and a rule only. This darker teal is what
## carries text when text has to be teal at all.
TEAL_INK   <- "#00756B"

INK        <- "#333C47"
INK_MUTED  <- "#5F6B7A"
PAPER      <- "#FFFFFF"
PANEL      <- "#F7F8FA"
LINE       <- "#D6DCE4"
WARN_AMBER <- "#C97A14"
ERR_RED    <- "#B3402F"

## Museo and Verlag Black are licensed and are not shipped with this repository,
## so the stack falls through to what the machine actually has.
FONT_STACK <- '"Museo Sans","Museo","Segoe UI",system-ui,-apple-system,sans-serif'

## ---------------------------------------------------------------------------
## Where things are
## ---------------------------------------------------------------------------

## The folder this file is in. runApp("gui") -- the launcher, and RStudio's Run
## App button -- makes it the working directory, but selecting all of app.R and
## running it does not: the working directory is then wherever R happens to be,
## and nothing ties the code to its folder. So the candidates are tried in turn
## and the first that holds app.R and www/ wins.
APP_DIR <- local({
  is_app_dir <- function(d) {
    length(d) == 1 && !is.na(d) && nzchar(d) &&
      file.exists(file.path(d, "app.R")) && dir.exists(file.path(d, "www"))
  }
  from_editor <- tryCatch({
    if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable())
      dirname(rstudioapi::getSourceEditorContext()$path) else NULL
  }, error = function(e) NULL)
  from_source <- tryCatch(dirname(sys.frame(1)$ofile), error = function(e) NULL)
  candidates <- list(getwd(), file.path(getwd(), "gui"), from_editor, from_source)
  hit <- Filter(is_app_dir, candidates)
  if (!length(hit)) {
    stop("Could not find the app's folder (gui/, with app.R and www/ in it). ",
         "Start the app with run_data_interpreter.bat, or with ",
         "shiny::runApp(\"gui\") from the repository folder.", call. = FALSE)
  }
  normalizePath(hit[[1]], winslash = "/")
})

## The app directory is gui/; the repository is its parent.
REPO    <- dirname(APP_DIR)
CODE    <- file.path(REPO, "code")
DATA    <- file.path(REPO, "data", "Reef_Check")
RESULTS <- file.path(REPO, "results")
FIGS    <- file.path(REPO, "figs")

## The logos. Shiny serves www/ by itself only when the app is started as a
## folder -- runApp("gui") or Run App -- and not when app.R is run line by
## line, which is why the logos showed for some people and only their alt text
## for others. Naming the folder here serves it either way.
addResourcePath("ukelp", file.path(APP_DIR, "www"))

## Every file location the three steps between them care about. One registry
## rather than one per step, because the steps share files: step 1's output is
## step 2's input, and step 2's outputs are step 3's inputs. Pointing step 1
## somewhere new therefore carries through the chain without being retyped.
##
## `kind` is how it gets browsed for: an existing file, a file to write, or a
## folder. `env` is the environment variable the scripts read it from.
FILES <- list(
  algae   = list(label = "Algae",        env = "UKELP_ALGAE_CSV",   kind = "open",
                 default = file.path(DATA, "Algae_Washington_raw.csv")),
  upc     = list(label = "UPC",          env = "UKELP_UPC_CSV",     kind = "open",
                 default = file.path(DATA, "UPC_Washington_raw.csv")),
  invert  = list(label = "Invertebrates", env = "UKELP_INVERT_CSV", kind = "open",
                 default = file.path(DATA, "Invert_Washington_raw.csv")),
  fish    = list(label = "Fish",         env = "UKELP_FISH_CSV",    kind = "open",
                 default = file.path(DATA, "Fish_Washington_raw.csv")),
  cleaned = list(label = "Combined table", env = "UKELP_CLEANED_CSV", kind = "save",
                 default = file.path(RESULTS, "reef_check_cleaned.csv")),
  transect = list(label = "Transect scale", env = "UKELP_TRANSECT_CSV", kind = "save",
                  default = file.path(RESULTS, "metrics_transect_scale.csv")),
  site    = list(label = "Site scale",   env = "UKELP_SITE_CSV",    kind = "save",
                 default = file.path(RESULTS, "metrics_site_scale.csv")),
  basin   = list(label = "Basin scale",  env = "UKELP_BASIN_CSV",   kind = "save",
                 default = file.path(RESULTS, "metrics_basin_scale.csv")),
  puget   = list(label = "Puget Sound-wide", env = "UKELP_PUGET_CSV", kind = "save",
                 default = file.path(RESULTS, "metrics_puget_sound_scale.csv")),
  trend_site  = list(label = "Trend, site",  env = "UKELP_TREND_SITE_CSV", kind = "save",
                     default = file.path(RESULTS, "trend_site_scale.csv")),
  trend_basin = list(label = "Trend, basin", env = "UKELP_TREND_BASIN_CSV", kind = "save",
                     default = file.path(RESULTS, "trend_basin_scale.csv")),
  trend_puget = list(label = "Trend, Puget Sound", env = "UKELP_TREND_PUGET_CSV", kind = "save",
                     default = file.path(RESULTS, "trend_puget_sound_scale.csv")),
  nmds_points  = list(label = "NMDS, transects", env = "UKELP_NMDS_POINTS_CSV", kind = "save",
                      default = file.path(RESULTS, "nmds_algae_points.csv")),
  nmds_species = list(label = "NMDS, species", env = "UKELP_NMDS_SPECIES_CSV", kind = "save",
                      default = file.path(RESULTS, "nmds_algae_species.csv")),
  nmds_stress  = list(label = "NMDS, stress", env = "UKELP_NMDS_STRESS_CSV", kind = "save",
                      default = file.path(RESULTS, "nmds_algae_stress.csv")),
  figs    = list(label = "Figures folder", env = "UKELP_FIGS_DIR",  kind = "dir",
                 default = FIGS)
)

## `scripts` run in order, as one step: a failure in the first stops the step
## before the second starts.
STEPS <- list(
  list(id = "clean", n = 1, title = "Clean data",
       scripts = "rc_full_data_clean.R",
       blurb = paste("Reads the four raw Reef Check exports, drops Canadian sites",
                     "and the withheld basins, corrects known site-name",
                     "misspellings, removes the duplicate winter 2025 surveys, and",
                     "writes one row per transect. A 0 means looked for and not",
                     "found; a blank means the category was not on that survey's",
                     "datasheet (several were added in 2022-2024)."),
       ins = c("algae", "upc", "invert", "fish"),
       outs = c("cleaned")),
  list(id = "metrics", n = 2, title = "Build metrics",
       scripts = c("metrics_across_scale.R", "temporal_trend.R", "nmds_algae.R"),
       blurb = paste("Turns the combined table into transect, site, basin and Puget",
                     "Sound-wide tables -- each average with its standard deviation",
                     "and variance -- then computes the temporal trend (each year's",
                     "density as a percentage of the preceding years' mean) and an",
                     "NMDS of the understory algae community, one point per",
                     "transect. The NMDS takes a few minutes, and is only re-run",
                     "when the transect table has changed."),
       ins = c("cleaned"),
       outs = c("transect", "site", "basin", "puget",
                "trend_site", "trend_basin", "trend_puget",
                "nmds_points", "nmds_species", "nmds_stress")),
  list(id = "figures", n = 3, title = "Figures",
       scripts = "visualizations.R",
       blurb = paste("Draws every figure set ticked below into the figures folder,",
                     "by analysis, scale, view and data type. This is the slowest",
                     "step: the default selection is about 320 files and takes",
                     "20-30 minutes."),
       ins = c("site", "basin", "puget"),
       outs = c("figs"))
)
names(STEPS) <- vapply(STEPS, function(s) s$id, character(1))

## ---------------------------------------------------------------------------
## What the figures can show
## ---------------------------------------------------------------------------
##
## The figure definitions -- and the choices that name them -- live in
## code/figure_functions.R, which visualizations.R uses as well, so the preview
## and the files Run writes are the same figures by construction. It is loaded
## here, before the UI is built, because the dropdowns are made from it.
FIG_ENV <- new.env(parent = globalenv())
FIG_OK <- tryCatch({
  sys.source(file.path(CODE, "figure_functions.R"), envir = FIG_ENV)
  TRUE
}, error = function(e) {
  warning("Could not load figure_functions.R: ", conditionMessage(e))
  FALSE
})

if (FIG_OK) {
  ANALYSES   <- FIG_ENV$ANALYSES
  FIG_SCALES <- FIG_ENV$SCALES
  VIEWS      <- FIG_ENV$VIEWS
  FIG_GROUPS <- setNames(vapply(FIG_ENV$metric_groups, function(g) g$folder, character(1)),
                         vapply(FIG_ENV$metric_groups, function(g) g$label, character(1)))
} else {
  ANALYSES <- FIG_SCALES <- VIEWS <- FIG_GROUPS <- character(0)
}

## the tables the figures read, by FILES key
FIG_TABLES <- c("transect", "site", "basin", "puget",
                "trend_site", "trend_basin", "trend_puget",
                "nmds_points", "nmds_species", "nmds_stress")

## ---------------------------------------------------------------------------
## The three withheld basins
## ---------------------------------------------------------------------------
##
## Every script leaves these out by default. Each tab carries one checkbox per
## basin, and the ticked names go to the scripts as UKELP_INCLUDE_BASINS.
##
## Steps 2 and 3 can only include what step 1 actually kept: if the cleaned
## table was written with the box unticked, the rows are simply not there, and
## ticking the box downstream cannot conjure them back. basins_present() is how
## the app checks, so it can say so rather than quietly producing the same
## figures either way.
EXCLUDED_BASINS <- c("Northern Coast",
                     "Western Strait of Juan de Fuca",
                     "Eastern Strait of Juan de Fuca")

#: Each withheld basin gets its own checkbox, so one can be brought back without
#: the other two. The key is what the checkbox input is named; the value is the
#: basin name as the scripts and the cleaned tables spell it.
INCLUDE_KEYS <- c(north = "Northern Coast",
                  west  = "Western Strait of Juan de Fuca",
                  east  = "Eastern Strait of Juan de Fuca")

INCLUDE_HEADING <- "Withheld basins"
INCLUDE_BLURB <- paste("Left out of every output unless ticked. Tick them",
                       "independently -- one, two or all three. With any of them",
                       "kept, the Puget Sound scale also offers Washington-wide.")

#: Which of the three appear in a csv's `basin` column. Names are compared with
#: underscores collapsed to spaces, since the cleaned tables carry the
#: underscored form.
basins_present <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) return(NULL)
  d <- tryCatch(read.csv(path, check.names = FALSE), error = function(e) NULL)
  if (is.null(d) || !"basin" %in% names(d)) return(NULL)
  found <- unique(gsub("_", " ", as.character(d$basin)))
  intersect(EXCLUDED_BASINS, found)
}

## ---------------------------------------------------------------------------
## Helpers
## ---------------------------------------------------------------------------

`%||%` <- function(a, b) if (is.null(a)) b else a

rscript_path <- function() {
  exe <- if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript"
  for (p in c(file.path(R.home("bin"), exe), Sys.which("Rscript"))) {
    if (nzchar(p) && file.exists(p)) return(normalizePath(p, winslash = "/"))
  }
  NULL
}

## A native file dialog, so a person browses the real filesystem and the app
## gets a real path. A browser upload would copy the file somewhere temporary
## and hand R a name it cannot report back usefully.
##
## The dialog is parented to a 1x1 topmost window that is created and destroyed
## around it. Without that parent the dialog opens behind the browser, which
## looks exactly like the button doing nothing. The path can always be typed or
## pasted into the field instead, so the dialog is never the only way in.
with_tk_parent <- function(fn) {
  if (!requireNamespace("tcltk", quietly = TRUE)) return(NULL)
  tt <- tryCatch({
    w <- tcltk::tktoplevel()
    tcltk::tkwm.title(w, " ")
    tcltk::tkwm.geometry(w, "1x1+0+0")
    tcltk::tcl("wm", "attributes", w, "-topmost", TRUE)
    tcltk::tkfocus(w)
    w
  }, error = function(e) NULL)
  if (is.null(tt)) return(NULL)
  on.exit(try(tcltk::tkdestroy(tt), silent = TRUE), add = TRUE)
  tryCatch(fn(tt), error = function(e) NULL)
}

clean_path <- function(x) {
  if (is.null(x) || !length(x) || !nzchar(x[1]) || identical(x[1], "NA")) return(NULL)
  normalizePath(x[1], winslash = "/", mustWork = FALSE)
}

pick_open <- function(caption, initial) {
  clean_path(with_tk_parent(function(tt) as.character(tcltk::tkgetOpenFile(
    parent = tt, title = caption,
    initialdir = if (nzchar(initial)) dirname(initial) else REPO,
    filetypes = "{{CSV files} {.csv}} {{All files} {*}}"))))
}

pick_save <- function(caption, initial, ext = ".csv",
                      types = "{{CSV files} {.csv}} {{All files} {*}}") {
  clean_path(with_tk_parent(function(tt) as.character(tcltk::tkgetSaveFile(
    parent = tt, title = caption,
    initialfile = basename(initial),
    initialdir = if (nzchar(initial)) dirname(initial) else REPO,
    defaultextension = ext,
    filetypes = types))))
}

pick_dir <- function(caption, initial) {
  clean_path(with_tk_parent(function(tt) as.character(tcltk::tkchooseDirectory(
    parent = tt, title = caption,
    initialdir = if (nzchar(initial)) initial else REPO))))
}

## What the GUI shows beneath a path.
describe <- function(path, kind) {
  if (is.null(path) || !nzchar(path)) {
    return(list(ok = FALSE, text = "Nothing chosen", class = "meta-none"))
  }
  if (kind == "dir") {
    if (!dir.exists(path)) return(list(ok = TRUE, text = "will be created",
                                       class = "meta-none"))
    n <- length(list.files(path, pattern = "[.]png$", recursive = TRUE))
    return(list(ok = TRUE,
                text = sprintf("%s figures currently in this folder",
                               format(n, big.mark = ",")),
                class = "meta-ok"))
  }
  info <- file.info(path)
  if (is.na(info$size)) {
    return(if (kind == "save")
             list(ok = TRUE, text = "will be created", class = "meta-none")
           else list(ok = FALSE, text = "File not found", class = "meta-bad"))
  }
  size <- if (info$size < 1024^2) sprintf("%.0f KB", info$size / 1024)
          else sprintf("%.1f MB", info$size / 1024^2)
  rows <- tryCatch(max(0L, length(readLines(path, warn = FALSE)) - 1L),
                   error = function(e) NA_integer_)
  head <- if (kind == "save") "exists, will be overwritten  \u00b7  " else ""
  text <- if (is.na(rows)) size
          else sprintf("%s rows  \u00b7  %s", format(rows, big.mark = ","), size)
  list(ok = TRUE,
       text = sprintf("%s%s  \u00b7  %s", head, text,
                      format(info$mtime, "%Y-%m-%d %H:%M")),
       class = "meta-ok")
}

## ---------------------------------------------------------------------------
## Theme
## ---------------------------------------------------------------------------

rc_theme <- bs_theme(
  version = 5, bg = PAPER, fg = INK,
  primary = NAVY, secondary = INK_MUTED, success = TEAL_INK
)

## Only the brand colors and the font go through sprintf(): the whole sheet is
## longer than the 8,192 characters sprintf() accepts as a format.
app_css <- paste0(sprintf('
  :root { --navy:%s; --teal:%s; --teal-ink:%s; --ink:%s; --ink-muted:%s;
          --panel:%s; --line:%s; --warn:%s; --err:%s; }
  body { font-family: %s; background:#fff; }
', NAVY, TEAL, TEAL_INK, INK, INK_MUTED, PANEL, LINE,
                          WARN_AMBER, ERR_RED, FONT_STACK), '
  /* Banner. The supplied logo is a white outline, so it needs the navy ground
     the logo guide itself shows the reversed mark on. */
  .rc-banner { background: var(--navy); color:#fff; display:flex;
               align-items:center; gap:22px; padding:18px 28px 16px 28px; }
  .rc-banner img { height:74px; width:auto; display:block; }
  .rc-title { font-size:26px; font-weight:700; line-height:1.15; margin:0; color:#fff; }
  .rc-sub { font-size:13px; color:#A9BCD1; margin:3px 0 0 0; }

  /* The one gradient: navy into teal, both brand colors at full strength. A
     single object carrying no type, which is where a gradient earns its keep. */
  .rc-rule { height:5px; background:linear-gradient(90deg,var(--navy),var(--teal)); }

  .rc-wrap { max-width:1180px; margin:0 auto; padding:20px 24px 44px 24px; }

  .rc-card { background:var(--panel); border:1px solid var(--line);
             border-radius:10px; padding:18px 20px; margin-bottom:18px; }
  .rc-card h2 { font-size:17px; font-weight:600; color:var(--navy); margin:0 0 4px 0; }
  .rc-card p.blurb { font-size:13px; color:var(--ink-muted); margin:0 0 14px 0; }

  /* One file: name, an editable path, Browse. The path is a field rather than
     static text so it can be pasted into when a dialog is inconvenient. */
  .rc-file { display:grid; grid-template-columns:150px 1fr auto; align-items:start;
             gap:12px; padding:9px 0; border-bottom:1px dashed var(--line); }
  .rc-file:last-of-type { border-bottom:0; }
  .rc-file .who { font-weight:600; font-size:13px; color:var(--ink); padding-top:7px; }
  .rc-file .form-group, .rc-file .shiny-input-container { margin-bottom:0; width:100%; }
  .rc-file input.form-control { font-family:Consolas,"Courier New",monospace;
             font-size:12px; padding:5px 9px; border-color:var(--line); }
  .rc-meta { font-size:11.5px; margin-top:3px; }
  .meta-ok { color:var(--teal-ink); } .meta-bad { color:var(--err); }
  .meta-none { color:var(--ink-muted); }

  .btn-browse { background:#fff; border:1px solid var(--line); color:var(--ink);
                font-size:13px; padding:5px 14px; border-radius:7px; margin-top:1px; }
  .btn-browse:hover { border-color:var(--navy); color:var(--navy); }

  .btn-run { background:var(--navy); border:0; color:#fff; font-weight:600;
             font-size:14px; padding:9px 22px; border-radius:8px; }
  .btn-run:hover { background:#002B55; color:#fff; }
  .btn-all { background:var(--teal-ink); border:0; color:#fff; font-weight:600;
             font-size:15px; padding:11px 26px; border-radius:8px; }
  .btn-all:hover { background:#005C55; color:#fff; }
  .btn-stop { background:#fff; border:1px solid var(--err); color:var(--err);
              font-size:14px; padding:8px 18px; border-radius:8px; }

  .rc-status { font-size:13px; margin-left:14px; }
  .s-idle { color:var(--ink-muted); } .s-busy { color:var(--navy); font-weight:600; }
  .s-ok { color:var(--teal-ink); font-weight:600; }
  .s-warn { color:var(--warn); font-weight:600; }
  .s-err { color:var(--err); font-weight:600; }

  .rc-log { background:#00223F; color:#E8F1F8; border-radius:8px; padding:12px 14px;
            height:280px; overflow-y:auto; font-family:Consolas,"Courier New",monospace;
            font-size:12px; line-height:1.45; white-space:pre-wrap; word-break:break-word; }

  /* Navigation */
  .nav-pills { gap:8px; margin-bottom:16px; }
  .nav-pills .nav-link { font-size:13.5px; color:var(--ink-muted); background:#fff;
            border:1px solid var(--line); border-radius:8px; padding:8px 16px; }
  .nav-pills .nav-link:hover { color:var(--navy); border-color:var(--navy); }
  .nav-pills .nav-link.active { background:#fff !important; color:var(--navy) !important;
            border:2px solid var(--teal); font-weight:600; }

  /* The choice columns on the Figures tab */
  .rc-choices { display:grid; grid-template-columns:repeat(5,minmax(0,1fr)); gap:20px; }
  /* Shiny gives every input a fixed 300px, which five columns cannot hold */
  .rc-choices .shiny-input-container { width:100% !important; }
  .rc-choices h3 { font-size:12px; font-weight:700; letter-spacing:.06em;
                   text-transform:uppercase; color:var(--navy);
                   margin:0 0 8px 0; padding-bottom:5px;
                   border-bottom:2px solid var(--teal); }
  .rc-choices .shiny-options-group { margin-top:0; }
  .rc-choices .checkbox, .rc-choices .radio { margin:0 0 5px 0; }
  .rc-choices .checkbox label, .rc-choices .radio label { font-size:13px; color:var(--ink);
                                                          line-height:1.4; }
  @media (max-width: 980px) { .rc-choices { grid-template-columns:1fr 1fr; gap:14px; } }

  .rc-plan { background:#fff; border:1px solid var(--line); border-radius:8px;
             padding:12px 14px; margin-top:14px; font-size:12.5px; }
  .rc-plan .hd { font-weight:600; color:var(--navy); margin-bottom:6px; }
  .rc-plan table { width:100%; border-collapse:collapse; }
  .rc-plan td { padding:3px 0; vertical-align:top; }
  .rc-plan td.f { font-family:Consolas,"Courier New",monospace; color:var(--ink);
                  white-space:nowrap; padding-right:14px; }
  .rc-plan td.d { color:var(--ink-muted); }
  .rc-plan td.c { text-align:right; color:var(--teal-ink); font-weight:600;
                  white-space:nowrap; padding-left:14px; }
  .rc-plan .tot { border-top:1px solid var(--line); font-weight:600; color:var(--navy); }
  .rc-plan .none { color:var(--warn); font-weight:600; }

  /* The withheld basins, one checkbox each, set apart from the rows above */
  .rc-include { margin-top:14px; padding-top:12px; border-top:1px solid var(--line); }
  .rc-include .hd { font-size:12px; font-weight:700; letter-spacing:.06em;
                    text-transform:uppercase; color:var(--navy); margin-bottom:2px; }
  .rc-include .sub { font-size:11.5px; color:var(--ink-muted); margin-bottom:7px; }
  .rc-include-row { display:grid; grid-template-columns:repeat(3,1fr); gap:10px; }
  @media (max-width: 760px) { .rc-include-row { grid-template-columns:1fr; } }
  .rc-include .checkbox { margin:0; }
  .rc-include .checkbox label { font-size:13px; color:var(--ink); }
  .rc-include .note { font-size:11.5px; margin-top:4px; margin-left:21px; }
  .rc-include .note.warn { color:var(--warn); }
  .rc-include .note.ok { color:var(--teal-ink); }
  .rc-include .note.muted { color:var(--ink-muted); }

  /* The preview: four choices that define the figure, then where or what, and
     the y-axis switch */
  .rc-preview-controls { display:grid; grid-template-columns:repeat(4,minmax(0,1fr));
                         gap:16px; margin-bottom:10px; }
  .rc-preview-controls .shiny-input-container { width:100% !important; }
  .rc-preview-stage img { width:100%; height:auto; display:block; }
  .rc-preview-controls h3 { font-size:12px; font-weight:700; letter-spacing:.06em;
                            text-transform:uppercase; color:var(--navy);
                            margin:0 0 7px 0; padding-bottom:5px;
                            border-bottom:2px solid var(--teal); }
  .rc-preview-controls .form-group { margin-bottom:0; }
  .rc-preview-controls select { font-size:13px; }
  .rc-preview-controls .rc-hint { font-size:11.5px; color:var(--ink-muted); margin-top:5px; }
  .rc-preview-controls .form-switch { margin-top:6px; }
  .rc-preview-controls .form-check-label { font-size:13px; color:var(--ink); }
  @media (max-width: 900px) { .rc-preview-controls { grid-template-columns:1fr 1fr; } }

  .rc-preview-stage { background:#fff; border:1px solid var(--line);
                      border-radius:8px; padding:10px; margin-top:10px; }
  .rc-pv-msg { font-size:12.5px; margin-top:8px; }
  .rc-pv-msg.warn { color:var(--warn); font-weight:600; }
  .rc-pv-msg.muted { color:var(--ink-muted); }
  .rc-pv-target { font-size:11.5px; color:var(--ink-muted); margin-left:14px;
                  font-family:Consolas,"Courier New",monospace; }

  .rc-chain { font-size:13px; color:var(--ink); line-height:1.9; margin:0 0 6px 0; }
  .rc-chain .n { display:inline-block; min-width:22px; font-weight:700; color:var(--navy); }
  .rc-foot { font-size:11.5px; color:var(--ink-muted); margin-top:8px; }
')

## ---------------------------------------------------------------------------
## UI
## ---------------------------------------------------------------------------

file_row <- function(key) {
  f <- FILES[[key]]
  div(
    class = "rc-file",
    div(class = "who", f$label),
    div(
      textInput(paste0("path_", key), label = NULL, value = f$default, width = "100%"),
      div(class = "rc-meta", uiOutput(paste0("meta_", key), inline = TRUE))
    ),
    actionButton(paste0("browse_", key), "Browse\u2026", class = "btn-browse")
  )
}

runs_line <- function(step) {
  sprintf("Runs %s", paste0("code/", step$scripts, collapse = ", then "))
}

step_panel <- function(step) {
  nav_panel(
    title = sprintf("%d.  %s", step$n, step$title),
    value = step$id,
    div(
      class = "rc-card",
      h2(sprintf("%d.  %s", step$n, step$title)),
      p(class = "blurb", step$blurb),
      tags$div(style = "font-size:12px;color:#5F6B7A;margin:0 0 6px 0;", runs_line(step))
    ),
    ## The preview comes first on the Figures tab: looking at one figure is how
    ## a person works out which sets are worth generating, so it belongs above
    ## the checkboxes that decide what Run writes rather than below them.
    if (identical(step$id, "figures")) preview_card() else NULL,
    if (identical(step$id, "figures")) {
      ## The tables this step reads are step 2's outputs and are set on that
      ## tab, so repeating the pickers here would be two places to change one
      ## path. This tab chooses what to draw instead.
      div(
        class = "rc-card",
        h2("What to draw"),
        p(class = "blurb",
          paste("Every combination of the boxes below is one set of figures,",
                "written to figs/<analysis>/<scale>/<view>/<data type>/. These",
                "are independent of the preview above.")),
        div(
          class = "rc-choices",
          div(tags$h3("Analysis"),
              checkboxGroupInput("fig_analyses", label = NULL, choices = ANALYSES,
                                 selected = unname(ANALYSES))),
          ## The site scale is one file per site per data type per view --
          ## about 1,500 files -- so it is there to be ticked, not ticked.
          div(tags$h3("Scale"),
              checkboxGroupInput("fig_scales", label = NULL, choices = FIG_SCALES,
                                 selected = setdiff(unname(FIG_SCALES), "site"))),
          div(tags$h3("View"),
              checkboxGroupInput("fig_views", label = NULL, choices = VIEWS,
                                 selected = unname(VIEWS))),
          div(tags$h3("Data type"),
              checkboxGroupInput("fig_groups", label = NULL, choices = FIG_GROUPS,
                                 selected = unname(FIG_GROUPS))),
          div(tags$h3("Y-axis"),
              radioButtons("fig_ylock", label = NULL,
                           choices = c("Float -- each panel its own range" = "float",
                                       "Lock -- one range for every panel" = "lock"),
                           selected = "float"))
        ),
        include_box(step$id),
        div(style = "margin-top:4px;", uiOutput("fig_plan"))
      )
    } else {
      div(
        class = "rc-card",
        h2("Reads"),
        lapply(step$ins, file_row),
        include_box(step$id)
      )
    },
    div(
      class = "rc-card",
      h2("Writes"),
      lapply(step$outs, file_row)
    ),
    div(
      class = "rc-card",
      div(style = "display:flex;align-items:center;flex-wrap:wrap;",
          actionButton(paste0("run_", step$id), "Run this step", class = "btn-run"),
          div(style = "width:10px;"),
          actionButton("reset", "Reset all locations", class = "btn-browse"))
    )
  )
}

## The withheld basins, one checkbox each. `scope` is the step id, or "all" for
## the Run-everything tab, and becomes the input name prefix.
include_box <- function(scope) {
  div(
    class = "rc-include",
    tags$div(class = "hd", INCLUDE_HEADING),
    tags$div(class = "sub", INCLUDE_BLURB),
    div(class = "rc-include-row",
        lapply(names(INCLUDE_KEYS), function(k)
          checkboxInput(paste0("include_", scope, "_", k),
                        INCLUDE_KEYS[[k]], value = FALSE))),
    uiOutput(paste0("include_note_", scope))
  )
}

## ---------------------------------------------------------------------------
## Explore data
## ---------------------------------------------------------------------------
##
## Build one figure and look at it before anything is written. Four choices
## define the figure -- the same four that name the folders Run writes into:
##
##   analysis  temporal trend, average density, density with its SD band, or
##             the NMDS of the algae community
##   scale     site, basin or Puget Sound (the NMDS: the transect)
##   view      species within one geography (a panel per species), one species
##             across geographies (a panel per place), or every species
##             composited on one set of axes for one geography -- for the NMDS,
##             what colours the points: basin, site, year or depth zone
##   data      algae or one of the four UPC groups (the NMDS: algae)
##
## and a second row then asks only for what that figure still needs -- which
## site, basin or region, which species, or for the NMDS which species sizes
## the points -- plus the y-axis switch (for the NMDS, arrows and ellipses).
## The slots relabel themselves with the analysis, so no control changes
## meaning without saying so.
preview_card <- function() {
  div(
    class = "rc-card",
    h2("Explore data"),
    p(class = "blurb",
      paste("Build one figure and look at it. These choices are independent of",
            "the checkboxes below -- they decide what you are looking at, not",
            "what Run writes.")),
    div(
      class = "rc-preview-controls",
      div(tags$h3("Analysis"),
          selectInput("pv_analysis", NULL, choices = ANALYSES, selected = "density",
                      width = "100%")),
      div(uiOutput("pv_scale_ui")),
      div(uiOutput("pv_view_ui")),
      div(uiOutput("pv_group_ui"))
    ),
    div(
      class = "rc-preview-controls",
      div(uiOutput("pv_where_ui")),
      div(uiOutput("pv_cat_ui")),
      div(uiOutput("pv_info_ui")),
      div(uiOutput("pv_ylock_ui"))
    ),
    uiOutput("pv_status"),
    div(class = "rc-preview-stage", imageOutput("pv_plot", height = "auto")),
    div(style = "display:flex;align-items:center;flex-wrap:wrap;margin-top:12px;",
        actionButton("pv_save", "Save this figure", class = "btn-run"),
        div(style = "width:8px;"),
        actionButton("pv_save_as", "Save as\u2026", class = "btn-browse"),
        uiOutput("pv_target", inline = TRUE))
  )
}

ui <- page_fluid(
  theme = rc_theme,
  tags$head(tags$style(HTML(app_css)),
            tags$title("Understory Kelp Indicator: Data Interpreter")),

  div(class = "rc-banner",
      tags$img(src = "ukelp/rc_logo.png", alt = "Reef Check"),
      tags$img(src = "ukelp/seaq_logo.png", alt = "Seattle Aquarium"),
      div(h1("Understory Kelp Indicator: Data Interpreter", class = "rc-title"),
          p("Reef Check  \u00b7  Seattle Aquarium", class = "rc-sub"))),
  div(class = "rc-rule"),

  div(
    class = "rc-wrap",

    navset_pill(
      id = "nav",
      !!!unname(lapply(STEPS, step_panel)),
      nav_panel(
        title = "Run everything",
        value = "all",
        div(
          class = "rc-card",
          h2("Run everything"),
          p(class = "blurb",
            paste("Runs all three steps in order, each one reading what the step",
                  "before it wrote. If a step fails, the chain stops there --",
                  "nothing after it runs on stale data.")),
          div(class = "rc-chain",
              lapply(STEPS, function(s)
                div(span(class = "n", paste0(s$n, ".")), s$title,
                    span(style = "color:#5F6B7A;",
                         paste0("  \u2014  ", paste0("code/", s$scripts, collapse = ", ")))))),
          include_box("all"),
          div(style = "margin-top:14px;",
              actionButton("run_all", "Run all three steps", class = "btn-all")),
          div(class = "rc-foot",
              paste("File locations come from the three step tabs, and the",
                    "figures step draws whatever is ticked there. Steps 1 and 2",
                    "take seconds; the figures step takes the longest -- 20-30",
                    "minutes for the default selection."))
        )
      )
    ),

    ## One log for every step, below the tabs, so switching tabs never hides
    ## what is running.
    div(
      class = "rc-card",
      div(style = "display:flex;align-items:center;flex-wrap:wrap;margin-bottom:10px;",
          h2("Output from R", style = "margin:0;"),
          uiOutput("status", inline = TRUE),
          div(style = "flex:1;"),
          uiOutput("stop_ui", inline = TRUE)),
      div(id = "logbox", class = "rc-log", uiOutput("log", inline = TRUE)),
      div(class = "rc-foot", textOutput("rinfo", inline = TRUE))
    )
  ),

  ## Keep the log pinned to the newest line as it grows.
  tags$script(HTML("
    $(document).on('shiny:value', function(e) {
      if (e.name === 'log') {
        setTimeout(function(){
          var b = document.getElementById('logbox');
          if (b) b.scrollTop = b.scrollHeight;
        }, 20);
      }
    });
  "))
)

## ---------------------------------------------------------------------------
## Server
## ---------------------------------------------------------------------------

## ---------------------------------------------------------------------------
## Quitting when the browser closes
## ---------------------------------------------------------------------------
##
## Closing the browser tab should end the program, so the console window the
## launcher opened goes away with it. Shiny ends a session on any disconnect --
## including a page refresh or a brief hiccup -- so quitting immediately would
## kill the app every time someone hit F5. Instead the session count is tracked
## here, at file scope so it survives between sessions, and the app only stops
## once nothing has reconnected for a few seconds.
##
## Only when run non-interactively. Started from RStudio with runApp(), closing
## the viewer should leave the app running to be reopened.
QUIT_GRACE_SECONDS <- 8

session_tally <- local({
  n <- 0L
  list(open  = function() n <<- n + 1L,
       close = function() n <<- n - 1L,
       count = function() n)
})

server <- function(input, output, session) {

  if (!interactive()) {
    session_tally$open()
    session$onSessionEnded(function() {
      session_tally$close()
      later::later(function() {
        if (session_tally$count() <= 0) shiny::stopApp()
      }, QUIT_GRACE_SECONDS)
    })
  }


  rscript <- rscript_path()

  rv <- reactiveValues(
    proc = NULL,                 # processx handle for the running script
    current = NULL,              # which step id is running
    scripts = character(0),      # that step's scripts still to run
    queue = character(0),        # step ids still to run, for "Run everything"
    all_run = FALSE,             # chain started from the "Run everything" tab
    tables_version = 0L,         # bumped when a step finishes, so the preview rereads
    log = character(0),
    state = list(text = "Not run yet", class = "s-idle")
  )

  output$rinfo <- renderText({
    if (is.null(rscript))
      "R was not found. Install R, or start this app from an R session."
    else sprintf("Using %s  \u00b7  %s  \u00b7  packages from %s", R.version.string, rscript,
                 .libPaths()[1])
  })

  ## ---- the path fields ---------------------------------------------------
  ## Each one is a text input, so a path can be typed or pasted; Browse fills it
  ## in from a native dialog.

  current_path <- function(key) {
    v <- input[[paste0("path_", key)]]
    if (is.null(v)) FILES[[key]]$default else trimws(v)
  }

  lapply(names(FILES), function(key) {
    f <- FILES[[key]]

    observeEvent(input[[paste0("browse_", key)]], {
      here <- current_path(key)
      chosen <- switch(
        f$kind,
        open = pick_open(sprintf("Choose the %s file", f$label), here),
        save = pick_save(sprintf("Save %s as", f$label), here),
        dir  = pick_dir(sprintf("Choose the %s", f$label), here)
      )
      if (!is.null(chosen)) {
        updateTextInput(session, paste0("path_", key), value = chosen)
      } else {
        ## Either the dialog was cancelled or Tk could not open one. Say so
        ## rather than leaving the button looking broken.
        if (!requireNamespace("tcltk", quietly = TRUE)) {
          showNotification(
            "A file dialog could not be opened on this computer. Type or paste the path instead.",
            type = "warning", duration = 8)
        }
      }
    }, ignoreInit = TRUE)

    output[[paste0("meta_", key)]] <- renderUI({
      rv$tables_version
      d <- describe(current_path(key), f$kind)
      span(class = d$class, d$text)
    })
  })

  observeEvent(input$reset, {
    for (key in names(FILES)) {
      updateTextInput(session, paste0("path_", key), value = FILES[[key]]$default)
    }
    showNotification("File locations reset to this repository.", duration = 4)
  }, ignoreInit = TRUE)

  ## ---- the withheld basins -----------------------------------------------

  ## Which upstream table each step would have to find them in. Step 1 reads the
  ## raw exports, which always carry every basin, so it has nothing to check.
  include_source <- list(clean = NULL, metrics = "cleaned", figures = "basin")

  ## The basins ticked on one tab. Each has its own checkbox, so this is a set
  ## rather than a flag -- a person can bring back the Eastern Strait alone.
  include_picked <- function(scope) {
    keys <- names(INCLUDE_KEYS)
    ticked <- keys[vapply(keys, function(k)
      isTRUE(input[[paste0("include_", scope, "_", k)]]), logical(1))]
    unname(INCLUDE_KEYS[ticked])
  }

  ## Which tab's ticks govern the step about to run. A chain started from "Run
  ## everything" answers to that tab for all three steps -- step 1 rebuilds the
  ## cleaned table within the same chain, so whatever is ticked there is always
  ## honorable, which is not true of the per-step boxes.
  include_for <- function(step_id) {
    if (isTRUE(rv$all_run)) include_picked("all") else include_picked(step_id)
  }

  ## NULL when there is nothing to check (step 1, or the upstream file is not
  ## there yet); otherwise which of the withheld basins upstream actually holds.
  include_available <- function(step_id) {
    key <- include_source[[step_id]]
    if (is.null(key)) return(NULL)
    basins_present(current_path(key))
  }

  ## Shiny suspends an output while its tab is hidden and resumes it when the
  ## tab is shown, which it works out from element visibility. Inside a navset
  ## that resume is unreliable, and an output stuck "recalculating" reads as a
  ## broken panel. These are all cheap -- a checkbox state, or a header read off
  ## a small csv -- so they are simply never suspended.
  keep_live <- function(...) {
    for (nm in c(...)) outputOptions(output, nm, suspendWhenHidden = FALSE)
  }

  one_or_more <- function(x) if (length(x) == 1) x else
    paste(paste(head(x, -1), collapse = ", "), "and", tail(x, 1))

  ## The run-everything box needs no availability check: step 1 runs first in
  ## the same chain, so whatever is ticked there is what the later steps get.
  output$include_note_all <- renderUI({
    picked <- include_picked("all")
    if (!length(picked))
      div(class = "note muted",
          "All three steps will leave every withheld basin out.")
    else
      div(class = "note ok",
          sprintf("All three steps will keep %s, starting from the cleaned table.",
                  one_or_more(picked)))
  })

  lapply(names(STEPS), function(id) {
    output[[paste0("include_note_", id)]] <- renderUI({
      rv$tables_version
      picked <- include_picked(id)
      if (!length(picked)) {
        return(div(class = "note muted",
                   if (is.null(include_source[[id]]))
                     "None will be kept in the cleaned table."
                   else "None will be included."))
      }
      key <- include_source[[id]]
      if (is.null(key)) {
        return(div(class = "note ok",
                   sprintf("%s will be kept in the cleaned table.",
                           one_or_more(picked))))
      }

      have <- include_available(id)
      src <- FILES[[key]]$label
      if (is.null(have)) {
        return(div(class = "note muted",
                   sprintf("Cannot tell yet \u2014 %s has not been written.", src)))
      }
      missing <- setdiff(picked, have)
      usable <- intersect(picked, have)
      if (!length(usable)) {
        div(class = "note warn",
            sprintf("Not available: %s does not hold %s. Re-run step 1 with %s ticked.",
                    src, one_or_more(picked),
                    if (length(picked) == 1) "it" else "them"))
      } else if (length(missing)) {
        div(class = "note warn",
            sprintf("%s will be included; %s is not in %s.",
                    one_or_more(usable), one_or_more(missing), src))
      } else {
        div(class = "note ok",
            sprintf("%s will be included.", one_or_more(usable)))
      }
    })
  })

  keep_live("include_note_all",
            paste0("include_note_", names(STEPS)))

  ## ---- the tables the figures read ----------------------------------------
  ##
  ## Figures are drawn in this process rather than by starting R again: a
  ## subprocess costs about 1.3 s per preview (R start-up plus the packages plus
  ## reading the tables), which is tiresome across an exploration loop. The
  ## tables are reread whenever a path changes or a step finishes, so a preview
  ## never shows figures built from a table that has since been rewritten.
  fig_tables <- reactive({
    rv$tables_version
    if (!FIG_OK) return(FALSE)
    paths <- setNames(lapply(FIG_TABLES, current_path), FIG_TABLES)
    ok <- tryCatch({
      FIG_ENV$load_figure_tables(paths)
      !is.null(FIG_ENV$df_basin)
    }, error = function(e) FALSE)
    ok
  })

  ## ---- what the figures step will draw -----------------------------------

  fig_selection <- reactive({
    list(analyses = input$fig_analyses %||% character(0),
         scales   = input$fig_scales   %||% character(0),
         views    = input$fig_views    %||% character(0),
         groups   = input$fig_groups   %||% character(0),
         y_lock   = identical(input$fig_ylock, "lock"))
  })

  label_of <- function(choices, value) names(choices)[match(value, choices)]

  output$fig_plan <- renderUI({
    if (!FIG_OK) return(div(class = "rc-plan", span(class = "none",
      "code/figure_functions.R could not be loaded -- see the R console.")))
    sel <- fig_selection()
    sets <- list()
    for (a in sel$analyses) for (st in FIG_ENV$figure_sets(a, sel$scales, sel$views)) {
      if (FIG_ENV$is_nmds(a) || length(sel$groups))
        sets[[length(sets) + 1]] <- c(list(analysis = a), st)
    }

    if (!length(sets)) {
      return(div(class = "rc-plan",
                 span(class = "none", "Nothing selected \u2014 this step would write no figures."),
                 div(style = "color:#5F6B7A;margin-top:4px;",
                     paste("Pick at least one analysis, scale, view and data type",
                           "(a composite has no standard-deviation version, and",
                           "Puget Sound has no 'across' view)."))))
    }

    have <- isTRUE(fig_tables())
    ## the NMDS is the algae community whatever the data-type boxes say
    groups_for <- function(a) {
      if (FIG_ENV$is_nmds(a)) list(FIG_ENV$group_by_folder("algae"))
      else Filter(function(g) g$folder %in% sel$groups, FIG_ENV$metric_groups)
    }
    count <- function(st) {
      if (!have) return(NA_integer_)
      sum(vapply(groups_for(st$analysis), function(g)
        length(FIG_ENV$figure_targets(st$analysis, st$scale, st$view, g)), integer(1)))
    }
    counts <- vapply(sets, count, integer(1))

    rows <- Map(function(st, n) {
      nm <- FIG_ENV$is_nmds(st$analysis)
      tags$tr(
        tags$td(class = "f", sprintf("figs/%s/%s/%s/", FIG_ENV$ANALYSIS_FOLDER[[st$analysis]],
                                     FIG_ENV$SCALE_FOLDER[[st$scale]], st$view)),
        tags$td(class = "d", paste(label_of(ANALYSES, st$analysis), "\u00b7",
                                   if (nm) "Transect" else label_of(FIG_SCALES, st$scale), "\u00b7",
                                   if (nm) paste("colored by", tolower(label_of(FIG_ENV$NMDS_COLOR_BY, st$view)))
                                   else label_of(VIEWS, st$view))),
        tags$td(class = "c", if (is.na(n)) "\u2014" else format(n, big.mark = ","))
      )
    }, sets, counts)

    total <- if (!have) NA_integer_ else sum(counts)

    div(
      class = "rc-plan",
      div(class = "hd", sprintf("%d figure %s, across %d data %s, y-axis %s",
                                length(sets),
                                if (length(sets) == 1) "set" else "sets",
                                length(sel$groups),
                                if (length(sel$groups) == 1) "type" else "types",
                                if (sel$y_lock) "locked" else "floating")),
      tags$table(
        tags$tbody(
          rows,
          tags$tr(class = "tot",
                  tags$td(class = "f", "total"),
                  tags$td(class = "d", if (!have)
                    "run step 2 first for a file count" else ""),
                  tags$td(class = "c",
                          if (is.na(total)) "\u2014"
                          else paste(format(total, big.mark = ","), "files")))
        )
      )
    )
  })

  keep_live("fig_plan")

  ## ---- the preview -------------------------------------------------------

  pv_group_def <- reactive({
    g <- input$pv_group
    if (is.null(g) || !FIG_OK) return(NULL)
    FIG_ENV$group_by_folder(g)
  })

  ## The NMDS has controls of its own: one scale (the transect), a colouring in
  ## place of the view, algae only, a species to size the points by, and the
  ## arrows and ellipses in place of the y-axis switch.
  pv_nmds <- reactive(FIG_OK && FIG_ENV$is_nmds(input$pv_analysis %||% ""))

  output$pv_scale_ui <- renderUI({
    if (!FIG_OK) return(NULL)
    choices <- if (pv_nmds()) FIG_ENV$NMDS_SCALES else FIG_SCALES
    keep <- isolate(input$pv_scale)
    tagList(tags$h3("Scale"),
            selectInput("pv_scale", NULL, width = "100%", choices = choices,
                        selected = if (!is.null(keep) && keep %in% choices) keep
                                   else if ("basin" %in% choices) "basin" else choices[[1]]))
  })

  ## Views that make a figure for this analysis and scale. A Puget Sound
  ## "across" has only one place to lay a species across, and SD bands for every
  ## species on one set of axes bury each other, so those two are not offered.
  ## For the NMDS this slot is what colours the points.
  output$pv_view_ui <- renderUI({
    if (!FIG_OK) return(NULL)
    a <- input$pv_analysis %||% "density"
    nm <- pv_nmds()
    s <- if (nm) "transect" else input$pv_scale %||% "basin"
    pool <- if (nm) FIG_ENV$NMDS_COLOR_BY else VIEWS
    ok <- pool[vapply(pool, function(v) is.null(FIG_ENV$view_problem(a, s, v)), logical(1))]
    keep <- isolate(input$pv_view)
    tagList(tags$h3(if (nm) "Color by" else "View"),
            selectInput("pv_view", NULL, width = "100%", choices = ok,
                        selected = if (!is.null(keep) && keep %in% ok) keep else ok[[1]]))
  })

  output$pv_group_ui <- renderUI({
    choices <- if (pv_nmds()) c("Algae" = "algae") else FIG_GROUPS
    keep <- isolate(input$pv_group)
    tagList(tags$h3("Data"),
            selectInput("pv_group", NULL, width = "100%", choices = choices,
                        selected = if (!is.null(keep) && keep %in% choices) keep else "algae"))
  })

  nmds_basins <- reactive({
    if (!isTRUE(fig_tables()) || is.null(FIG_ENV$nmds_points)) return(character(0))
    b <- sort(unique(as.character(FIG_ENV$nmds_points$basin)))
    setNames(b, gsub("_", " ", b))
  })

  sites_by_basin <- reactive({
    if (!isTRUE(fig_tables()) || is.null(FIG_ENV$df_site)) return(list())
    d <- unique(FIG_ENV$df_site[, c("basin", "site")])
    d <- d[order(d$basin, d$site), ]
    ## as.list() so a basin with a single site still gets its own heading --
    ## selectInput() flattens a one-element vector into a plain option
    out <- lapply(split(d$site, d$basin), function(s) as.list(setNames(s, gsub("_", " ", s))))
    setNames(out, gsub("_", " ", names(out)))
  })

  basins <- reactive({
    if (!isTRUE(fig_tables())) return(character(0))
    b <- sort(unique(as.character(FIG_ENV$df_basin$basin)))
    setNames(b, gsub("_", " ", b))
  })

  regions <- reactive({
    if (!isTRUE(fig_tables()) || is.null(FIG_ENV$df_puget)) return(c("Puget Sound" = "Puget_Sound"))
    r <- unique(as.character(FIG_ENV$df_puget$region))
    setNames(r, unname(FIG_ENV$place_label(r)))
  })

  ## What the second row asks for. Within and composite need a place: a site,
  ## a basin or a region, by scale. Across needs a species, and at the site scale
  ## a basin too -- its sites become the panels.
  output$pv_where_ui <- renderUI({
    s <- input$pv_scale %||% "basin"
    v <- input$pv_view %||% "within"
    keep <- isolate(input$pv_where)
    pick <- function(heading, choices, hint = NULL) {
      flat <- unlist(choices, use.names = FALSE)
      tagList(tags$h3(heading),
              selectInput("pv_where", NULL, width = "100%", choices = choices,
                          selected = if (!is.null(keep) && keep %in% flat) keep else NULL),
              if (!is.null(hint)) div(class = "rc-hint", hint))
    }
    if (pv_nmds()) {
      if (!identical(v, "site")) return(NULL)
      return(pick("Basin", nmds_basins(), "its sites in color, the rest in gray"))
    }
    if (identical(v, "across")) {
      if (identical(s, "site")) return(pick("Basin", basins(), "its sites become the panels"))
      return(NULL)
    }
    switch(s,
           site  = pick("Site", sites_by_basin()),
           basin = pick("Basin", basins()),
           puget = pick("Region", regions()))
  })

  output$pv_cat_ui <- renderUI({
    if (pv_nmds()) {
      if (!isTRUE(fig_tables())) return(NULL)
      ch <- c("None" = "none", FIG_ENV$nmds_size_choices())
      keep <- isolate(input$pv_cat)
      return(tagList(tags$h3("Size by"),
                     selectInput("pv_cat", NULL, width = "100%", choices = ch,
                                 selected = if (!is.null(keep) && keep %in% ch) keep else "none"),
                     div(class = "rc-hint", "point size: that species' density on the transect")))
    }
    if (!identical(input$pv_view, "across")) return(NULL)
    grp <- pv_group_def()
    if (is.null(grp) || !isTRUE(fig_tables())) return(NULL)
    cols <- FIG_ENV$group_cols(grp)
    keep <- isolate(input$pv_cat)
    tagList(tags$h3("Species"),
            selectInput("pv_cat", NULL, width = "100%",
                        choices = setNames(cols, vapply(cols, FIG_ENV$species_label, character(1))),
                        selected = if (!is.null(keep) && keep %in% cols) keep else NULL),
            div(class = "rc-hint", "one panel per place"))
  })

  ## Float gives every panel the range its own values need; lock puts every
  ## panel on one shared range, so panels can be compared by eye -- at the cost
  ## of flattening the small ones. A single-panel composite has nothing to lock.
  ## For the NMDS: how well two dimensions hold the transects' dissimilarities
  output$pv_info_ui <- renderUI({
    if (!pv_nmds() || !isTRUE(fig_tables()) || is.null(FIG_ENV$nmds_stress)) return(NULL)
    st <- FIG_ENV$nmds_stress
    tagList(tags$h3("Stress"),
            div(style = "font-size:13px;color:#333C47;margin-top:3px;",
                sprintf("2-D %.3f  \u00b7  3-D %.3f", st$stress[st$k == 2], st$stress[st$k == 3])),
            div(class = "rc-hint",
                sprintf("%s transects \u00b7 under 0.2 is usable (Clarke 1993)",
                        format(st$n_transects[1], big.mark = ","))))
  })

  ## Float gives every panel the range its own values need; lock puts every
  ## panel on one shared range, so panels can be compared by eye -- at the cost
  ## of flattening the small ones. A single-panel composite has nothing to lock.
  ## The NMDS's switches go here instead: its species arrows and group ellipses.
  ## (Two checkboxes rather than one group, so that unticking both is not the
  ## same as "not drawn yet".)
  output$pv_ylock_ui <- renderUI({
    if (pv_nmds()) {
      return(tagList(tags$h3("Show"),
                     checkboxInput("pv_arrows", "Species arrows",
                                   value = isTRUE(isolate(input$pv_arrows %||% TRUE))),
                     checkboxInput("pv_ellipses", "95% ellipses",
                                   value = isTRUE(isolate(input$pv_ellipses %||% TRUE)))))
    }
    if (identical(input$pv_view, "composite")) return(NULL)
    tagList(tags$h3("Y-axis"),
            input_switch("pv_ylock", "Lock y-axis across panels",
                         value = isTRUE(isolate(input$pv_ylock))),
            div(class = "rc-hint", "off: each panel floats to its own range"))
  })

  pv_request <- reactive({
    v <- input$pv_view
    s <- input$pv_scale
    if (pv_nmds()) {
      size <- input$pv_cat
      return(list(analysis = input$pv_analysis, scale = "transect", view = v,
                  grp = FIG_ENV$group_by_folder("algae"),
                  where = if (identical(v, "site")) input$pv_where else NULL,
                  category = if (is.null(size) || identical(size, "none")) NULL else size,
                  arrows = !isFALSE(input$pv_arrows),
                  ellipses = !isFALSE(input$pv_ellipses)))
    }
    list(analysis = input$pv_analysis, scale = s, view = v, grp = pv_group_def(),
         where = if (identical(v, "across") && !identical(s, "site")) NULL else input$pv_where,
         category = if (identical(v, "across")) input$pv_cat else NULL,
         y_lock = isTRUE(input$pv_ylock))
  })

  ## What `where` may be for a scale and view. Switching the scale re-renders the
  ## second row, but for a moment input$pv_where still holds the old scale's
  ## choice; checking it against this keeps that moment from flashing a "nothing
  ## recorded" message.
  valid_where <- function(scale, view) {
    if (identical(scale, "transect")) return(if (identical(view, "site")) unname(nmds_basins()) else NULL)
    if (identical(view, "across")) return(if (identical(scale, "site")) unname(basins()) else NULL)
    switch(scale,
           site  = unlist(sites_by_basin(), use.names = FALSE),
           basin = unname(basins()),
           puget = unname(regions()))
  }

  pv_figure <- reactive({
    if (!isTRUE(fig_tables())) return(NULL)
    r <- pv_request()
    if (is.null(r$analysis) || is.null(r$scale) || is.null(r$view) || is.null(r$grp)) return(NULL)
    if (!is.null(FIG_ENV$view_problem(r$analysis, r$scale, r$view))) return(NULL)
    if (FIG_ENV$is_nmds(r$analysis)) {
      if (identical(r$view, "site") && (is.null(r$where) || !r$where %in% valid_where("transect", "site")))
        return(NULL)
      if (!is.null(r$category) && !r$category %in% FIG_ENV$nmds_size_choices()) return(NULL)
      return(tryCatch(do.call(FIG_ENV$make_figure, r),
                      error = function(e) list(ok = FALSE, reason = conditionMessage(e))))
    }
    needs_where <- !identical(r$view, "across") || identical(r$scale, "site")
    if (needs_where && (is.null(r$where) || !r$where %in% valid_where(r$scale, r$view)))
      return(NULL)
    if (identical(r$view, "across") &&
        (is.null(r$category) || !r$category %in% FIG_ENV$group_cols(r$grp))) return(NULL)
    tryCatch(do.call(FIG_ENV$make_figure, r),
             error = function(e) list(ok = FALSE, reason = conditionMessage(e)))
  })

  output$pv_status <- renderUI({
    if (!FIG_OK)
      return(div(class = "rc-pv-msg warn",
                 "code/figure_functions.R could not be loaded, so previews are unavailable."))
    if (!isTRUE(fig_tables()))
      return(div(class = "rc-pv-msg muted",
                 "Run step 2 first -- the preview reads the tables it writes."))
    res <- pv_figure()
    if (!is.null(res) && !isTRUE(res$ok))
      return(div(class = "rc-pv-msg warn",
                 paste0("No figure for this combination \u2014 ", res$reason, ".")))
    NULL
  })

  ## The preview is the saved file: the figure is drawn on the same 12 x 7 in
  ## canvas Save writes, at whatever resolution fills the panel, and shown
  ## scaled to fit. Drawing it straight into the panel instead -- as renderPlot()
  ## does -- lays it out on a canvas as wide as the window, so in a narrow window
  ## the facet titles clipped and the caption fell off the bottom, none of which
  ## would happen in the file.
  output$pv_plot <- renderImage({
    res <- pv_figure()
    validate(need(isTRUE(res$ok), ""))
    px <- session$clientData$output_pv_plot_width %||% 1000
    ratio <- session$clientData$pixelratio %||% 1
    dpi <- max(60, min(300, round(px * ratio / FIG_ENV$fig_width)))
    f <- tempfile(fileext = ".png")
    ggplot2::ggsave(f, plot = res$plot, width = FIG_ENV$fig_width,
                    height = FIG_ENV$fig_height, dpi = dpi, bg = "white")
    list(src = f, contentType = "image/png", width = "100%",
         alt = "Figure preview")
  }, deleteFile = TRUE)

  ## ---- where saving would put it ----

  pv_target <- reactive({
    res <- pv_figure()
    if (!isTRUE(res$ok)) return(NULL)
    r <- pv_request()
    file.path(current_path("figs"),
              FIG_ENV$figure_file(r$analysis, r$scale, r$view, r$grp, r$where, r$category))
  })

  output$pv_target <- renderUI({
    t <- pv_target()
    if (is.null(t)) return(NULL)
    span(class = "rc-pv-target",
         sub(paste0("^", REPO, "/?"), "", t, fixed = FALSE))
  })

  save_preview <- function(path) {
    res <- pv_figure()
    if (!isTRUE(res$ok)) {
      showNotification("There is no figure to save yet.", type = "warning")
      return(invisible(FALSE))
    }
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    ok <- tryCatch({
      ggplot2::ggsave(path, plot = res$plot, width = FIG_ENV$fig_width,
                      height = FIG_ENV$fig_height, dpi = FIG_ENV$fig_dpi,
                      bg = "white")
      TRUE
    }, error = function(e) {
      showNotification(paste("Could not save:", conditionMessage(e)),
                       type = "error", duration = 10)
      FALSE
    })
    if (ok) showNotification(paste("Saved", basename(path)), duration = 5)
    invisible(ok)
  }

  observeEvent(input$pv_save, {
    t <- pv_target()
    if (!is.null(t)) save_preview(t)
  }, ignoreInit = TRUE)

  observeEvent(input$pv_save_as, {
    t <- pv_target()
    chosen <- pick_save("Save this figure as", t %||% file.path(REPO, "figure.png"),
                        ext = ".png", types = "{{PNG images} {.png}} {{All files} {*}}")
    if (!is.null(chosen)) save_preview(chosen)
  }, ignoreInit = TRUE)

  ## Registered here rather than beside each definition: outputOptions() needs
  ## the output to exist already, so this has to follow all of them.
  keep_live("pv_scale_ui", "pv_view_ui", "pv_group_ui", "pv_where_ui", "pv_cat_ui",
            "pv_info_ui", "pv_ylock_ui", "pv_status", "pv_target")

  ## ---- running -----------------------------------------------------------

  say <- function(...) rv$log <- c(rv$log, ...)

  ## Every script gets every path. Each reads only the variables it knows about,
  ## so one environment serves all of them and nothing has to be kept in step.
  run_env <- function(step_id) {
    vals <- vapply(names(FILES), current_path, character(1))
    names(vals) <- vapply(FILES, function(f) f$env, character(1))

    ## Each step carries its own checkbox, so the variable is set from the one
    ## belonging to the step about to run.
    vals <- c(vals,
              UKELP_INCLUDE_BASINS = paste(include_for(step_id), collapse = ","))

    ## The figures step also takes what to draw. Empty means "everything", which
    ## is what the script does when a variable is unset -- so a deliberately
    ## empty selection is sent as a value nothing matches rather than as "".
    sel <- fig_selection()
    none <- "__none__"
    as_list <- function(x) paste(c(x, if (!length(x)) none), collapse = ",")
    vals <- c(vals,
              UKELP_FIG_ANALYSES = as_list(sel$analyses),
              UKELP_FIG_SCALES   = as_list(sel$scales),
              UKELP_FIG_VIEWS    = as_list(sel$views),
              UKELP_FIG_GROUPS   = as_list(sel$groups),
              UKELP_FIG_YLOCK    = if (sel$y_lock) "1" else "0")
    c("current", vals)
  }

  missing_inputs <- function(step) {
    keys <- step$ins
    keys[!vapply(keys, function(k) {
      p <- current_path(k)
      nzchar(p) && (if (FILES[[k]]$kind == "dir") dir.exists(p) else file.exists(p))
    }, logical(1))]
  }

  ## Each script is started as its own R process rather than sourced: each opens
  ## with rm(list=ls()) and setwd("../"), both of which would reach into this
  ## running app if it shared the session. A separate process also means Stop
  ## can actually stop it.
  start_script <- function() {
    script <- rv$scripts[1]
    rv$scripts <- rv$scripts[-1]
    say(sprintf("Running code/%s", script))
    rv$proc <- processx::process$new(rscript, script, wd = CODE,
                                     env = run_env(rv$current), stdout = "|",
                                     stderr = "2>&1")
  }

  begin <- function(step_id) {
    step <- STEPS[[step_id]]

    if (is.null(rscript)) {
      rv$state <- list(text = "R was not found", class = "s-err")
      rv$queue <- character(0); return(invisible(FALSE))
    }

    gaps <- missing_inputs(step)
    if (length(gaps)) {
      labels <- paste(vapply(gaps, function(k) FILES[[k]]$label, character(1)),
                      collapse = ", ")
      rv$state <- list(text = sprintf("Step %d needs: %s", step$n, labels),
                       class = "s-err")
      say(sprintf("Cannot start step %d. These are missing: %s", step$n, labels))
      rv$queue <- character(0)
      return(invisible(FALSE))
    }

    say(strrep("-", 72),
        sprintf("Step %d of 3 -- %s", step$n, step$title),
        strrep("-", 72))

    ## What was ticked, and whether this step can actually honor it.
    picked <- include_for(step_id)
    if (length(picked)) {
      ## In a "Run everything" chain step 1 rebuilds the cleaned table first, so
      ## there is nothing upstream to be missing.
      key <- if (isTRUE(rv$all_run)) NULL else include_source[[step_id]]
      if (is.null(key)) {
        say(paste0("Including: ", paste(picked, collapse = ", ")))
      } else {
        have <- include_available(step_id)
        src <- FILES[[key]]$label
        if (is.null(have)) {
          say(sprintf("Note: asked to include %s, but %s could not be read to check.",
                      paste(picked, collapse = ", "), src))
        } else {
          usable <- intersect(picked, have)
          missing <- setdiff(picked, have)
          if (length(usable)) say(paste0("Including: ", paste(usable, collapse = ", ")))
          if (length(missing)) {
            say(sprintf("Note: %s not available -- %s does not hold %s.",
                        paste(missing, collapse = ", "), src,
                        if (length(missing) == 1) "it" else "them"),
                "      They were left out when the cleaned table was built, so this run cannot put them back.",
                "      To include them: tick them on step 1, re-run it, then re-run this step.")
          }
        }
      }
    }

    rv$state <- list(text = sprintf("Step %d of 3: %s\u2026", step$n, step$title),
                     class = "s-busy")
    rv$current <- step_id
    rv$scripts <- step$scripts
    start_script()
    invisible(TRUE)
  }

  start_chain <- function(ids, all_run = FALSE) {
    if (!is.null(rv$proc)) return()
    rv$log <- character(0)
    rv$all_run <- all_run
    rv$queue <- ids
    nxt <- rv$queue[1]
    rv$queue <- rv$queue[-1]
    begin(nxt)
  }

  lapply(names(STEPS), function(id) {
    observeEvent(input[[paste0("run_", id)]], start_chain(id), ignoreInit = TRUE)
  })

  observeEvent(input$run_all, start_chain(names(STEPS), all_run = TRUE),
               ignoreInit = TRUE)

  observeEvent(input$stop, {
    p <- rv$proc
    if (!is.null(p) && p$is_alive()) {
      rv$queue <- character(0)
      rv$scripts <- character(0)
      rv$state <- list(text = "Stopped before it finished", class = "s-warn")
      p$kill()
    }
  }, ignoreInit = TRUE)

  output$stop_ui <- renderUI({
    if (is.null(rv$proc)) NULL else actionButton("stop", "Stop", class = "btn-stop")
  })

  ## Drain the running process on a timer. processx buffers for us, so this only
  ## moves whatever has arrived since the last tick, then decides what happens
  ## when the process ends: the step's next script, the chain's next step, or
  ## the end.
  observe({
    p <- rv$proc
    if (is.null(p)) return()
    invalidateLater(250, session)

    isolate({
      new <- tryCatch(p$read_output_lines(), error = function(e) character(0))
      if (length(new)) say(new)
      if (p$is_alive()) return()

      ## read_all, not read: the process has ended, but what it wrote last can
      ## still be in the pipe, and a single non-blocking read can come back
      ## without it -- the script's final lines then never reach the log
      rest <- tryCatch(p$read_all_output_lines(), error = function(e) character(0))
      if (length(rest)) say(rest)
      code <- tryCatch(p$get_exit_status(), error = function(e) NA_integer_)

      stopped <- identical(rv$state$class, "s-warn")
      rv$proc <- NULL
      done <- STEPS[[rv$current]]

      if (stopped) {
        say("Stopped. Anything already written was left as it is.")
        rv$queue <- character(0)
        rv$current <- NULL
        rv$tables_version <- rv$tables_version + 1L
      } else if (identical(code, 0L) && length(rv$scripts)) {
        start_script()
      } else if (identical(code, 0L)) {
        say(sprintf("Step %d finished successfully.", done$n))
        rv$current <- NULL
        rv$tables_version <- rv$tables_version + 1L
        if (length(rv$queue)) {
          nxt <- rv$queue[1]; rv$queue <- rv$queue[-1]
          begin(nxt)
        } else {
          rv$state <- list(text = "Finished successfully", class = "s-ok")
        }
      } else {
        rv$current <- NULL
        rv$scripts <- character(0)
        rv$state <- list(
          text = sprintf("Step %d failed (R exited with code %s)", done$n, code),
          class = "s-err")
        say(sprintf(
          "Step %d failed. R exited with code %s. The message above says why; a 'Permission denied' usually means the file is open in Excel.",
          done$n, code))
        if (length(rv$queue)) {
          say("Stopping here -- the steps after this one would have run on stale data.")
          rv$queue <- character(0)
        }
      }
    })
  })

  output$status <- renderUI(span(class = paste("rc-status", rv$state$class),
                                 rv$state$text))

  output$log <- renderUI({
    if (!length(rv$log)) return(HTML("Nothing has run yet."))
    HTML(paste(htmltools::htmlEscape(rv$log), collapse = "\n"))
  })
}

shinyApp(ui, server)
