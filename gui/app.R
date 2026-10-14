## Understory Kelp Indicator -- desktop application
##
## A front end to the repository's R scripts. It does not reimplement any step:
## it collects the file locations a step needs, starts that script as its own R
## process, and shows what comes back as it happens.
##
##   1. Clean data      code/rc_full_data_clean.R
##   2. Build metrics   code/metrics_across_scale.R
##   3. Figures         code/visualizations.R
##   Run everything     all three in order, stopping at the first failure
##
## Run it with run_understory_kelp_gui.bat, or from R:
##   shiny::runApp("gui")
##
## Design follows ReefCheck_BasicLogoGuide.pdf. That guide fixes two colors --
## #003A70 and #00C4B3 -- and two licensed typefaces, and says nothing about
## surfaces or states, so everything beyond the two colors is derived here and
## the derivation is written down rather than left to taste.

library(shiny)
library(bslib)

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

## The app directory is gui/; the repository is its parent.
REPO    <- normalizePath("..", winslash = "/", mustWork = FALSE)
CODE    <- file.path(REPO, "code")
DATA    <- file.path(REPO, "data", "Reef_Check")
RESULTS <- file.path(REPO, "results")
FIGS    <- file.path(REPO, "figs")

## Every file location the three scripts between them care about. One registry
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
  site    = list(label = "Site scale",   env = "UKELP_SITE_CSV",    kind = "save",
                 default = file.path(RESULTS, "metrics_site_scale.csv")),
  basin   = list(label = "Basin scale",  env = "UKELP_BASIN_CSV",   kind = "save",
                 default = file.path(RESULTS, "metrics_basin_scale.csv")),
  puget   = list(label = "Puget Sound-wide", env = "UKELP_PUGET_CSV", kind = "save",
                 default = file.path(RESULTS, "metrics_puget_sound_scale.csv")),
  figs    = list(label = "Figures folder", env = "UKELP_FIGS_DIR",  kind = "dir",
                 default = FIGS)
)

STEPS <- list(
  list(id = "clean", n = 1, title = "Clean data",
       script = "rc_full_data_clean.R",
       blurb = paste("Reads the four raw Reef Check exports, drops Canadian sites",
                     "and the excluded basins, corrects known site-name",
                     "misspellings, removes the duplicate winter 2025 surveys,",
                     "and assigns one unique key per dive."),
       ins = c("algae", "upc", "invert", "fish"),
       outs = c("cleaned")),
  list(id = "metrics", n = 2, title = "Build metrics",
       script = "metrics_across_scale.R",
       blurb = paste("Aggregates the combined table into site, basin and Puget",
                     "Sound-wide averages, each with its standard deviation and",
                     "variance."),
       ins = c("cleaned"),
       outs = c("site", "basin", "puget")),
  list(id = "figures", n = 3, title = "Figures",
       script = "visualizations.R",
       blurb = paste("Draws every figure set for algae and the four UPC groups,",
                     "into the figures folder by group. This is the slowest step",
                     "-- it writes a few hundred files."),
       ins = c("site", "basin", "puget"),
       outs = c("figs"))
)
names(STEPS) <- vapply(STEPS, function(s) s$id, character(1))

## ---------------------------------------------------------------------------
## What the figures step can draw
## ---------------------------------------------------------------------------
##
## Three choices -- scale, data type, figure type -- and one set of figures for
## each valid (scale, type) pair. The table below is the whole mapping; change a
## folder here and the summary, the counts and the script all follow.
##
## "composite" exists only at the basin scale: there is no site or sound-wide
## equivalent of the one-axis layout, so asking for it alone draws nothing.

FIG_GROUPS <- c("Algae"             = "algae",
                "Cover (cv_)"       = "cover",
                "Relief (rf_)"      = "relief",
                "Substrate (sb_)"   = "substrate",
                "Superlayer (sp_)"  = "superlayer")

FIG_SCALES <- c("Site"             = "site",
                "Basin"            = "basin",
                "Puget Sound-wide" = "puget")

FIG_TYPES <- c("Density / proportion"                        = "plain",
               "Density / proportion with standard deviation" = "sd",
               "Basin and species composites"                 = "composite")

#: `per` is what one file covers, which is how the file counts are worked out:
#:   basin     -> one file per basin
#:   category  -> one file per category in the group
#:   one       -> a single sound-wide file
FIG_SETS <- list(
  list(scale = "site",  type = "plain",     folder = "species_composites",
       per = "category", about = "one file per category, faceted by basin"),
  list(scale = "site",  type = "sd",        folder = "species_composites_sd",
       per = "category", about = "the same, with a standard deviation band"),
  list(scale = "basin", type = "plain",     folder = "basin_composites",
       per = "basin",    about = "one file per basin, faceted by category"),
  list(scale = "basin", type = "sd",        folder = "basin_composites_sd",
       per = "basin",    about = "the same, with a standard deviation band"),
  list(scale = "basin", type = "composite", folder = "basin_species",
       per = "basin",    about = "one file per basin, every category on one axis"),
  list(scale = "puget", type = "plain",     folder = "puget_sound",
       per = "one",      about = "one sound-wide file, faceted by category"),
  list(scale = "puget", type = "sd",        folder = "puget_sound_sd",
       per = "one",      about = "the same, with a standard deviation band")
)

#: Prefixes and the algae exclusions, so the summary can count categories from
#: the basin-scale table rather than from a number written down here.
GROUP_PREFIX <- c(algae = "ak_", cover = "cv_", relief = "rf_",
                  substrate = "sb_", superlayer = "sp_")
EXCLUDED_CATEGORIES <- c("ak_bull_kelp_avg", "ak_feather_boa_n_avg",
                         "ak_giant_kelp_n_avg")

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

pick_save <- function(caption, initial) {
  clean_path(with_tk_parent(function(tt) as.character(tcltk::tkgetSaveFile(
    parent = tt, title = caption,
    initialfile = basename(initial),
    initialdir = if (nzchar(initial)) dirname(initial) else REPO,
    defaultextension = ".csv",
    filetypes = "{{CSV files} {.csv}} {{All files} {*}}"))))
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
  head <- if (kind == "save") "exists, will be overwritten  ·  " else ""
  text <- if (is.na(rows)) size
          else sprintf("%s rows  ·  %s", format(rows, big.mark = ","), size)
  list(ok = TRUE,
       text = sprintf("%s%s  ·  %s", head, text,
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

app_css <- sprintf('
  :root { --navy:%s; --teal:%s; --teal-ink:%s; --ink:%s; --ink-muted:%s;
          --panel:%s; --line:%s; --warn:%s; --err:%s; }
  body { font-family: %s; background:#fff; }

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

  .rc-wrap { max-width:1080px; margin:0 auto; padding:20px 24px 44px 24px; }

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
  .rc-file .form-group, .rc-file .shiny-input-container { margin-bottom:0; width:100%%; }
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

  /* The three choice columns on the Figures tab */
  .rc-choices { display:grid; grid-template-columns:1fr 1fr 1.35fr; gap:26px; }
  .rc-choices h3 { font-size:12px; font-weight:700; letter-spacing:.06em;
                   text-transform:uppercase; color:var(--navy);
                   margin:0 0 8px 0; padding-bottom:5px;
                   border-bottom:2px solid var(--teal); }
  .rc-choices .shiny-options-group { margin-top:0; }
  .rc-choices .checkbox { margin:0 0 5px 0; }
  .rc-choices .checkbox label { font-size:13px; color:var(--ink); line-height:1.4; }
  @media (max-width: 820px) { .rc-choices { grid-template-columns:1fr; gap:14px; } }

  .rc-plan { background:#fff; border:1px solid var(--line); border-radius:8px;
             padding:12px 14px; margin-top:14px; font-size:12.5px; }
  .rc-plan .hd { font-weight:600; color:var(--navy); margin-bottom:6px; }
  .rc-plan table { width:100%%; border-collapse:collapse; }
  .rc-plan td { padding:3px 0; vertical-align:top; }
  .rc-plan td.f { font-family:Consolas,"Courier New",monospace; color:var(--ink);
                  white-space:nowrap; padding-right:14px; }
  .rc-plan td.d { color:var(--ink-muted); }
  .rc-plan td.c { text-align:right; color:var(--teal-ink); font-weight:600;
                  white-space:nowrap; padding-left:14px; }
  .rc-plan .tot { border-top:1px solid var(--line); font-weight:600; color:var(--navy); }
  .rc-plan .none { color:var(--warn); font-weight:600; }

  .rc-chain { font-size:13px; color:var(--ink); line-height:1.9; margin:0 0 6px 0; }
  .rc-chain .n { display:inline-block; min-width:22px; font-weight:700; color:var(--navy); }
  .rc-foot { font-size:11.5px; color:var(--ink-muted); margin-top:8px; }
', NAVY, TEAL, TEAL_INK, INK, INK_MUTED, PANEL, LINE, WARN_AMBER, ERR_RED, FONT_STACK)

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
    actionButton(paste0("browse_", key), "Browse…", class = "btn-browse")
  )
}

step_panel <- function(step) {
  nav_panel(
    title = sprintf("%d.  %s", step$n, step$title),
    value = step$id,
    div(
      class = "rc-card",
      h2(sprintf("%d.  %s", step$n, step$title)),
      p(class = "blurb", step$blurb),
      tags$div(style = "font-size:12px;color:#5F6B7A;margin:0 0 6px 0;",
               sprintf("Runs code/%s", step$script))
    ),
    if (identical(step$id, "figures")) {
      ## The three tables this step reads are step 2's outputs and are set on
      ## that tab, so repeating the pickers here would be two places to change
      ## one path. This tab chooses what to draw instead.
      div(
        class = "rc-card",
        h2("What to draw"),
        p(class = "blurb",
          paste("Every combination of the three below is one set of figures.",
                "Reads the site, basin and Puget Sound-wide tables from step 2.")),
        div(
          class = "rc-choices",
          div(
            tags$h3("Scale"),
            checkboxGroupInput("fig_scales", label = NULL, choices = FIG_SCALES,
                               selected = unname(FIG_SCALES))
          ),
          div(
            tags$h3("Data type"),
            checkboxGroupInput("fig_groups", label = NULL, choices = FIG_GROUPS,
                               selected = unname(FIG_GROUPS))
          ),
          div(
            tags$h3("Figure type"),
            checkboxGroupInput("fig_types", label = NULL, choices = FIG_TYPES,
                               selected = unname(FIG_TYPES))
          )
        ),
        div(style = "margin-top:4px;", uiOutput("fig_plan"))
      )
    } else {
      div(
        class = "rc-card",
        h2("Reads"),
        lapply(step$ins, file_row)
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

ui <- page_fluid(
  theme = rc_theme,
  tags$head(tags$style(HTML(app_css)), tags$title("Understory Kelp Indicator")),

  div(class = "rc-banner",
      tags$img(src = "logo.png", alt = "Reef Check Worldwide"),
      div(h1("Understory Kelp Indicator", class = "rc-title"),
          p("Reef Check Worldwide  ·  Seattle Aquarium", class = "rc-sub"))),
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
                    span(style = "color:#5F6B7A;", paste0("  —  code/", s$script))))),
          div(style = "margin-top:14px;",
              actionButton("run_all", "Run all three steps", class = "btn-all")),
          div(class = "rc-foot",
              paste("File locations come from the three step tabs. The figures",
                    "step writes a few hundred files and takes the longest."))
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

server <- function(input, output, session) {

  rscript <- rscript_path()

  rv <- reactiveValues(
    proc = NULL,                 # processx handle for the running step
    current = NULL,              # which step id is running
    queue = character(0),        # step ids still to run, for "Run everything"
    log = character(0),
    state = list(text = "Not run yet", class = "s-idle")
  )

  output$rinfo <- renderText({
    if (is.null(rscript))
      "R was not found. Install R, or start this app from an R session."
    else sprintf("Using %s  ·  %s", R.version.string, rscript)
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

  ## ---- what the figures step will draw -----------------------------------

  ## Basin count and categories-per-group come from the basin-scale table rather
  ## than from numbers written down here, so the summary stays right when the
  ## excluded basins or the species list change.
  fig_scale <- reactive({
    p <- current_path("basin")
    if (!nzchar(p) || !file.exists(p)) return(NULL)
    d <- tryCatch(read.csv(p, check.names = FALSE), error = function(e) NULL)
    if (is.null(d) || !"basin" %in% names(d)) return(NULL)
    list(
      basins = length(unique(d$basin)),
      categories = vapply(GROUP_PREFIX, function(px) {
        cols <- grep(paste0("^", px, ".*_avg$"), names(d), value = TRUE)
        length(setdiff(cols, EXCLUDED_CATEGORIES))
      }, integer(1))
    )
  })

  fig_selection <- reactive({
    list(groups = input$fig_groups %||% character(0),
         scales = input$fig_scales %||% character(0),
         types  = input$fig_types  %||% character(0))
  })

  output$fig_plan <- renderUI({
    sel <- fig_selection()
    sets <- Filter(function(s) s$scale %in% sel$scales && s$type %in% sel$types,
                   FIG_SETS)

    if (!length(sets) || !length(sel$groups)) {
      return(div(class = "rc-plan",
                 span(class = "none", "Nothing selected — this step would write no figures."),
                 div(style = "color:#5F6B7A;margin-top:4px;",
                     "Pick at least one scale, one data type and one figure type.")))
    }

    sc <- fig_scale()
    per_file <- function(s, grp) {
      if (is.null(sc)) return(NA_integer_)
      switch(s$per,
             basin = sc$basins,
             category = unname(sc$categories[[grp]]),
             one = 1L)
    }

    rows <- lapply(sets, function(s) {
      n <- if (is.null(sc)) NA_integer_
           else sum(vapply(sel$groups, function(g) per_file(s, g), integer(1)))
      tags$tr(
        tags$td(class = "f", paste0("figs/", s$folder, "/")),
        tags$td(class = "d", s$about),
        tags$td(class = "c", if (is.na(n)) "—" else format(n, big.mark = ","))
      )
    })

    total <- if (is.null(sc)) NA_integer_
             else sum(vapply(sets, function(s)
               sum(vapply(sel$groups, function(g) per_file(s, g), integer(1))),
               integer(1)))

    div(
      class = "rc-plan",
      div(class = "hd", sprintf("%d figure %s, across %d data %s",
                                length(sets),
                                if (length(sets) == 1) "set" else "sets",
                                length(sel$groups),
                                if (length(sel$groups) == 1) "type" else "types")),
      tags$table(
        tags$tbody(
          rows,
          tags$tr(class = "tot",
                  tags$td(class = "f", "total"),
                  tags$td(class = "d", if (is.null(sc))
                    "run step 2 first for a file count" else ""),
                  tags$td(class = "c",
                          if (is.na(total)) "—"
                          else paste(format(total, big.mark = ","), "files")))
        )
      )
    )
  })

  ## ---- running -----------------------------------------------------------

  say <- function(...) rv$log <- c(rv$log, ...)

  ## Every script gets every path. Each reads only the variables it knows about,
  ## so one environment serves all three and nothing has to be kept in step.
  run_env <- function() {
    vals <- vapply(names(FILES), current_path, character(1))
    names(vals) <- vapply(FILES, function(f) f$env, character(1))

    ## The figures step also takes what to draw. Empty means "everything", which
    ## is what the script does when the variable is unset -- so a deliberately
    ## empty selection is sent as a value no set matches rather than as "".
    sel <- fig_selection()
    none <- "__none__"
    vals <- c(vals,
              UKELP_FIG_GROUPS = paste(c(sel$groups, if (!length(sel$groups)) none),
                                       collapse = ","),
              UKELP_FIG_SCALES = paste(c(sel$scales, if (!length(sel$scales)) none),
                                       collapse = ","),
              UKELP_FIG_TYPES  = paste(c(sel$types,  if (!length(sel$types))  none),
                                       collapse = ","))
    c("current", vals)
  }

  missing_inputs <- function(step) {
    keys <- step$ins
    keys[!vapply(keys, function(k) {
      p <- current_path(k)
      nzchar(p) && (if (FILES[[k]]$kind == "dir") dir.exists(p) else file.exists(p))
    }, logical(1))]
  }

  ## Started as its own R process rather than sourced: each script opens with
  ## rm(list=ls()) and setwd("../"), both of which would reach into this running
  ## app if it shared the session. A separate process also means Stop can
  ## actually stop it.
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
        sprintf("Running code/%s", step$script),
        strrep("-", 72))
    rv$state <- list(text = sprintf("Step %d of 3: %s…", step$n, step$title),
                     class = "s-busy")
    rv$current <- step_id
    rv$proc <- processx::process$new(rscript, step$script, wd = CODE,
                                     env = run_env(), stdout = "|", stderr = "2>&1")
    invisible(TRUE)
  }

  start_chain <- function(ids) {
    if (!is.null(rv$proc)) return()
    rv$log <- character(0)
    rv$queue <- ids
    nxt <- rv$queue[1]
    rv$queue <- rv$queue[-1]
    begin(nxt)
  }

  lapply(names(STEPS), function(id) {
    observeEvent(input[[paste0("run_", id)]], start_chain(id), ignoreInit = TRUE)
  })

  observeEvent(input$run_all, start_chain(names(STEPS)), ignoreInit = TRUE)

  observeEvent(input$stop, {
    p <- rv$proc
    if (!is.null(p) && p$is_alive()) {
      rv$queue <- character(0)
      rv$state <- list(text = "Stopped before it finished", class = "s-warn")
      p$kill()
    }
  }, ignoreInit = TRUE)

  output$stop_ui <- renderUI({
    if (is.null(rv$proc)) NULL else actionButton("stop", "Stop", class = "btn-stop")
  })

  ## Drain the running process on a timer. processx buffers for us, so this only
  ## moves whatever has arrived since the last tick, then decides what happens
  ## when the process ends.
  observe({
    p <- rv$proc
    if (is.null(p)) return()
    invalidateLater(250, session)

    isolate({
      new <- tryCatch(p$read_output_lines(), error = function(e) character(0))
      if (length(new)) say(new)
      if (p$is_alive()) return()

      rest <- tryCatch(p$read_output_lines(), error = function(e) character(0))
      if (length(rest)) say(rest)
      code <- tryCatch(p$get_exit_status(), error = function(e) NA_integer_)

      stopped <- identical(rv$state$class, "s-warn")
      rv$proc <- NULL
      done <- STEPS[[rv$current]]
      rv$current <- NULL

      if (stopped) {
        say("Stopped. Anything already written was left as it is.")
        rv$queue <- character(0)
      } else if (identical(code, 0L)) {
        say(sprintf("Step %d finished successfully.", done$n))
        if (length(rv$queue)) {
          nxt <- rv$queue[1]; rv$queue <- rv$queue[-1]
          begin(nxt)
        } else {
          rv$state <- list(text = "Finished successfully", class = "s-ok")
        }
      } else {
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
