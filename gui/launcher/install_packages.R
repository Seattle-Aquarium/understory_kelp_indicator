## Installs the R packages the Data Interpreter needs into the private library
## run_data_interpreter.bat set up, from one fixed day's CRAN snapshot.
##
##   Rscript --vanilla install_packages.R <library folder> <repository URL>
##
## Windows binaries only: nothing is compiled, so no Rtools. The snapshot URL
## pins every version, so two computers set up a month apart get the same
## packages.

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) stop("usage: install_packages.R <library> <repository URL>")
lib  <- args[1]
repo <- args[2]

dir.create(lib, recursive = TRUE, showWarnings = FALSE)
## this library and R's own base packages, and nothing else on the computer
.libPaths(c(lib, .Library))
options(repos = c(CRAN = repo), timeout = 900,
        install.packages.compile.from.source = "never")

## What the app and the step scripts load; install.packages() brings in what
## these depend on. Change this list and bump LIBRARY_REV in runtime.cfg.
need <- c("shiny", "bslib", "processx",          # the app
          "dplyr", "tidyr", "tibble", "ggplot2",  # the step scripts
          "vegan", "ggrepel",                     # the NMDS and its labels
          "glmmTMB", "patchwork")                 # the GLMMs and their two-panel figures

installed <- function() rownames(installed.packages(lib.loc = lib))

missing <- setdiff(need, installed())
if (length(missing)) {
  cat("  Installing", paste(missing, collapse = ", "), "and the packages they use.\n")
  cat("  From", repo, "\n\n")
  install.packages(missing, lib = lib, type = "binary", quiet = FALSE)
}

## Installed is not the same as working: each one has to load
loads <- vapply(need, function(p) {
  ok <- tryCatch({ loadNamespace(p, lib.loc = lib); TRUE }, error = function(e) {
    cat("  ", p, ": ", conditionMessage(e), "\n", sep = "")
    FALSE
  })
  ok
}, logical(1))
if (!all(loads)) {
  cat("\n  These did not install properly:", paste(need[!loads], collapse = ", "), "\n")
  quit(status = 1)
}

versions <- vapply(need, function(p) as.character(packageVersion(p, lib.loc = lib)), "")
writeLines(c(format(Sys.time(), "%Y-%m-%d %H:%M"), R.version.string, repo,
             paste(need, versions)),
           file.path(lib, ".ukelp_complete"))
cat(sprintf("\n  Packages ready: %d in %s\n", length(installed()), lib))
