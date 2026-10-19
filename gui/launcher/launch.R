## Starts the Data Interpreter. run_data_interpreter.bat runs this with the
## private R it set up, that environment's own package library, and nobody's
## .Renviron or .Rprofile; the repository folder is the one argument.
##
## The app stops itself a few seconds after its last browser tab closes (see
## the session tally in gui/app.R), which ends this script and lets the
## launcher's window close.

args <- commandArgs(trailingOnly = TRUE)
repo <- normalizePath(if (length(args)) args[1] else ".", winslash = "/", mustWork = TRUE)

need <- c("shiny", "bslib", "processx", "dplyr", "tidyr", "ggplot2")
missing <- need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) {
  cat("\n  These R packages could not be loaded:", paste(missing, collapse = ", "),
      "\n  Run  run_data_interpreter.bat --repair  to rebuild the environment.\n\n")
  quit(status = 3)
}

cat(sprintf("  %s\n  packages: %s\n\n", R.version.string, .libPaths()[1]))
cat("  Your browser will open in a moment. If it does not, open the address\n",
    " printed below. Close the browser tab when you are finished and this\n",
    " window will close itself a few seconds later.\n\n", sep = "")

## UKI_PORT fixes the port and UKI_NO_BROWSER=1 skips opening a browser -- for
## running it somewhere a browser cannot be opened, and for testing the launcher
## without one popping up. Neither is set in normal use.
port <- suppressWarnings(as.integer(Sys.getenv("UKI_PORT", "")))
shiny::runApp(file.path(repo, "gui"),
              port = if (is.na(port)) getOption("shiny.port") else port,
              launch.browser = !identical(Sys.getenv("UKI_NO_BROWSER"), "1"))
