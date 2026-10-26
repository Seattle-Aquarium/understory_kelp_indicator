## The Understory Kelp Indicator's Data Explorer, as a website.
##
## This is the file Posit Connect Cloud starts: the Data Interpreter's Explore
## data card on its own, drawing every figure from the tables in results/.
## It runs gui/app.R with the explore-only switch on; nothing else differs, so
## the website and the desktop app draw the same figures.
##
## The desktop app -- all three steps, Run, saving to figs/ -- is gui/app.R,
## started with run_data_interpreter.bat. To see the website version on your
## own computer: shiny::runApp() from the repository folder.
##
## manifest.json beside this file lists the packages the website installs;
## regenerate it whenever that list changes (see INVARIANTS.md, R-6).

options(ukelp.explore_only = TRUE)
source(file.path("gui", "app.R"), local = TRUE)$value
