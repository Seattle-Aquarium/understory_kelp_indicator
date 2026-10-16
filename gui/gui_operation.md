# Data Interpreter GUI

## Purpose

Data Interpreter is a one stop shop for manipulating and visualizing Reef Check's Washington State dataset. The R Shiny app combines the capabilities of R scripts located in this repo's `code` directory, and cleanly guides users through each step in a user interface. With surveys conducted annually for more than 50 sites in Washington waters alone, Data Interpreter will rapidly deliver the latest updates and trends on the health of our understory kelp forests.

<p align="center">
  <img width="66%" alt="Understory Kelp Indicator: Data Interpreter GUI Figures tab" src="understory_kelp_indicator/gui/uki_datainterp.png"/>
</p>

## Operation

1. You may initialize Data Interpreter via `understory_kelp_indicator/run_data_interpreter.bat` to open in browser, or run `understory_kelp_indicator/gui/app.R` in RStudio.
2. **Clean Data** - Generates reformatted csv files from raw dataset for later analysis.
- The default open page for Data Interpreter assumes no data has been processed. If the users has cloned the `understory_kelp_indicator` repo, the GUI will auto-populate each data category with the current raw data file for each category available on GitHub. Should the user wish to update the data source, click "Browse..." and locate within file explorer.
- The "Withheld Basins" section contains checkboxes to include data from the Northern Coast, Western Strait of Juan de Fuca, and Eastern Strait of Juan de Fuca basins, which are otherwise excluded.
- The default save location for outputs is `understory_kelp_indicator\results`; however, users may again select "Browse..." to change save location.
- Select "Run this step" to execute with your selections.
- "Reset all locations" restores all file and save paths to default.
3. **Build Metrics** - Generates csv files containing standardized average data from the site scale to Washington-wide.
- Note that this step is dependent on outputs from Clean Data; therefore, `understory_kelp_indicator/results/reef_check_cleaned.csv` is set as default.
4. **Figures** - Generates several varieties of figures from cleaned data and metrics csv files (longest run time).
- This tab begins with "Preview a figure", which helps users explore trends and basic analyses before exporting.
  - The user begins by selecting from the "Scale" dropdown menu. Alternatvely, selecting "Invert to Category" allows users to inspect data across scale for a target category, and "Percentage" will generate individual percent deviation from average graphs.
  - Further dropdowns help narrow the focus until the intended visualization is produced.
  - "Save this figure" allows the user to save the png file as it is presented on screen. Note that this method produces lower-quality images than properly executing the script--it is designed for quick access. "Save as..." allows the user to save the png file to a specified location.
- Previewing outputs helps inform selections for the "What to draw" panel. Here, users choose which figures to construct in a higher-quality.
  - Use the checkboxes to plan figure generation.
  - The box at the bottom of the panel shows which figures will be generated, how many there are in the set, and where in the `understory_kelp_indicator/figs` directory they can be found.
5. **Run Everything** - Executes all three steps in Data Interpreter from data cleaning through visualizations.