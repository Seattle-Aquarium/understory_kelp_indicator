@echo off
REM ---------------------------------------------------------------------------
REM  Understory Kelp Indicator - launcher
REM
REM  Double-click this file. It starts the application and opens it in your
REM  browser. The window it runs in has to stay open while you use it; close
REM  that window when you are finished.
REM
REM  You need R installed. On the first run it installs the three R packages the
REM  front end needs, which takes a minute or two and needs the internet.
REM ---------------------------------------------------------------------------
setlocal EnableExtensions
cd /d "%~dp0"

REM ---- 1. find R ---------------------------------------------------------
set "RS="
for /f "delims=" %%P in ('where Rscript 2^>nul') do if not defined RS set "RS=%%~P"

if not defined RS (
  for /f "delims=" %%D in ('dir /b /ad /o-n "%ProgramFiles%\R" 2^>nul') do (
    if not defined RS if exist "%ProgramFiles%\R\%%D\bin\x64\Rscript.exe" (
      set "RS=%ProgramFiles%\R\%%D\bin\x64\Rscript.exe"
    )
  )
)

if not defined RS (
  echo.
  echo   R was not found.
  echo.
  echo   Install R from https://cran.r-project.org/bin/windows/base/
  echo   and then run this again.
  echo.
  pause
  exit /b 1
)

echo   Using %RS%

REM ---- 2. the packages the front end needs -------------------------------
REM  The analysis itself needs tidyverse, which the scripts load themselves.
"%RS%" -e "p <- c('shiny','bslib','processx'); m <- p[!p %%in%% rownames(installed.packages())]; if (length(m)) { cat('Installing:', paste(m, collapse=', '), '\n'); install.packages(m, repos='https://cloud.r-project.org') } else cat('Front-end packages are ready.\n')"
if errorlevel 1 (
  echo.
  echo   Could not install what the front end needs. Are you online?
  pause
  exit /b 1
)

REM ---- 3. start ----------------------------------------------------------
REM  The app stops itself a few seconds after the last browser tab closes (see
REM  the session tally in gui/app.R), so Rscript returns and this window closes
REM  on its own. No pause here -- a pause would hold the window open waiting for
REM  a keypress, which is the very thing we are trying to avoid.
echo.
echo   Starting. Your browser will open in a moment.
echo   Close the browser tab when you are finished and this window will close
echo   itself a few seconds later.
echo.
"%RS%" -e "shiny::runApp('gui', launch.browser = TRUE)"

REM  Anything other than a clean exit is worth reading, so hold the window open
REM  only in that case.
if errorlevel 1 (
  echo.
  echo   The application stopped with an error. The message above says why.
  pause
)
exit /b 0
