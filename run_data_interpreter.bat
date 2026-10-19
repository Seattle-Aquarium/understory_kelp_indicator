@echo off
REM ---------------------------------------------------------------------------
REM  Understory Kelp Indicator: Data Interpreter - launcher
REM
REM  Double-click this file. It opens the Data Interpreter in your browser. This
REM  window stays open while the app runs and closes itself a few seconds after
REM  you close the browser tab.
REM
REM  Nothing has to be installed first -- not even R. The first time, it
REM  downloads its own copy of R and the R packages the app needs (about 250 MB,
REM  a few minutes, internet needed once) into
REM      %LOCALAPPDATA%\CCR\understory_kelp_indicator
REM  and from then on starts straight away, using only that. Other R versions,
REM  package libraries, .Renviron and .Rprofile files on the computer are never
REM  read, so they cannot break it -- and it cannot change them.
REM
REM  Which R and which packages are pinned in gui\launcher\runtime.cfg.
REM
REM  Switches:
REM    --repair   rebuild the private environment from scratch
REM ---------------------------------------------------------------------------
setlocal EnableExtensions
cd /d "%~dp0"
title Understory Kelp Indicator: Data Interpreter

set "REPAIR="
for %%A in (%*) do if /i "%%~A"=="--repair" set "REPAIR=1"

REM ---- 1. which environment ------------------------------------------------
set "UKI_R_VERSION="
for /f "usebackq eol=# tokens=1,* delims==" %%A in ("gui\launcher\runtime.cfg") do set "UKI_%%A=%%B"
if not defined UKI_R_VERSION (
  echo.
  echo   gui\launcher\runtime.cfg is missing or unreadable. This file has to stay
  echo   in the understory_kelp_indicator folder, beside the gui folder.
  echo.
  pause
  exit /b 1
)

set "UKI_HOME=%LOCALAPPDATA%\CCR\understory_kelp_indicator"
set "UKI_LIB=%UKI_HOME%\library\R-%UKI_R_VERSION%_%UKI_CRAN_SNAPSHOT%_rev%UKI_LIBRARY_REV%"
set "UKI_RUNTIME=%UKI_HOME%\runtime-%UKI_R_VERSION%.cmd"

REM ---- 2. set it up if it is not there yet ------------------------------------
if defined REPAIR goto setup
if not exist "%UKI_RUNTIME%" goto setup
if not exist "%UKI_LIB%\.ukelp_complete" goto setup
call "%UKI_RUNTIME%"
if not exist "%UKI_RSCRIPT%" goto setup
goto run

:setup
set "UKI_REPAIR_FLAG="
if defined REPAIR set "UKI_REPAIR_FLAG=-Repair"
powershell -NoProfile -ExecutionPolicy Bypass -File "%~dp0gui\launcher\setup_runtime.ps1" -RepoDir "%~dp0." -UkiHome "%UKI_HOME%" -Lib "%UKI_LIB%" -RVersion "%UKI_R_VERSION%" -RSha256 "%UKI_R_SHA256%" -Snapshot "%UKI_CRAN_SNAPSHOT%" -Runtime "%UKI_RUNTIME%" %UKI_REPAIR_FLAG%
if errorlevel 1 (
  echo.
  echo   The Data Interpreter could not be set up. The message above says why;
  echo   the most common cause is no internet connection on the first run.
  echo.
  pause
  exit /b 1
)
call "%UKI_RUNTIME%"

REM ---- 3. run, sealed off from everything else on the computer ----------------
REM  The libraries are this environment's only. R_ENVIRON_USER and R_PROFILE_USER
REM  point at empty files, so nobody's ~/.Renviron or ~/.Rprofile is read -- by
REM  this R or by the step scripts the app starts, which inherit all of this.
REM  A .Renviron that set R_LIBS_USER to another version's library is exactly
REM  what made the old launcher crash with "entry point not found".
:run
set "R_HOME="
set "R_ENVIRON="
set "R_PROFILE="
set "R_LIBS=%UKI_LIB%"
set "R_LIBS_USER=%UKI_LIB%"
set "R_LIBS_SITE=%UKI_LIB%"
set "R_ENVIRON_USER=%UKI_HOME%\Renviron"
set "R_PROFILE_USER=%UKI_HOME%\Rprofile"

echo.
echo   Understory Kelp Indicator: Data Interpreter
echo.
"%UKI_RSCRIPT%" --no-environ --no-site-file --no-init-file --no-restore "%~dp0gui\launcher\launch.R" "%~dp0."

REM  Anything other than a clean exit is worth reading, so hold the window open
REM  only in that case.
if errorlevel 1 (
  echo.
  echo   The application stopped with an error. The message above says why.
  echo   If it mentions packages, run:  run_data_interpreter.bat --repair
  echo.
  pause
  exit /b 1
)
exit /b 0
