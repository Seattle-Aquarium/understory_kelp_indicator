@echo off
REM ---------------------------------------------------------------------------
REM  Puts "Understory Kelp Indicator" on this user's desktop.
REM
REM  Double-click it once. Run it again whenever the repository is moved or
REM  renamed -- it overwrites the shortcut it made last time rather than adding
REM  a second one. It needs no administrator rights and changes nothing but
REM  the desktop.
REM
REM  The same care as the ROV Flight Operations shortcut:
REM    * the repository is found from this file's own location (%~dp0), so a
REM      copy anywhere -- Dropbox paths with spaces included -- points at itself;
REM    * the desktop is asked for rather than assumed, because OneDrive often
REM      takes it over and "%USERPROFILE%\Desktop" is then a folder nobody sees;
REM    * the paths cross into PowerShell in environment variables, never pasted
REM      into the command text, so a quote in a path cannot break it.
REM ---------------------------------------------------------------------------
setlocal EnableExtensions

set "HERE=%~dp0"
if "%HERE:~-1%"=="\" set "HERE=%HERE:~0,-1%"

set "UKI_TARGET=%HERE%\run_data_interpreter.bat"
set "UKI_ICON=%HERE%\gui\launcher\data_interpreter.ico"
set "UKI_DIR=%HERE%"
set "UKI_NAME=Understory Kelp Indicator"

if not exist "%UKI_TARGET%" (
  echo.
  echo   Could not find run_data_interpreter.bat beside this file.
  echo   This file has to stay in the understory_kelp_indicator folder.
  echo.
  pause
  exit /b 1
)

powershell -NoProfile -ExecutionPolicy Bypass -Command ^
  "$ErrorActionPreference = 'Stop';" ^
  "$desktop = [Environment]::GetFolderPath('Desktop');" ^
  "if ([string]::IsNullOrWhiteSpace($desktop)) { $desktop = Join-Path $env:USERPROFILE 'Desktop' };" ^
  "$link = Join-Path $desktop ($env:UKI_NAME + '.lnk');" ^
  "$s = (New-Object -ComObject WScript.Shell).CreateShortcut($link);" ^
  "$s.TargetPath = $env:UKI_TARGET;" ^
  "$s.WorkingDirectory = $env:UKI_DIR;" ^
  "$s.Description = 'Understory Kelp Indicator: Data Interpreter';" ^
  "if (Test-Path -LiteralPath $env:UKI_ICON) { $s.IconLocation = $env:UKI_ICON + ',0' };" ^
  "$s.Save();" ^
  "Write-Host ('  shortcut: ' + $link)"

if errorlevel 1 (
  echo.
  echo   The shortcut could not be created. The message above says why.
  echo.
  pause
  exit /b 1
)

echo.
echo   Done. The first start sets up the app's own copy of R, which takes a few
echo   minutes and needs the internet; after that it opens straight away.
echo.
pause
exit /b 0
