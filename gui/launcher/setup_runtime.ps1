# ---------------------------------------------------------------------------
#  Builds the Data Interpreter's private R environment. run_data_interpreter.bat
#  calls this when the environment is missing, incomplete or out of date, and
#  with -Repair when asked to rebuild it.
#
#    1. R itself -- the version runtime.cfg pins. If exactly that version is
#       already installed on this computer it is used as it is; otherwise the
#       official CRAN installer is downloaded, its SHA-256 checked against
#       runtime.cfg, and it is installed silently into this environment's own
#       folder: no administrator rights, no desktop or Start-menu icons, no file
#       associations, and the "current R version" other programs (RStudio) look
#       up is left alone. It does appear in Apps & Features as "R for Windows
#       <version>", where it can be uninstalled like any program.
#    2. The R packages, from one day's CRAN snapshot, into a library of their
#       own (install_packages.R).
#    3. runtime-<version>.cmd, which tells the launcher which Rscript to run.
#
#  Everything lives under %LOCALAPPDATA%\CCR\understory_kelp_indicator -- out
#  of Dropbox on purpose: an R installation plus its packages is tens of
#  thousands of files, which the sync client would push to every computer that
#  shares the folder. Deleting that folder resets everything.
# ---------------------------------------------------------------------------
param(
  [Parameter(Mandatory = $true)] [string] $RepoDir,
  [Parameter(Mandatory = $true)] [string] $UkiHome,
  [Parameter(Mandatory = $true)] [string] $Lib,
  [Parameter(Mandatory = $true)] [string] $RVersion,
  [Parameter(Mandatory = $true)] [string] $RSha256,
  [Parameter(Mandatory = $true)] [string] $Snapshot,
  [Parameter(Mandatory = $true)] [string] $Runtime,
  [switch] $Repair
)

$ErrorActionPreference = 'Stop'
# Invoke-WebRequest's progress bar slows downloads to a crawl in Windows PowerShell
$ProgressPreference = 'SilentlyContinue'
[Net.ServicePointManager]::SecurityProtocol = [Net.SecurityProtocolType]::Tls12

function Say([string] $m) { Write-Host "  $m" }

# Windows PowerShell turns anything a program writes to stderr into an error
# record, and with ErrorActionPreference = Stop that aborts the script -- R and
# curl both report progress on stderr. Programs are run through this instead,
# and judged by their exit code.
function Invoke-Native([scriptblock] $block) {
  $saved = $ErrorActionPreference
  $ErrorActionPreference = 'Continue'
  try { & $block } finally { $ErrorActionPreference = $saved }
}

$privateR = Join-Path $UkiHome "R-$RVersion"
$launcher = Join-Path $RepoDir 'gui\launcher'
$repoUrl  = "https://packagemanager.posit.co/cran/$Snapshot"

New-Item -ItemType Directory -Force -Path $UkiHome | Out-Null
Start-Transcript -Path (Join-Path $UkiHome 'setup.log') -Force | Out-Null

# One setup at a time: a second double-click while the first is still
# installing would otherwise run two installers into the same folder.
$mutex = New-Object System.Threading.Mutex($false, 'Local\UnderstoryKelpIndicator_setup')
if (-not $mutex.WaitOne(0)) {
  Say 'Setup is already running in another window. Let it finish, then start again.'
  Stop-Transcript | Out-Null
  exit 3
}

try {
  Write-Host ''
  Say 'Setting up the Data Interpreter''s own copy of R. This happens once and'
  Say 'takes a few minutes; it needs the internet.'
  Say "Into: $UkiHome"
  Write-Host ''

  if ($Repair) {
    Say 'Repair: removing the package library and the record of which R to use.'
    foreach ($p in @($Lib, $Runtime)) {
      if (Test-Path -LiteralPath $p) { Remove-Item -LiteralPath $p -Recurse -Force }
    }
    if (Test-Path -LiteralPath $privateR) {
      Say "Repair: removing $privateR so R is installed fresh."
      Remove-Item -LiteralPath $privateR -Recurse -Force
    }
  }

  # ---- 1. R --------------------------------------------------------------
  $rscript = $null
  $ours = Join-Path $privateR 'bin\x64\Rscript.exe'
  if (Test-Path -LiteralPath $ours) {
    $rscript = $ours
  } else {
    # Exactly this version already installed? Use it rather than installing a
    # second copy -- the R installer would also take over that copy's
    # Apps & Features entry, since both carry the same name.
    $key = "Software\Microsoft\Windows\CurrentVersion\Uninstall\R for Windows $($RVersion)_is1"
    foreach ($hive in @('HKCU:', 'HKLM:')) {
      $k = Get-ItemProperty -Path (Join-Path $hive $key) -ErrorAction SilentlyContinue
      if ($k -and $k.InstallLocation) {
        $cand = Join-Path $k.InstallLocation 'bin\x64\Rscript.exe'
        if (Test-Path -LiteralPath $cand) {
          Say "R $RVersion is already installed at $($k.InstallLocation) -- using it as it is."
          $rscript = $cand
          break
        }
      }
    }
  }

  if (-not $rscript) {
    $dl = Join-Path $UkiHome 'downloads'
    New-Item -ItemType Directory -Force -Path $dl | Out-Null
    $exe = Join-Path $dl "R-$RVersion-win.exe"
    $want = $RSha256.ToUpperInvariant()

    $have = (Test-Path -LiteralPath $exe) -and ((Get-FileHash -LiteralPath $exe -Algorithm SHA256).Hash -eq $want)
    if (-not $have) {
      # The release moves from base/ to base/old/<version>/ once the next one
      # is out; both are tried, on two CRAN hosts.
      $urls = @(
        "https://cloud.r-project.org/bin/windows/base/old/$RVersion/R-$RVersion-win.exe",
        "https://cran.r-project.org/bin/windows/base/old/$RVersion/R-$RVersion-win.exe",
        "https://cloud.r-project.org/bin/windows/base/R-$RVersion-win.exe",
        "https://cran.r-project.org/bin/windows/base/R-$RVersion-win.exe")
      $curl = Join-Path $env:SystemRoot 'System32\curl.exe'
      foreach ($u in $urls) {
        Say "Downloading R $RVersion (about 90 MB) from"
        Say "  $u"
        try {
          if (Test-Path -LiteralPath $curl) {
            Invoke-Native { & $curl --location --fail --retry 2 --progress-bar --output $exe $u }
            if ($LASTEXITCODE -ne 0) { throw "curl stopped with code $LASTEXITCODE" }
          } else {
            Invoke-WebRequest -Uri $u -OutFile $exe -UseBasicParsing
          }
        } catch {
          Say "  could not download from there: $($_.Exception.Message)"
          continue
        }
        if ((Get-FileHash -LiteralPath $exe -Algorithm SHA256).Hash -eq $want) { $have = $true; break }
        Say '  that file did not match the expected checksum, so it was discarded.'
        Remove-Item -LiteralPath $exe -Force
      }
      if (-not $have) {
        throw "Could not download R $RVersion. Check the internet connection and run this again."
      }
    }

    Say "Installing R $RVersion into $privateR"
    # /CURRENTUSER: no administrator rights. /TASKS="": no desktop icon, no
    # .RData association, and the registry's "current R" is not changed.
    $installLog = Join-Path $UkiHome 'r-install.log'
    $arguments = @('/VERYSILENT', '/SUPPRESSMSGBOXES', '/NORESTART', '/SP-', '/CURRENTUSER',
                   '/NOICONS', '/TASKS=""', "/DIR=`"$privateR`"", "/LOG=`"$installLog`"")
    $proc = Start-Process -FilePath $exe -ArgumentList $arguments -Wait -PassThru
    if ($proc.ExitCode -ne 0 -or -not (Test-Path -LiteralPath $ours)) {
      throw "The R installer stopped with code $($proc.ExitCode). Its log is $installLog"
    }
    $rscript = $ours
    Remove-Item -LiteralPath $exe -Force -ErrorAction SilentlyContinue
  }

  # Whatever was found has to be the version asked for, and has to run
  $got = Invoke-Native { & $rscript --vanilla -e "cat(paste(R.version[['major']], R.version[['minor']], sep = '.'))" }
  if ($LASTEXITCODE -ne 0 -or "$got".Trim() -ne $RVersion) {
    throw "$rscript did not run as R $RVersion (it said: '$got')."
  }
  Say "R $RVersion is ready: $rscript"

  # ---- 2. the packages ------------------------------------------------------
  # The same isolation the launcher uses, so the installer sees no library but
  # this one and reads no one's .Renviron.
  $env:R_LIBS = $Lib
  $env:R_LIBS_USER = $Lib
  $env:R_LIBS_SITE = $Lib
  foreach ($v in @('R_HOME', 'R_ENVIRON', 'R_PROFILE', 'R_ENVIRON_USER', 'R_PROFILE_USER')) {
    Remove-Item -Path "Env:$v" -ErrorAction SilentlyContinue
  }

  if (-not (Test-Path -LiteralPath (Join-Path $Lib '.ukelp_complete'))) {
    Write-Host ''
    Say 'Installing the R packages (a minute or two)...'
    Invoke-Native { & $rscript --vanilla (Join-Path $launcher 'install_packages.R') $Lib $repoUrl }
    if ($LASTEXITCODE -ne 0) {
      throw 'The R packages did not install. The messages above say why.'
    }
  }

  # ---- 3. what the launcher runs ---------------------------------------------
  # Empty files, but files: R reads these in place of the person's own
  # ~/.Renviron and ~/.Rprofile (R_ENVIRON_USER / R_PROFILE_USER), so a library
  # path set there -- the cause of the "entry point not found" crash -- cannot
  # reach this environment, nor the step scripts the app starts.
  foreach ($f in @('Renviron', 'Rprofile')) {
    $p = Join-Path $UkiHome $f
    if (-not (Test-Path -LiteralPath $p)) {
      Set-Content -LiteralPath $p -Value "# deliberately empty -- see gui/launcher/setup_runtime.ps1" -Encoding ASCII
    }
  }
  Set-Content -LiteralPath $Runtime -Encoding ASCII -Value @(
    '@rem written by gui\launcher\setup_runtime.ps1',
    "@set ""UKI_RSCRIPT=$rscript""")

  # ---- tidy up after a version change ---------------------------------------
  # A new R_VERSION, CRAN_SNAPSHOT or LIBRARY_REV gets a new folder; the ones it
  # replaced are a few hundred MB each, so they go.
  $libRoot = Split-Path -Parent $Lib
  Get-ChildItem -LiteralPath $libRoot -Directory -ErrorAction SilentlyContinue |
    Where-Object { $_.FullName -ne $Lib -and $_.Name -like 'R-*' } |
    ForEach-Object { Say "Removing an older package library: $($_.Name)"; Remove-Item -LiteralPath $_.FullName -Recurse -Force }
  # An older private R goes through its own uninstaller, which also takes its
  # entry out of Apps & Features; deleting the folder alone would strand that.
  Get-ChildItem -LiteralPath $UkiHome -Directory -Filter 'R-*' -ErrorAction SilentlyContinue |
    Where-Object { $_.FullName -ne $privateR } |
    ForEach-Object {
      Say "Removing an older R: $($_.Name)"
      $unins = Join-Path $_.FullName 'unins000.exe'
      if (Test-Path -LiteralPath $unins) {
        Start-Process -FilePath $unins -ArgumentList @('/VERYSILENT', '/SUPPRESSMSGBOXES', '/NORESTART') -Wait | Out-Null
      }
      if (Test-Path -LiteralPath $_.FullName) {
        Remove-Item -LiteralPath $_.FullName -Recurse -Force -ErrorAction SilentlyContinue
      }
    }

  Write-Host ''
  Say 'Setup complete.'
  Write-Host ''
  exit 0
}
catch {
  Write-Host ''
  Say "Setup did not finish: $($_.Exception.Message)"
  Say "The full log is $(Join-Path $UkiHome 'setup.log')"
  exit 1
}
finally {
  $mutex.ReleaseMutex()
  Stop-Transcript | Out-Null
}
