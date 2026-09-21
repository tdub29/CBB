@echo off
REM Build coach_tournament_experience.csv
REM Options: Try R (toRvik) first if available, else Python with Playwright

cd /d "%~dp0.."

REM Try R script (requires toRvik - install with: install.packages("toRvik") or Rtools + remotes::install_github("andreweatherman/toRvik"))
where Rscript >nul 2>&1
if %ERRORLEVEL% EQU 0 (
  echo Trying R script...
  Rscript scripts\fetch_coach_experience.R
  if %ERRORLEVEL% EQU 0 exit /b 0
)

REM Fallback: Python with Playwright (~20 min for 367 teams)
echo Using Python + Playwright...
python scripts\fetch_coach_experience.py --fetch --playwright
exit /b %ERRORLEVEL%
