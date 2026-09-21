$stateFile = Join-Path $PSScriptRoot "..\\.claude\\ralph-loop.local.md"
$stateFile = [System.IO.Path]::GetFullPath($stateFile)

if (-not (Test-Path $stateFile)) {
    Write-Output "No active Ralph loop found."
    exit 0
}

$iterationLine = Get-Content -Path $stateFile | Where-Object { $_ -match "^iteration:" } | Select-Object -First 1
$iteration = if ($iterationLine) { ($iterationLine -split ":\s*", 2)[1] } else { "unknown" }

Remove-Item -Force $stateFile
Write-Output "Cancelled Ralph loop (was at iteration $iteration)."
