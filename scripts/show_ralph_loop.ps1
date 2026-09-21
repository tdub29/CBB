$stateFile = Join-Path $PSScriptRoot "..\\.claude\\ralph-loop.local.md"
$stateFile = [System.IO.Path]::GetFullPath($stateFile)

if (-not (Test-Path $stateFile)) {
    Write-Output "No active Ralph loop."
    exit 0
}

$raw = Get-Content -Path $stateFile

function Get-FrontMatterValue {
    param([string[]]$Lines, [string]$Key)
    $pattern = '^{0}:' -f [regex]::Escape($Key)
    $match = $Lines | Where-Object { $_ -match $pattern } | Select-Object -First 1
    if (-not $match) { return $null }
    return ($match -split ":\s*", 2)[1]
}

$iteration = Get-FrontMatterValue -Lines $raw -Key "iteration"
$maxIterations = Get-FrontMatterValue -Lines $raw -Key "max_iterations"
$completionPromise = Get-FrontMatterValue -Lines $raw -Key "completion_promise"
$startedAt = Get-FrontMatterValue -Lines $raw -Key "started_at"

Write-Output "Ralph loop active."
Write-Output "State file: $stateFile"
Write-Output "Iteration: $iteration"
Write-Output "Max iterations: $maxIterations"
Write-Output "Completion promise: $completionPromise"
Write-Output "Started at: $startedAt"
Write-Output ""
Write-Output "Prompt:"
Write-Output ($raw | Select-Object -Skip 7)
