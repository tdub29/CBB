param(
    [switch]$Complete
)

$stateFile = Join-Path $PSScriptRoot "..\\.claude\\ralph-loop.local.md"
$stateFile = [System.IO.Path]::GetFullPath($stateFile)

if (-not (Test-Path $stateFile)) {
    Write-Error "No active Ralph loop."
    exit 1
}

$raw = Get-Content -Path $stateFile

function Get-FrontMatterValue {
    param([string[]]$Lines, [string]$Key)
    $pattern = '^{0}:' -f [regex]::Escape($Key)
    $match = $Lines | Where-Object { $_ -match $pattern } | Select-Object -First 1
    if (-not $match) { return $null }
    return ($match -split ":\s*", 2)[1].Trim()
}

$iteration = [int](Get-FrontMatterValue -Lines $raw -Key "iteration")
$maxIterations = [int](Get-FrontMatterValue -Lines $raw -Key "max_iterations")
$completionPromise = Get-FrontMatterValue -Lines $raw -Key "completion_promise"

if ($Complete) {
    Remove-Item -Force $stateFile
    Write-Output "Ralph loop completed and cleared."
    exit 0
}

$nextIteration = $iteration + 1
$updated = foreach ($line in $raw) {
    if ($line -match "^iteration:") {
        "iteration: $nextIteration"
    } else {
        $line
    }
}

Set-Content -Path $stateFile -Value $updated -Encoding UTF8

Write-Output "Ralph loop advanced to iteration $nextIteration."

if (($maxIterations -gt 0) -and ($nextIteration -ge $maxIterations)) {
    Write-Output "Max iterations reached."
    if ($completionPromise -ne "null") {
        Write-Output "Required promise before completion: $completionPromise"
    }
}
