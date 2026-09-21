param(
    [Parameter(Mandatory = $true)]
    [string]$Prompt,
    [int]$MaxIterations = 0,
    [string]$CompletionPromise,
    [switch]$Force
)

$stateDir = Join-Path $PSScriptRoot "..\\.claude"
$stateDir = [System.IO.Path]::GetFullPath($stateDir)
$stateFile = Join-Path $stateDir "ralph-loop.local.md"

if ((Test-Path $stateFile) -and -not $Force) {
    Write-Error "Ralph loop already active at $stateFile. Use -Force to replace it or cancel it first."
    exit 1
}

New-Item -ItemType Directory -Force -Path $stateDir | Out-Null

$promiseValue = if ([string]::IsNullOrWhiteSpace($CompletionPromise)) { "null" } else { '"' + $CompletionPromise.Replace('"', '\"') + '"' }
$startedAt = [DateTime]::UtcNow.ToString("yyyy-MM-ddTHH:mm:ssZ")

$content = @(
    "---"
    "active: true"
    "iteration: 1"
    "max_iterations: $MaxIterations"
    "completion_promise: $promiseValue"
    "started_at: `"$startedAt`""
    "---"
    ""
    $Prompt
)

Set-Content -Path $stateFile -Value $content -Encoding UTF8

Write-Output "Ralph loop activated."
Write-Output "State file: $stateFile"
Write-Output "Iteration: 1"
Write-Output "Max iterations: $(if ($MaxIterations -gt 0) { $MaxIterations } else { 'unlimited' })"
Write-Output "Completion promise: $(if ($promiseValue -eq 'null') { 'none' } else { $CompletionPromise })"
