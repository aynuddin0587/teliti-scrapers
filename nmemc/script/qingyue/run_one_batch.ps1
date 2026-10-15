# Runs EXACTLY ONE bounded acquisition batch against the existing v1.1.0 executor.
# Never modifies the approved plan or replaces the SQLite ledger.
# Execute deliberately from a PowerShell terminal after reviewing the request list.
param(
    [Parameter(Mandatory=$true)]
    [ValidateSet('3','4','deferred-2020')]
    [string]$Stage,

    [Parameter(Mandatory=$true)]
    [string]$RequestList,

    [ValidateRange(1,500)]
    [int]$MaxNewCalls = 250,

    [ValidateRange(0.25,30.0)]
    [double]$DelaySeconds = 0.5,

    [switch]$Backup,
    [switch]$Export
)

$ErrorActionPreference = 'Stop'
$hist = 'D:/# R Project/penelitian/nmemc/data/surfacewater/qingyue/historical'
$exe = 'D:/# R Project/penelitian/nmemc/script/qingyue/qingyue_historical_executor.py'
$plan = Join-Path $hist 'plan/qingyue_request_plan_recommended.csv'
$expectedSha = 'A8E0C166F199971F63BDB209D4E5C13BBF5DAE21EEC9EBD45447AD6C453C6587'

if (-not (Test-Path -LiteralPath $RequestList -PathType Leaf)) {
    throw "STOP: request list not found: $RequestList"
}
if (-not (Test-Path -LiteralPath $plan -PathType Leaf)) {
    throw "STOP: original plan not found: $plan"
}
if (-not (Test-Path -LiteralPath $exe -PathType Leaf)) {
    throw "STOP: executor not found: $exe"
}
$actualSha = (Get-FileHash -LiteralPath $plan -Algorithm SHA256).Hash
if ($actualSha -ne $expectedSha) {
    throw 'STOP: original 39,626-row plan checksum does not match.'
}

function Invoke-Executor {
    param([string[]]$Arguments)
    & python $exe @Arguments
    if ($LASTEXITCODE -ne 0) {
        throw "STOP: executor failed with exit code $LASTEXITCODE"
    }
}

Write-Host "Confirmed approved plan SHA256; one-batch limit: $MaxNewCalls HTTP attempts."
Invoke-Executor -Arguments @('status','--request-list',$RequestList)
if ($Backup) {
    Invoke-Executor -Arguments @('backup-state')
}
Invoke-Executor -Arguments @('run','--stage',$Stage,'--request-list',$RequestList,
                             '--max-new-calls',[string]$MaxNewCalls,
                             '--delay',[string]::Format([System.Globalization.CultureInfo]::InvariantCulture,'{0}', $DelaySeconds))
Invoke-Executor -Arguments @('status','--request-list',$RequestList)
if ($Export) {
    Invoke-Executor -Arguments @('export')
}
Write-Host 'One bounded batch finished. Check the HTTP statuses and unresolved counts before repeating.'
