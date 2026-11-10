[CmdletBinding(DefaultParameterSetName = 'Text')]
param(
    [Parameter(Mandatory = $true, ParameterSetName = 'Text')]
    [string] $Patch,

    [Parameter(Mandatory = $true, ParameterSetName = 'Base64')]
    [string] $PatchBase64
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

if ($PSCmdlet.ParameterSetName -eq 'Base64') {
    try {
        $Patch = [System.Text.Encoding]::UTF8.GetString(
            [System.Convert]::FromBase64String($PatchBase64)
        )
    }
    catch {
        throw 'PatchBase64 must contain a valid UTF-8 patch encoded as base64.'
    }
}

$Patch = $Patch -replace '\r\n?', [string][char] 10
$trimmedPatch = $Patch.TrimEnd()
$beginMarker = '*** Begin ' + 'Patch'
$endMarker = '*** End ' + 'Patch'

if (-not $trimmedPatch.StartsWith($beginMarker)) {
    throw 'Patch has an invalid opening marker.'
}

if (-not $trimmedPatch.EndsWith($endMarker)) {
    throw 'Patch has an invalid closing marker.'
}

$Patch = $trimmedPatch
$repoRoot = (Resolve-Path (Join-Path $PSScriptRoot '..\..')).Path
$codexCommand = Get-Command codex -CommandType Application -ErrorAction Stop

Push-Location $repoRoot
try {
    & $codexCommand.Source --codex-run-as-apply-patch $Patch
    if ($LASTEXITCODE -ne 0) {
        throw ('The Codex patch interface exited with code {0}.' -f $LASTEXITCODE)
    }
}
finally {
    Pop-Location
}
