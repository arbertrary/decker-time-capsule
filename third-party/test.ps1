$current = Split-Path -parent $PSCommandPath
$parent = Split-Path $current -parent
$support = Resolve-Path "$parent\resource\support\vendor"
$support = $ExecutionContext.SessionState.Path.GetUnresolvedProviderPathFromPSPath("$parent\resource\support\vendor")

Write-Output "$current"
Write-Output "$parent"
Write-Output "$support"
Write-Output "$test"