Write-Output "THIS IS A TEST SCRIPT"

Write-Output "$PSScriptRoot"

# $test = "($PSScriptRoot).Directory.Parent.FullName\resource\support\vendor"
$test = ($PSScriptRoot).parent.FullName
Write-Output ($PSScriptRoot).parent.FullName"\resource\support\vendor"
