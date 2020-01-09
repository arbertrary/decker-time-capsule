<#
.SYNOPSIS
    Builds Decker for windows
.DESCRIPTION
    Build script to build Decker for windows. Allows to set the build
    mode via parameters for custom builds in the CI pipeline.
#>
Param(
    [switch] $skiptemplates,
    [switch] $preparepackage
)
Write-Output "Building Windows Decker"
if (-Not $skiptemplates) {
    Write-Output "Copying resources to resource directory is not available at the moment. See the Makefile for assistance."
}

Write-Output "Building standalone binary"
& stack clean
& git submodule update --init
& .\third-party\vendor.ps1
Set-Location (Split-Path $PSScriptRoot -Parent)
& stack build -j4



if ($preparepackage) {
    $binpath = (Join-Path ($(stack path | Select-String -Pattern "local-install-root") -split " ")[1] "bin\decker.exe")
    Copy-Item $binpath .

    $version = Get-Content .\package.yaml | Select-String -Pattern "version: "
    $version = $version -replace "\s+", " "
    $version = ($version -split " ")[1]
    Write-Output $version > version.txt
}