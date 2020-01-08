<#
.SYNOPSIS
    Builds Decker for windows
.DESCRIPTION
    Build script to build Decker for windows. Allows to set the build
    mode via parameters for custom builds in the CI pipeline.
.PARAMETER buildtype
    Defaults to assume preextracted resources for the build.
    If set to any other value will include the resources in the executable.
#>
Param(
    [string] $buildtype = "standalone",
    [switch] $skiptemplates,
    [switch] $preparepackage
)
Write-Output "Building Windows Decker"
if (-Not $skiptemplates) {
    Write-Output "Copying resources to resource directory is not available at the moment. See the Makefile for assistance."
}

Write-Output "Building standalone binary"
<# TODO: Here we need to copy resources from `third-party` to
`resource/support/vendor` as done in `third-party/symlinks.mk`. Once that
works, pre-extraction is obsolete because everything is read directly from
the embedded ZIP archive. #>

& stack clean
& git submodule update --init
& .\third-party\vendor.ps1
# & .\third-party\test.ps1
Set-Location (Split-Path $PSScriptRoot -Parent)
& stack build -j4

# if($buildtype -eq "preextracted"){
#     Write-Output "Building for preextracted resources"
#     & stack build -j4 --flag decker:preextractedresources
# } else {
#     Write-Output "Building standalone binary"
#     <# TODO: Here we need to copy resources from `third-party` to
#     `resource/support/vendor` as done in `third-party/symlinks.mk`. Once that
#     works, pre-extraction is obsolete because everything is read directly from
#     the embedded ZIP archive. #>
#     & stack build -j4
# }

if ($preparepackage) {
    $binpath = (Join-Path ($(stack path | Select-String -Pattern "local-install-root") -split " ")[1] "bin\decker.exe")
    Copy-Item $binpath .

    $version = Get-Content .\package.yaml | Select-String -Pattern "version: "
    $version = $version -replace "\s+", " "
    $version = ($version -split " ")[1]
    Write-Output $version > version.txt

    # if($buildtype -eq "preextracted"){
    #   Compress-Archive -Force -Path .\resource -CompressionLevel Fastest -DestinationPath resource
    # }
}