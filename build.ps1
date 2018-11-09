Write-Output "Building Windows Decker"
& yarn install
& yarn run webpack --mode production
Copy-Item -Recurse -Force node_modules/reveal.js-menu resource/support/
Copy-Item -Recurse -Force node_modules/reveal.js/plugin/notes resource/support/
& stack build

$binpath=(Join-Path ($(stack path | Select-String -Pattern "local-install-root") -split " ")[1] "bin\decker.exe")
Copy-Item $binpath .

$version = Get-Content .\package.yaml | Select-String -Pattern "version: "
$version = $version -replace "\s+", " "
$version = ($version -split " ")[1]
Write-Output $version > version.txt

Compress-Archive -Force -Path .\resource -CompressionLevel Fastest -DestinationPath resource