echo "Building Windows Decker"
& stack build
& yarn install
& yarn run webpack --mode production
Copy-Item -Recurse -Force node_modules/reveal.js-menu resource/support/

$binpath=(Join-Path ($(stack path | Select-String -Pattern "local-install-root") -split " ")[1] "bin\decker.exe")
Copy-Item $binpath .