$third = Split-Path -parent $PSCommandPath
$decker = Split-Path $third -parent
# $support = Resolve-Path "$decker\resource\support\vendor"
$support = $ExecutionContext.SessionState.Path.GetUnresolvedProviderPathFromPSPath("$decker\resource\support\vendor")

& mkdir $support -Force

# Build jquery
Set-Location "$third/jquery"
& npm run build
# (Invoke-Expression "npm run build")

# Build thebelab
Set-Location "$third/thebelab"
& npm install
& npm run build
# (Invoke-Expression "npm install")
# (Invoke-Expression "npm run build")

# Build Chart.js
Set-Location "$third/Chart.js"
& npm install
& npx rollup -c rollup.config.js
# (Invoke-Expression "npm install")
# (Invoke-Expression "npx rollup -c rollup.config.js")

Set-Location "$third"

# copy jquery
& cp $third/jquery/dist/jquery.min.js $support/jquery.js -Force

# copy thebelab
& mkdir $support/thebelab -Force
& cp $third/thebelab/lib/*.js $support/thebelab -Force
& cp $third/thebelab/lib/*.map $support/thebelab -Force

# copy mathjax
& mkdir $support/mathjax/jax/input -Force 
& mkdir $support/mathjax/jax/output -Force
Foreach ($i in ("MathJax.js", "config", "jax/input/TeX", "jax/output/SVG", "jax/element", "extensions")) {
  cp -r $third/MathJax/$i $support/mathjax/$i -Force
}



# Copy reveal.js
& mkdir $support/reveal/plugin -Force 
& mkdir $support/reveal/plugin/markdown -Force 
& mkdir $support/reveal/plugin/markdown -Force
& cp $third/reveal.js/plugin/markdown/marked.js $support/reveal/plugin/markdown/marked.js -Force
Foreach ($i in ("js", "css", "lib", "plugin/math", "plugin/zoom-js", "plugin/notes")) {
  cp -r $third/reveal.js/$i $support/reveal/$i -Force
}

# Copy Reveal.js-menu
& mkdir $support/reveal.js-menu -Force
Foreach ($i in ("menu.css", "menu.js")) {
  cp -r $third/reveal.js-menu/$i $support/reveal.js-menu/$i -Force
}

# Copy bootstrap
& mkdir $support/bootstrap -Force
& cp -r $third/bootstrap/dist/css $support/bootstrap/css -Force

# Copy piklor.js
& cp -r $third/piklor.js/src/piklor.min.js $support/piklor.js -Force

# Copy whiteboard
& cp -r $third/mb-reveal-plugins/whiteboard $support/whiteboard -Force

# Copy Charts
& cp -r $third/mb-reveal-plugins/charts $support/charts -Force

# copy math
& cp -r $third/mb-reveal-plugins/math $support/math -Force

# copy highlight
& cp -r $third/mb-reveal-plugins/highlight $support/highlight -Force

# copy fontawesome
& mkdir $support/fontawesome -Force
Foreach ($i in ( "js", "css", "webfonts", "svgs", "sprites")) {
  cp -r $third/Font-Awesome/js-packages/@fortawesome/fontawesome-free/$i $support/fontawesome/$i -Force
}
