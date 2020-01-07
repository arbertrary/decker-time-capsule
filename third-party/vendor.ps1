$parent = ($PSScriptRoot).parent.FullName
$support = "$parent\resource\support\vendor"

# Build jquery
Set-Location "$PSScriptRoot/jquery"
& npm run build
# (Invoke-Expression "npm run build")

# Build thebelab
Set-Location "$PSScriptRoot/thebelab"
& npm install
& npm run build
# (Invoke-Expression "npm install")
# (Invoke-Expression "npm run build")

# Build Chart.js
Set-Location "$PSScriptRoot/Chart.js"
& npm install
& npx rollup -c rollup.config.js
# (Invoke-Expression "npm install")
# (Invoke-Expression "npx rollup -c rollup.config.js")

Set-Location "$PSScriptRoot"

# copy jquery
& cp $PSScriptRoot/jquery/dist/jquery.min.js $support/jquery.js

# copy thebelab
& mkdir $support/thebelab
& cp $PSScriptRoot/thebelab/lib/*.js $support/thebelab
& cp $PSScriptRoot/thebelab/lib/*.map $support/thebelab

# copy mathjax
& mkdir $support/mathjax/jax/input $support/mathjax/jax/output
Foreach ($i in ("MathJax.js", "config", "jax/input/TeX", "jax/output/SVG", "jax/element", "extensions")) {
  cp -r $PSScriptRoot/MathJax/$i $support/mathjax/$i
}

# Copy reveal.js
& mkdir $support/reveal/plugin $support/reveal/plugin/markdown $support/reveal/plugin/markdown
& cp $PSScriptRoot/reveal.js/plugin/markdown/marked.js $support/reveal/plugin/markdown/marked.js
Foreach ($i in ("css", "lib", "plugin/math", "plugin/zoom-js", "plugin/notes")) {
  cp -r $PSScriptRoot/reveal.js/$i $support/reveal/$i
}

# Copy Reveal.js-menu
& mkdir $support/reveal.js-menu
Foreach ($i in ("menu.css", "menu.js")) {
  cp -r $PSScriptRoot/reveal.js-menu/$i $support/reveal.js-menu/$i
}

# Copy bootstrap
& mkdir $support/bootstrap
& cp -r $PSScriptRoot/bootstrap/dist/css $support/bootstrap/css

# Copy piklor.js
& cp -r $PSScriptRoot/piklor.js/src/piklor.min.js $support/piklor.js

# Copy whiteboard
& cp -r $PSScriptRoot/mb-reveal-plugins/whiteboard $support/whiteboard

# Copy Charts
& cp -r $PSScriptRoot/mb-reveal-plugins/charts $support/charts

# copy math
& cp -r $PSScriptRoot/mb-reveal-plugins/math $support/math

# copy highlight
& cp -r $PSScriptRoot/mb-reveal-plugins/highlight $support/highlight

# copy fontawesome
& mkdir $support/fontawesome
Foreach ($i in ( "js", "css", "webfonts", "svgs", "sprites")) {
  cp -r $PSScriptRoot/Font-Awesome/js-packages/@fortawesome/fontawesome-free/$i $support/fontawesome/$i
}
