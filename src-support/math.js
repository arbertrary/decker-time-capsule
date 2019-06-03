// require("mathjax/fonts/HTML-CSS/TeX/woff/MathJax_Zero.woff");
require("mathjax/fonts/HTML-CSS/TeX/woff/MathJax_Math-Italic.woff");
require("mathjax/fonts/HTML-CSS/TeX/woff/MathJax_Main-Regular.woff");
require("mathjax/fonts/HTML-CSS/TeX/woff/MathJax_Main-Bold.woff");
require("mathjax/fonts/HTML-CSS/TeX/woff/MathJax_Math-BoldItalic.woff");
require("mathjax/fonts/HTML-CSS/TeX/woff/MathJax_Size1-Regular.woff");
require("mathjax/fonts/HTML-CSS/TeX/woff/MathJax_AMS-Regular.woff");
require("mathjax/fonts/HTML-CSS/TeX/woff/MathJax_Size4-Regular.woff");

// Webpack handling of MathJax copied from
// https://github.com/mathjax/mathjax-v3/wiki/A-first-usable-demo-(using-webpack)
// the MathJax core
const MathJax = require("mathjax3/mathjax3/mathjax.js").MathJax;
// MathML input
const TeX = require("mathjax3/mathjax3/input/tex.js").TeX;
// HTML output
const CHTML = require("mathjax3/mathjax3/output/chtml.js").CHTML;
const SVG = require("mathjax3/mathjax3/output/svg.js").SVG;
// Use browser DOM
const adaptor = require("mathjax3/mathjax3/adaptors/browserAdaptor").browserAdaptor();
// Register the HTML document handler
require("mathjax3/mathjax3/handlers/html.js").RegisterHTMLHandler(adaptor);
require("mathjax3/mathjax3/input/tex/ams/AmsConfiguration.js");
require("mathjax3/mathjax3/input/tex/base/BaseConfiguration.js");
require("mathjax3/mathjax3/input/tex/ams/AmsConfiguration.js");
require("mathjax3/mathjax3/input/tex/noundefined/NoUndefinedConfiguration.js");
require("mathjax3/mathjax3/input/tex/newcommand/NewcommandConfiguration.js");
require("mathjax3/mathjax3/input/tex/boldsymbol/BoldsymbolConfiguration.js");
require("mathjax3/mathjax3/input/tex/braket/BraketConfiguration.js");
require("mathjax3/mathjax3/input/tex/mhchem/MhchemConfiguration.js");
require("mathjax3/mathjax3/input/tex/physics/PhysicsConfiguration.js");
require("mathjax3/mathjax3/input/tex/verb/VerbConfiguration.js");
require("mathjax3/mathjax3/input/tex/cancel/CancelConfiguration.js");
require("mathjax3/mathjax3/input/tex/enclose/EncloseConfiguration.js");

// initialize mathjax with with the browser DOM document; other documents are possible
const html = MathJax.document(document, {
  InputJax: new TeX({
    inlineMath: [["$", "$"], ["\\(", "\\)"]],
    packages: [
      "base",
      "ams",
      "noundefined",
      "newcommand",
      "boldsymbol",
      "braket",
      "mhchem",
      "physics",
      "verb",
      "cancel",
      "enclose"
    ]
  }),
  OutputJax: new SVG()
  // OutputJax: new CHTML({
  //   fontURL: window.deckerSupportDir + "/fonts",
  // })
});

window.addEventListener("load", function() {
  console.time("wrapper");
  // process the document
  html
    .findMath()
    .compile()
    .getMetrics()
    .typeset()
    .updateDocument();
  console.timeEnd("wrapper");
});