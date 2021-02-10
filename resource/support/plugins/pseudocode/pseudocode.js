/**
 * A plugin for pseudocode.js. See: https://github.com/SaswatPadhi/pseudocode.js.
 * 
 * MathJax inside Pseudocode is not rendering. MathJax.tex.inlinemath is not playing well with tex-svg. See test/decks/math-deck.
 */

"use strict";

let Pseudo = (() => {
    var head   = document.querySelector( 'head' );

    function loadScript(script, callback) {
        script.onload = callback;
        script.onreadystatechange = function() {
            if ( this.readyState === 'loaded' ) { callback; }
        }
        head.appendChild( script );
    }
    function loadLink(callback) {
        var link = document.createElement('link');
        link.id = "Pseudocode-style";
        link.rel = "stylesheet";
        link.href = "https://cdn.jsdelivr.net/npm/pseudocode@latest/build/pseudocode.min.css";
        loadScript(link, callback);
    }
    function loadPseudo(callback) {
        var script = document.createElement('script');
        script.type = "text/javascript";
        script.id = 'Pseudocode-script';
        script.src = 'https://cdn.jsdelivr.net/npm/pseudocode@latest/build/pseudocode.min.js';
        loadScript(script, callback);
    }
    function render() {
        let options = Reveal.getConfig().pseudocode;
        for (let elem of document.querySelectorAll('.pseudocode')) {
            elem.style.display = "hidden";
            elem.innerHTML = elem.firstChild.innerHTML;
            window.pseudocode.renderElement(elem, options);
        }
    }

    return {
        init: function() { 
            //uncomment this object and psuedocode works but math breaks.
            
            // window.MathJax = {
            //     tex: {
            //         inlineMath: [['$','$'], ['\\(','\\)']],
            //         displayMath: [['$$','$$'], ['\\[','\\]']],
            //         processEscapes: true,
            //         processEnvironments: true,
            //     }
            // }

            return new Promise(resolve => {
                loadLink(() => {
                    loadPseudo(() => {
                        render();
                        resolve();
                    });
                })
            });
        }
    }
})();

Reveal.registerPlugin( 'pseudo', Pseudo );

