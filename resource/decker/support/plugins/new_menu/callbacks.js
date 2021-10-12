/**
 * Generators for callback functions related to the decker menu plugin.
 */

export function makeToggleDeckerMenuCallback() {
    return function(event) {
        let menu = document.querySelector("#decker-menu");
        menu.inert = !menu.inert;
        if(menu.inert) {
            document.querySelector("#decker-menu-button").focus();
        } else {
            menu.querySelector("a").focus();
        }
    }
}

export function makeToggleFragmentsCallback(reveal) {
    return function(event) {
        let button = document.getElementById("decker-menu-animation-button");
        let animations = reveal.getConfig().fragments;
        reveal.configure({ fragments: !animations });
        if(!animations) {
            button.classList.add("checked");
            button.querySelector(".far").classList.remove("fa-circle");
            button.querySelector(".far").classList.add("fa-check-circle");
        } else {
            button.classList.remove("checked");
            button.querySelector(".far").classList.remove("fa-check-circle");
            button.querySelector(".far").classList.add("fa-circle");
        }
    }
}

export function makeToggleSearchbarCallback(reveal) {
    return function(event) {
        if (reveal.hasPlugin("search")) reveal.getPlugin("search").toggle();
    }
}

export function makePrintPDFCallback(){
    return function(event) {
        if (window.electronApp) {
            let url = location.protocol + "//" + location.host + location.pathname;
            window.electronApp.printPDF(url);
        } else {
            if (confirm("Leave/reload presentation to export PDF?")) { //MAYBE Localization
                let url = location.protocol + "//" + location.host + location.pathname + "?print-pdf";
                window.open(url, "_self");
            }
        }
    }
}

export function makePreventDefaultCallback(menu) {
    return function(event) {
        if(!menu.inert && (event.code == "Escape" || event.code == "ArrowUp" || event.code == "ArrowDown")) {
            event.preventDefault();
        }
    }
}

export function makeFocusTraversalAlteration(menu) {
    return function(event) {
        if(!menu.inert) {
            switch(event.code) {
                case "Escape":
                    event.stopImmediatePropagation();
                    toggleDeckerMenu();
                    break;
                case "ArrowUp":
                    if(document.activeElement && document.activeElement.classList.contains("slide-link")) {
                        event.stopImmediatePropagation();
                        let parent = document.activeElement.parentElement;
                        let target = undefined;
                        if(parent.previousElementSibling) { //target the a inside the previous list item
                            target = parent.previousElementSibling.firstChild;  
                        } else { // wrap around
                            target = parent.parentElement.lastElementChild.firstChild;
                        }    
                        target.focus();
                    }
                    break;
              case "ArrowDown":
                if(document.activeElement && document.activeElement.classList.contains("slide-link")) {
                    event.stopImmediatePropagation();
                    let parent = document.activeElement.parentElement;
                    let target = undefined;
                    if(parent.nextElementSibling) { //target the a inside the previous list item
                        target = parent.nextElementSibling.firstChild;  
                    } else { // wrap around
                        target = parent.parentElement.firstElementChild.firstChild;
                    }
                    target.focus();
                }
                break;
              default:
            }
        }
    }
}