let module_reveal = undefined;

import {
  makePrintPDFCallback,
  makeToggleDeckerMenuCallback,
  makeToggleFragmentsCallback,
  makeToggleSearchbarCallback,
  makePreventDefaultCallback,
  makeFocusTraversalAlteration } from "./callbacks.js";

class SlideMenu {
  id;
  reveal;
  config;
  decker_button;
  decker_anchor;

  constructor() {
    this.id = "newmenu";
    this.reveal = undefined;
    this.config = undefined;
    this.decker_button = this.prepareButton();
    this.decker_anchor = "TOP_LEFT";
  }

  announce_status(reveal, text) {
    if(reveal.hasPlugin("a11y-status")) {
      let status = reveal.getPlugin("a11y-status");
      status.announce(text);
    } else {
      console.log("No a11y-status plugin found.");
    }
  }

  prepareDOM() {
    let reveal_element = document.querySelector(".reveal");
    let menu = this.prepareMenu();
    if(!this.reveal.hasPlugin('decker-plugins')) {
      console.log("no decker plugin manager loaded");
      return;
    }
    let manager = this.reveal.getPlugin("decker-plugins");
    manager.registerPlugin(this);
    reveal_element.appendChild(menu);
  }

  prepareButton() {
    let template = document.createElement("template");
    let reader_text = "Open Navigation Menu"; //TODO: LOCALIZATION
    template.innerHTML = String.raw
    `<button class="decker-button" id="decker-menu-button" title="${reader_text}" aria-label="${reader_text}">
      <i class="fas fa-bars"></i>
    </button>`;

    let button = template.content.firstChild;
    button.addEventListener("click", makeToggleDeckerMenuCallback());
    return button;
  }

  prepareList() {
    let template = document.createElement("template");
    template.innerHTML = String.raw
    `<div class="slide-list-wrapper">
      <ul class="slide-list">
    
      </ul>
    </div>`
    let wrapper = template.content.firstChild;
    let list = wrapper.querySelector(".slide-list");
    let slides = document.querySelectorAll(".slides > section");
    slides.forEach((slide, h) => {
      let subslides = slide.querySelectorAll("section");
      if(subslides.length > 0) {
        subslides.forEach((subslide, v) => {
          //Maybe we do not need these
        });
      }
      var item = this.prepareListItem(slide, h)
      list.appendChild(item);
    });
    return wrapper;
  }

  prepareListItem(slide, h, v) {
    //Creating the item this way is far less confusing than using a template
    let item = document.createElement("li");
    let link = document.createElement("a");
    let href = "#/" + h + (v ? "/"+v : "");
    item.classList.add("slide-list-item");
    link.classList.add("slide-link");
    link.setAttribute("href", href);
    link.setAttribute("target", "_self");
    item.setAttribute("data-slide-h", h);
    if(v) {
      item.setAttribute("data-slide-v", v);
    }
    link.textContent = this.getTitle(slide, "h1, h2, h3, h4, h5"); //Default slide-menu behaviour
    link.addEventListener("click", makeToggleDeckerMenuCallback())
//    link.setAttribute("onclick", toggleDeckerMenu);
    item.appendChild(link);
    return item;
  }

  getTitle(section, selector) {
    let title = this.getTitleFromAttributesOrChildren(section, selector);
    if(!title) {
      title = this.getTitleFromSectionContent(section);
    }
    if(!title) {
      title = "Kein Titel"; //MAYBE: Query from localization options (far future feature?)
    }
    return title;
  }

  //Taken from Greg Denehy's Menu Plugin
  getTitleFromAttributesOrChildren(section, selector) {
    let title = section.getAttribute("data-menu-title");
    if(!title) {
      let element = section.querySelector(".menu-title");
      if(element) {
        title = element.textContent;
      }
    }
    if(!title && selector) {
      let element = section.querySelector(selector);
      if(element) {
        title = element.textContent;
      }
    }
    return title;
  }

  //Taken from Greg Denehy's Menu Plugin
  getTitleFromSectionContent(section) {
    let title = section.textContent.trim();
    if (title) {
      title =
        title
          .split("\n")
          .map(function (t) {
            return t.trim();
          })
          .join(" ")
          .trim()
          .replace(/^(.{16}[^\s]*).*/, "$1") // limit to 16 chars plus any consecutive non-whitespace chars (to avoid breaking words)
          .replace(/&/g, "&amp;")
          .replace(/</g, "&lt;")
          .replace(/>/g, "&gt;")
          .replace(/"/g, "&quot;")
          .replace(/'/g, "&#039;") + "...";
    }
    return title;
  }

  prepareMenu() {
    let template = document.createElement("template");
    let animations = this.reveal.getConfig().fragments;
    let toggle_icon = animations ? "fa-check-circle" : "fa-circle";
    template.innerHTML = String.raw
    `<div class="decker-menu" id="decker-menu" inert>
      <div class="tile-grid">
        <button class="tile" id="decker-menu-search-button">
          <i class="fas fa-search"></i>
          <p>Toggle Search Bar</p>
        </button>
        <button class="tile" id="decker-menu-print-button">
          <i class="fas fa-print"></i>
          <p>Print as PDF</p>
        </button>
        <button class="switch tile" id="decker-menu-animation-button">
          <i class="far ${toggle_icon}"></i>
          <p>Toggle Animations</p>
        </button>
        <button class="close tile">
          <i class="fas fa-times"></i>
          <p>Close Menu</p>
        </button>
      </div>
     </div>`
    let menu = template.content.firstChild;

    let closeCallback = makeToggleDeckerMenuCallback();

    /* Attaching callbacks to buttons */
    let searchToggle = menu.querySelector("#decker-menu-search-button")
    searchToggle.addEventListener("click", makeToggleSearchbarCallback(this.reveal));
    searchToggle.addEventListener("click", closeCallback);
    let printPDFButton = menu.querySelector("#decker-menu-print-button");
    printPDFButton.addEventListener("click", makePrintPDFCallback());
    printPDFButton.addEventListener("click", closeCallback);
    let fragmentToggle = menu.querySelector("#decker-menu-animation-button")
    fragmentToggle.addEventListener("click", makeToggleFragmentsCallback(this.reveal));
    let closeButton = menu.querySelector(".close");
    closeButton.addEventListener("click", closeCallback);

    /* Slide-List and keyboard handling */
    let list = this.prepareList();
    menu.appendChild(list);
    list.addEventListener("keydown", makePreventDefaultCallback(menu));
    menu.addEventListener("keydown", makeFocusTraversalAlteration(menu));
    return menu;
  }



  init(reveal) {
    module_reveal = reveal;
    this.reveal = reveal;
    this.config = reveal.getConfig();
    console.log("init new slide menu");

    this.prepareDOM();
  }
}

let instance = new SlideMenu();

export default instance;
