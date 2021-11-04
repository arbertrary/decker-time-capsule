/**
 * A rewrite of the Slide Menu, inspired by the Reveal.js Slide Menu Plugin
 * made by Greg Denehy but leaving out a lot of configuration options and adding
 * accessibility features like using the inate polyfill, focus management and 
 * proper aria-labels.
 * 
 * @author Sebastian Hauer 
 */

 class SlideMenu {
  id;
  reveal;
  config;
  open_button;
  position;
  menu;
  slide_list;

  constructor(position) {
    this.id = "decker-menu";
    this.reveal = undefined;
    this.config = undefined;
    this.open_button = undefined;
    this.menu = {
      container: undefined,
      search_button: undefined,
      pdf_button: undefined,
      fragments_button: undefined,
      close_button: undefined,
      slide_list: undefined,
    }
    this.position = position;
  }

  get inert() {
    return this.menu.container.inert;
  }

  set inert(value) {
    this.menu.container.inert = !!value; //force cast to boolean
  }

  /**
   * Exposes the list items for other plugins.
   * @param {*} h 
   * @param {*} v 
   * @returns 
   */
  getListItem(h, v) {
    let childNodes = this.menu.slide_list.childNodes;
    for(let i = 0; i < childNodes.length; i++) {
      if(childNodes[i].getAttribute("data-slide-h") == h) {
        if(v) {
          if(childNodes[i].getAttribute("data-slide-v") == v) {
            return childNodes[i];
          }
        } else {
          return childNodes[i];
        }
      }
    }
    return undefined;
  }

  /**
   * Exposes the slide list for other plugins.
   * @returns 
   */
  getSlideList() {
    return this.menu.slide_list.childNodes;
  }

  /**
   * Toggles the inert attribute of the menu on or off.
   * @param {*} event 
   */
  toggleMenu(event) {
    console.log("toggle menu called");
    if(this.inert) {
      this.openMenu(event);
    } else {
      this.closeMenu(event);
    }
  }

  /**
   * Opens the menu by removing inert.
   * @param {*} event 
   */
  openMenu(event) {
    if(this.inert) {
      this.inert = false;
      if(event && event.detail === 0) {
        this.menu.search_button.focus();
      }
    }
  }

  /**
   * Closes the menu by adding inert.
   * @param {*} event 
   */
  closeMenu(event) {
    if(!this.inert) {
      this.inert = true;
      if(event && event.detail === 0) {
        this.menu.decker_button.focus();
      }
    }
  }

  /**
   * Toggles the searchbar of the searchbar plugin.
   */
  toggleSearchbar() {
    if (this.reveal.hasPlugin("search")) this.reveal.getPlugin("search").toggle();
  }

  /**
   * Reopens the tab with ?print-pdf to allow PDF printing.
   */
  printPDF() {
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

  /**
   * Enables or disables the fragmentation of slides.
   */
  toggleFragments() {
    let animations = this.reveal.getConfig().fragments;
    this.reveal.configure({ fragments: !animations });
    if(!animations) {
      this.menu.fragments_button.classList.add("checked");
      this.menu.fragments_button.querySelector("i").classList.remove("fa-circle");
      this.menu.fragments_button.querySelector("i").classList.add("fa-check-circle");
    } else {
      this.menu.fragments_button.classList.remove("checked");
      this.menu.fragments_button.querySelector("i").classList.remove("fa-check-circle");
      this.menu.fragments_button.querySelector("i").classList.add("fa-circle");
    }
  }

  /**
   * If there is a status field to announce changes to the GUI then use that to announce
   * changes.
   * TODO: Test if this is actually necessary.
   * @param {*} text 
   */
  announceStatus(text) {
    if(this.reveal.hasPlugin("a11y-status")) {
      let status = this.reveal.getPlugin("a11y-status");
      status.announce(text);
    } else {
      console.log("No a11y-status plugin found.");
    }
  }

  /**
   * Stops the default functionality of moving up or down the scrollbar of the slide wrapper div. 
   * @param {*} event The Keyboard Event
   */
  ignoreTraversalKeys(event) {
    if(!this.inert && (event.code == "Escape" || event.code == "ArrowUp" || event.code == "ArrowDown")) {
      event.preventDefault();
    }
  }

  /**
   * Changes the way the up, down and escape keys work when a link inside the slide-list is focused.
   * @param {*} event The Keyboard Event
   */
  traverseList(event) {
    if(!this.inert) {
      switch(event.code) {
          case "Escape":
              event.stopImmediatePropagation();
              this.closeMenu();
              break;
          case "ArrowUp":
              if(document.activeElement && document.activeElement.classList.contains("tile")) {
                event.preventDefault();
                event.stopImmediatePropagation();
                this.menu.slide_list.firstElementChild.firstElementChild.focus();
              }
              if(document.activeElement && document.activeElement.classList.contains("slide-link")) {
                  event.stopImmediatePropagation();
                  let parent = document.activeElement.parentElement;
                  let target = undefined;
                  if(parent.previousElementSibling) { //target the a inside the previous list item
                      target = parent.previousElementSibling.firstElementChild;  
                  } else { // wrap around
                      target = parent.parentElement.lastElementChild.firstElementChild;
                  }    
                  target.focus();
              }
              break;
        case "ArrowDown":
          if(document.activeElement && document.activeElement.classList.contains("tile")) {
            event.preventDefault();
            event.stopImmediatePropagation();
            this.menu.slide_list.lastElementChild.firstElementChild.focus();
          }
          if(document.activeElement && document.activeElement.classList.contains("slide-link")) {
              event.stopImmediatePropagation();
              let parent = document.activeElement.parentElement;
              let target = undefined;
              if(parent.nextElementSibling) { //target the a inside the previous list item
                  target = parent.nextElementSibling.firstElementChild;  
              } else { // wrap around
                  target = parent.parentElement.firstElementChild.firstElementChild;
              }
              target.focus();
          }
          break;
        default:
      }
    }
  }

  /**
   * Instantiates the ui button that opens the menu.
   */
  initializeButton() {
    let template = document.createElement("template");
    let reader_text = "Open Navigation Menu"; //TODO: LOCALIZATION
    template.innerHTML = String.raw
    `<button class="decker-button" id="decker-menu-button" title="${reader_text}" aria-label="${reader_text}">
      <i class="fas fa-bars"></i>
    </button>`;

    let button = template.content.firstElementChild;
    button.addEventListener("click", (event) => this.toggleMenu(event));
    this.open_button = button;
  }

  /**
   * Instantiates the menu.
   */
  initializeSlideList() {
    let template = document.createElement("template");
    template.innerHTML = String.raw
    `<div class="slide-list-wrapper">
      <ul class="slide-list"></ul>
    </div>`
    let wrapper = template.content.firstElementChild;
    let list = wrapper.firstElementChild;
    let slides = document.querySelectorAll(".slides > section");
    slides.forEach((slide, h) => {
      var item = this.createListItem(slide, h);
      list.appendChild(item);
      let subslides = slide.querySelectorAll("section");
      if(subslides.length > 0) {
        subslides.forEach((subslide, v) => {
          let subitem = this.createListItem(slide, h, v);
          list.appendChild(subitem);
        });
      }
    });
    wrapper.addEventListener("keydown", (event) => this.ignoreTraversalKeys(event));
    this.menu.container.appendChild(wrapper);
    this.menu.slide_list = list;
  }

  /**
   * Creates a single list item for the slide list.
   * @param {*} slide 
   * @param {*} h 
   * @param {*} v 
   * @returns 
   */
  createListItem(slide, h, v) {
    let template = document.createElement("template");
    let title = this.getTitle(slide, "h1, h2, h3, h4, h5");
    title = `${h+1}.${v ? v : ""} ${title}`;
    template.innerHTML = String.raw
    `<li class="slide-list-item" data-slide-h="${h}" ${v ? "data-slide-v=\""+v+"\"" : ""}>
      <a class="slide-link" href="#/${h}/${v ? "/"+v : ""}" target="_self">${title}</a>
    </li>`
    let item = template.content.firstElementChild;
    let link = item.firstElementChild;
    link.addEventListener("click", (event) => this.toggleMenu(event));
    return item;
  }

  /**
   * Tries to retrieve the title of a slide by various means.
   * @param {*} section 
   * @param {*} selector 
   * @returns 
   */
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

  /**
   * Taken from Greg Denehy's Menu Plugin. Tries to find a title by going through
   * data-menu-title attribute of the given section, finding any element with the
   * class .menu-title or checking if there is an element with the custom selector
   * in the slide. If there is none, returns undefined.
   * @param {*} section 
   * @param {*} selector 
   * @returns The title of a slide or undefined.
   */
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

  /**
   * Taken from Greg Denehy's Menu Plugin.
   * Searches the content of a slide for any text and uses that as a title.
   * @param {*} section 
   * @returns 
   */
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

  /**
   * Instantiates the whole menu and adds it to the DOM.
   */
  initializeMenu() {
    let template = document.createElement("template");
    let animations = this.reveal.getConfig().fragments;
    let toggle_icon = animations ? "fa-check-circle" : "fa-circle";
    template.innerHTML = String.raw
    `<div class="decker-menu slide-in-left" id="decker-menu" inert>
      <div class="tile-grid">
        <button class="tile" id="decker-menu-search-button" title="Toggle Searchbar" aria-label="Toggle Searchbar">
          <i class="fas fa-search"></i>
          <p>Toggle Search Bar</p>
        </button>
        <button class="tile" id="decker-menu-print-button" title="Print PDF" aria-label="Print PDF">
          <i class="fas fa-print"></i>
          <p>Print as PDF</p>
        </button>
        <button class="switch tile" id="decker-menu-animation-button" title="Toggle fragmented slides" aria-label="Toggle fragmented slides">
          <i class="far ${toggle_icon}"></i>
          <p>Toggle Animations</p>
        </button>
        <button class="close tile" id="decker-menu-close-button" title="Close Menu" aria-label="Close Menu">
          <i class="fas fa-times"></i>
          <p>Close Menu</p>
        </button>
      </div>
     </div>`
    let container = template.content.firstElementChild;
    this.menu.container = container;

    /* Getting references */
    this.menu.search_button = container.querySelector("#decker-menu-search-button");
    this.menu.pdf_button = container.querySelector("#decker-menu-print-button");
    this.menu.fragments_button = container.querySelector("#decker-menu-animation-button");
    this.menu.close_button = container.querySelector("#decker-menu-close-button");

    /* Attach callbacks */
    this.menu.search_button.addEventListener("click", (event) => this.toggleSearchbar());
    this.menu.search_button.addEventListener("click", (event) => this.closeMenu(event));
    this.menu.pdf_button.addEventListener("click", (event) => this.printPDF());
    this.menu.pdf_button.addEventListener("click", (event) => this.closeMenu(event));
    this.menu.fragments_button.addEventListener("click", (event) => this.toggleFragments());
    this.menu.close_button.addEventListener("click", (event) => this.closeMenu(event));

    this.initializeSlideList();
    this.menu.container.addEventListener("keydown", (event) => this.traverseList(event));
  }

  init(reveal) {
    this.reveal = reveal;
    this.config = reveal.getConfig();

    this.initializeButton();
    this.initializeMenu();

    document.body.appendChild(this.menu.container);

    if(!this.reveal.hasPlugin('decker-plugins')) {
      console.log("no decker plugin manager loaded");
      return;
    }
    let manager = this.reveal.getPlugin("decker-plugins");
    manager.placeButton({button: this.open_button, position: this.position });
  }
}

let instance = new SlideMenu("TOP_LEFT");

export default instance;