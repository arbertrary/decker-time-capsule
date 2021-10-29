/**
 * Reveal Plugin that adds a black area to show a live transcription of the speaker's presentation.
 * 
 * The speech recognition currently only works in Google Chrome (and maybe Safari).
 * 
 * The black area will be maximised on the second screen if the window-placement api is available.
 * At the time of writing the window-placement api is still in its origin trial on chrome with no responses
 * of Mozilla or Apple to implement the api on their end.
 * 
 * TODO: If the window-placement api is unavaiable, open a secondary borderless window and request fullscreen.
 * 
 * @author Sebastian Lukas Hauer
 */

let menu_template = document.createElement("template");
menu_template.innerHTML = String.raw
`<div class="slide-in-left caption-options-menu" inert>
  <button class="toggle-button">
    <i class="fas fa-closed-captioning"></i>
    <span>Activate Live-Captioning</span>
  </button>
  <button class="close-button">
    <i class="fas fa-times"></i>
    <span>Close</span>
  </button>
</div>`

let button_template = document.createElement("template");
button_template.innerHTML = String.raw
`<awesome-button icon="fa-closed-captioning" icon-style="fas" label=""
title="Activate Live Captioning" aria-label="Activate Live Captioning">

</awesome-button>`

let caption_template = document.createElement("template");
caption_template.innerHTML = String.raw
`<div class="caption-area">
</div>`

var SpeechRecognition = SpeechRecognition || webkitSpeechRecognition
var SpeechGrammarList = SpeechGrammarList || webkitSpeechGrammarList
var SpeechRecognitionEvent = SpeechRecognitionEvent || webkitSpeechRecognitionEvent

/**
 * Plugin class to be registered so the activation-(decker-)button is at the right place.
 */
class LiveCaptioning {
  constructor() {
    this.reveal = undefined;
    this.id = "live-captioning";
    this.menu = {
      container: undefined,
      cc_button: undefined,
      close_button: undefined,
    };

    /* Attributes Decker Plugin Manager */
    this.decker_button = button_template.content.cloneNode(true).firstElementChild;
    this.decker_anchor = "TOP_LEFT";

    /* Filling Menu (currently unused) */
    this.menu.container = menu_template.content.cloneNode(true).firstElementChild;
    this.menu.cc_button = this.menu.container.querySelector(".toggle-button");
    this.menu.close_button = this.menu.container.querySelector(".close-button");

    /* Caption area and currently modified span */
    this.area = caption_template.content.cloneNode(true).firstElementChild;
    this.currentSpan = undefined;

//    this.decker_button.addEventListener("click", () => this.openMenu());
    this.decker_button.addEventListener("click", () => this.toggleCaptioning());
    this.menu.cc_button.addEventListener("click", () => this.toggleCaptioning());
    this.menu.close_button.addEventListener("click", () => this.closeMenu());

    this.captioning = false;
    this.speechRecog = undefined;

    this.fullscreenCaptioning = false;
    this.primaryScreen = undefined;
    this.secondaryScreen = undefined;
    this.popup = undefined;
  }

  openMenu() {
    this.menu.container.inert = false;
  }

  closeMenu() {
    this.menu.container.inert = true;
  }

  isCaptioning() {
    return this.captioning;
  }

  /**
   * Sets up the speech recognition of the browser and starts it. Also opens
   * a new window to present the recognized text in.
   * 
   * TODO: If the window-placement api is available it instead maximizes the caption area
   * on the secondary screen.
   */
  async startCaptioning() {
    this.speechRecog = new SpeechRecognition();
    this.speechRecog.continuous = true;
    this.speechRecog.interimResults = true;
    this.speechRecog.onstart = () => this.handleStart();
    this.speechRecog.onresult = (event) => this.handleResult(event);
    this.speechRecog.onerror = (event) => this.handleError(event);
    this.speechRecog.onend = () => this.handleEnd();
    this.speechRecog.start();
    if(this.fullscreenCaptioning) {
      this.area.classList.add("show");
      this.area.requestFullscreen({ screen: this.secondaryScreen });
    } else {
      this.popup = window.open("about:blank", "reveal.js - Captioning", "width=1920,height=1080");
      try {
        let response = await fetch("/support/plugins/live-captioning/live-captioning.html");
        let html = await response.text();
        this.popup.document.write(html);
      } catch (error) {
        console.log(error);
      }
      if( !this.popup ) {
        this.area.classList.add("show");
      }
    }
    this.captioning = true;
//    this.decker_button.classList.add("captioning");
    this.decker_button.toggle();
    this.decker_button.icon = "fa-circle";
    this.decker_button.title = "Deactivate Live Captioning";
    this.decker_button.setAttribute("aria-label", "Deactivate Live Captioning");
  }

  /**
   * Stops the captioning and closes the popup if available.
   */
  stopCaptioning() {
    this.captioning = false;
    
    this.speechRecog.stop();
    this.area.classList.remove("show");
    if(this.popup) {
      this.popup.close();
      this.popup = undefined;
    }
//    this.decker_button.classList.remove("captioning");
    this.decker_button.toggle();
    this.decker_button.icon = "fa-closed-captioning";
    this.decker_button.title = "Activate Live Captioning";
    this.decker_button.setAttribute("aria-label", "Activate Live Captioning");
  }

  toggleCaptioning() {
    if(!this.captioning) {
      this.startCaptioning();
    } else {
      this.stopCaptioning();
    }
  }

  handleStart() {
    //Right now no functionality
  }

  /**
   * Updates the current span if result is not final,
   * or finalizes the current one and creates a new one.
   * @param {*} event 
   */
  handleResult(event) {
    for(var i = event.resultIndex; i < event.results.length; i++) {
      if(event.results[i][0].confidence > 0.4) {
        this.updateCaptionContent(event.results[i][0].transcript);
        if(event.results[i].isFinal) {
          this.finalizeCaptionContent(event.results[i][0].transcript);
        }
      }
    }
  }

  /**
   * If an error occurs we just finalize the current span and log the error.
   * @param {*} event 
   */
  handleError(event) {
    console.log(event.type);
    this.finalizeCaptionContent(this.currentSpan?.innerHTML);
  }

  /**
   * Finalize the current span and restart the recognition if we still
   * want to caption.
   */
  handleEnd() {
    this.finalizeCaptionContent(this.currentSpan?.innerHTML);
    if(this.captioning) {
      this.speechRecog.start();
    }
  }

  /**
   * Updates the content of the current span and creates one if none exists.
   * 
   * Sends data to the popup if one exists instead.
   * @param {*} text 
   * @returns Nothing
   */
  updateCaptionContent(text) {
    if(!text) return;
    if(this.popup) {
      this.popup.postMessage( JSON.stringify( {
        type: "update",
        text: text,
      }));
      return;
    }
    if(!this.currentSpan) {
      this.currentSpan = document.createElement("span");
      this.currentSpan.classList.add("caption");
      this.area.appendChild(this.currentSpan);
    }
    this.currentSpan.textContent = text;
    this.area.scrollTo(0, this.area.scrollHeight);
  }

  /**
   * Updates the content of the current span and creates it if none exists.
   * Also deletes the reference to it so a new one gets created in the next round.
   * Sends data to the popup if one exists instead.
   * @param {*} text 
   * @returns Nothing
   */
  finalizeCaptionContent(text) {
    if(!text) return;
    if(this.popup) {
      this.popup.postMessage( JSON.stringify( {
        type: "final",
        text: text,
      }));
      return;
    }
    if(!this.currentSpan) {
      this.currentSpan = document.createElement("span");
      this.currentSpan.classList.add("caption");
      this.area.appendChild(currentSpan);
    }
    this.currentSpan.textContent = text + " ";
    this.currentSpan = undefined;
    this.area.scrollTo(0, this.area.scrollHeight);
  }

  /**
   * Reveal.js Plugin init function. Gets called by the framework.
   * @param {*} reveal 
   */
  init(reveal) {
    this.reveal = reveal;
    document.body.appendChild(this.menu.container);
    document.body.appendChild(this.area);
    let manager = this.reveal.getPlugin("decker-plugins");
    manager.registerPlugin(this);

    if('getScreens' in window) {
      navigator.permissions.query({ name: 'window-placement'})
        .then((state) => {
          if(state === 'granted' && window.screen.isExtended) {
            window.getScreens().then((data) => {
              this.primaryScreen = data.screens.filter((screen) => screen.isPrimary)[0];
              this.secondaryScreen = data.screens.filter((screen) => !screen.isPrimary)[0];
              this.fullscreenCaptioning = true;
            })
            .catch((error) => {
              console.log(error);
              this.area.classList.add("pinned");
            });
          } else {
            console.error("Did not get window-placement permission or there are no other screens.");
            this.area.classList.add("pinned");
          }
        })
        .catch ((error) => {
          console.error(error);
          this.area.classList.add("pinned");
        }
      );
    }
  }
}

let instance = new LiveCaptioning();

export default instance;