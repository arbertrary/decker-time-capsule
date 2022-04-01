/**
 * Reveal Plugin that adds a black area to show a live transcription of the speaker's presentation.
 *
 * The speech recognition currently only works in Google Chrome (and maybe Safari).
 *
 * @author Sebastian Lukas Hauer
 */
let localization;

let menu_template = document.createElement("template");
menu_template.innerHTML = String.raw`<div class="slide-in-left caption-options-menu" inert>
      <button class="toggle-button">
        <i class="fas fa-closed-captioning"></i>
        <span>Activate Live-Captioning</span>
      </button>
      <button class="close-button">
        <i class="fas fa-times"></i>
        <span>Close</span>
      </button>
    </div>`;

// Using the custom web component here is optional and can be replaced by something less
// bleeding edge
let button_template = document.createElement("template");
button_template.innerHTML = String.raw`<button is="awesome-button" class="fa-button" icon="fa-closed-captioning" icon-style="fas" title="Activate Live Captioning" aria-label="Activate Live Captioning">
   </button>`;

let caption_template = document.createElement("template");
caption_template.innerHTML = String.raw`<div class="caption-area">
    </div>`;

var SpeechRecognition = undefined;
var SpeechGrammarList = undefined;
var SpeechRecognitionEvent = undefined;

if (window.chrome) {
  SpeechRecognition = SpeechRecognition || webkitSpeechRecognition;
  SpeechGrammarList = SpeechGrammarList || webkitSpeechGrammarList;
  SpeechRecognitionEvent =
    SpeechRecognitionEvent || webkitSpeechRecognitionEvent;
}

/**
 * Plugin class to be registered so the activation-(decker-)button is at the right place.
 */
class LiveCaptioning {
  constructor() {
    this.reveal = undefined;
    this.id = "live-captioning";

    /* Attributes Decker Plugin Manager */
    this.record_button =
      button_template.content.cloneNode(true).firstElementChild;
    this.position = "TOP_RIGHT";

    this.record_button.addEventListener("click", () => this.toggleCaptioning());

    this.captioning = false;
    this.speechRecog = undefined;
    this.connection = undefined;

    /* Left for posterity if at some point the window-placement API is supported in browsers
     this.fullscreenCaptioning = false;
     this.primaryScreen = undefined;
     this.secondaryScreen = undefined;
     */
    this.popup = undefined;
  }

  isCaptioning() {
    return this.captioning;
  }

  async askPermission() {
    let options = [
      { text: localization.accept, value: "ACCEPT" },
      { text: localization.abort, value: "ABORT" },
    ];
    let choice = await window.showChoice(
      localization.caption_warning,
      options,
      "warning"
    );
    if (choice.submit !== "ACCEPT") {
      return false;
    } else {
      return true;
    }
  }

  /**
   * Sets up the speech recognition of the browser and starts it. Also opens
   * a new window to present the recognized text in.
   */
  async startCaptioning() {
    let permission = await this.askPermission();
    if (!permission) {
      return;
    }
    this.speechRecog = new SpeechRecognition();
    this.speechRecog.continuous = true;
    this.speechRecog.interimResults = true;
    this.speechRecog.onstart = () => this.handleStart();
    this.speechRecog.onresult = (event) => this.handleResult(event);
    this.speechRecog.onerror = (event) => this.handleError(event);
    this.speechRecog.onend = () => this.handleEnd();

    this.popup = window.open(
      "about:blank",
      "reveal.js - Captioning",
      "width=1920,height=1080"
    );
    try {
      let response = await fetch(
        "./support/plugins/live-captioning/live-captioning.html"
      );
      let html = await response.text();
      this.popup.document.write(html);
      this.popup.onbeforeunload = () => {
        this.popup = undefined;
        this.stopCaptioning();
      };
    } catch (error) {
      console.log(error);
    }

    let url = window.Decker.meta.caption_server;
    if (url) {
      const response = await fetch(url + "/api/new", {
        method: "POST",
      });
      let json = await response.json();
      if (json.session && json.token) {
        this.connection = json;
        if (!this.connection.server) {
          this.connection.server = url;
        }
        let noop = await window.showQRCode(
          localization.qrcode_message,
          `${this.connection.server}/${this.connection.session}`
        );
      }
    }

    this.speechRecog.start();
    this.captioning = true;
    this.record_button.icon = "fa-circle";
    this.record_button.classList.add("recording");
    this.record_button.title = localization.stop_captioning;
    this.record_button.setAttribute("aria-label", localization.stop_captioning);
  }

  /**
   * Stops the captioning and closes the popup if available.
   */
  stopCaptioning() {
    this.captioning = false;

    this.speechRecog.stop();
    if (this.popup) {
      this.popup.close();
      this.popup = undefined;
    }
    this.record_button.icon = "fa-closed-captioning";
    this.record_button.classList.remove("recording");
    this.record_button.title = localization.start_captioning;
    this.record_button.setAttribute(
      "aria-label",
      localization.start_captioning
    );
  }

  toggleCaptioning() {
    if (!this.captioning) {
      this.startCaptioning();
    } else {
      this.stopCaptioning();
    }
  }

  handleStart() {
    this.popup.postMessage(
      JSON.stringify({
        type: "status",
        value: "start",
      })
    );
  }

  /**
   * Updates the current span if result is not final,
   * or finalizes the current one and creates a new one.
   * @param {*} event
   */
  handleResult(event) {
    for (var i = event.resultIndex; i < event.results.length; i++) {
      if (event.results[i][0].confidence > 0.1) {
        this.updateCaptionContent(event.results[i][0].transcript);
        if (event.results[i].isFinal) {
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
    console.error(event);
    if (this.popup) {
      this.popup.postMessage(
        JSON.stringify({
          type: "status",
          value: "inactive",
        })
      );
    }
    this.finalizeCaptionContent(this.currentSpan?.innerHTML);
  }

  /**
   * Finalize the current span and restart the recognition if we still
   * want to caption.
   */
  handleEnd() {
    this.finalizeCaptionContent(this.currentSpan?.innerHTML);
    if (this.captioning) {
      this.speechRecog.start();
    }
  }

  /**
   * Sends data to the popup to update their current span and also informs
   * the connected syncronization service of the update.
   * @param {*} text
   * @returns Nothing
   */
  updateCaptionContent(text) {
    if (!text) return;
    if (this.connection) {
      try {
        fetch(
          `${this.connection.server}/api/${this.connection.session}/update`,
          {
            method: "POST",
            headers: {
              "Content-Type": "application/json",
            },
            body: JSON.stringify({ token: this.connection.token, text: text }),
          }
        );
      } catch (error) {
        console.error(error);
      }
    }
    if (this.popup) {
      this.popup.postMessage(
        JSON.stringify({
          type: "update",
          text: text,
        })
      );
      return;
    }
  }

  /**
   * Updates the content of the current span and creates it if none exists.
   * Also deletes the reference to it so a new one gets created in the next round.
   * Sends data to the popup and informs the syncronization service about the finalization.
   * @param {*} text
   * @returns Nothing
   */
  finalizeCaptionContent(text) {
    if (!text) return;
    if (this.connection) {
      try {
        fetch(
          `${this.connection.server}/api/${this.connection.session}/final`,
          {
            method: "POST",
            headers: {
              "Content-Type": "application/json",
            },
            body: JSON.stringify({ token: this.connection.token, text: text }),
          }
        );
      } catch (error) {
        console.error(error);
      }
    }
    if (this.popup) {
      this.popup.postMessage(
        JSON.stringify({
          type: "final",
          text: text,
        })
      );
      return;
    }
  }

  addCCButton() {
    let anchors = this.reveal.getPlugin("ui-anchors");
    anchors.placeButton(this.record_button, this.position);
  }

  removeCCButton() {
    this.record_button.parentElement.removeChild(this.record_button);
  }

  /**
   * Reveal.js Plugin init function. Gets called by the framework.
   * @param {*} reveal
   */
  init(reveal) {
    if (!SpeechRecognition) {
      console.error("SpeechRecognition not available in this browser.");
      return;
    }

    localization = {
      start_captioning: "Start Live Captioning",
      stop_captioning: "Stop Live Captioning",
      accept: "Accept",
      abort: "Abort",
      qrcode_message: "Live Captioning",
      caption_warning:
        "Using this feature will use your Browser's WebSpeech API to transcribe your voice. \
       To facilitate this, your voice will be sent to your Browser's manufacturer's Cloud Service \
       (Google or Apple). Do you accept this?",
    };
    let lang = navigator.language;
    if (lang === "de") {
      localization = {
        start_captioning: "Live-Untertitelung aktivieren",
        stop_captioning: "Live-Untertitelung stoppen",
        accept: "Akzeptieren",
        abort: "Abbrechen",
        qrcode_message: "Live-Untertitel",
        caption_warning:
          "Diese Funktion wird die eingebaute WebSpeech API Ihres Browsers benutzen, \
       um Ihre Stimme zu transkribieren. Die dabei aufgezeichneten Daten werden dazu an den Hersteller \
       Ihres Browsers gesendet. Sind Sie damit einverstanden?",
      };
    }

    this.reveal = reveal;

    /* Leave this for later generations here, if the window-placement API ever gets made a fully supported
      * feature in all browsers ...
     if ("getScreens" in window) {
       navigator.permissions
         .query({ name: "window-placement" })
         .then((state) => {
           if (state === "granted" && window.screen.isExtended) {
             window
               .getScreens()
               .then((data) => {
                 this.primaryScreen = data.screens.filter(
                   (screen) => screen.isPrimary
                 )[0];
                 this.secondaryScreen = data.screens.filter(
                   (screen) => !screen.isPrimary
                 )[0];
                 this.fullscreenCaptioning = true;
               })
               .catch((error) => {
                 console.log(error);
               });
           } else {
             console.error(
               "Did not get window-placement permission or there are no other screens."
             );
           }
         })
         .catch((error) => {
           console.error(error);
         });
     }
     */

    reveal.addEventListener("ready", () => {
      Decker.addPresenterModeListener((on) => {
        if (on) {
          this.addCCButton();
        } else {
          this.removeCCButton();
        }
      });
    });
  }
}

let instance = new LiveCaptioning();

export default instance;
