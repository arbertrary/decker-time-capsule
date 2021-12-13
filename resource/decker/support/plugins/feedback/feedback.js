<<<<<<< HEAD
/**
 * Plugin that adds a menu for viewers of the slides to give feedback and ask
 * questions.
 * 
 * @author Henrik Tramberend
 * @author Sebastian Hauer (rewrite)
 */

 class Feedback {
  timeout = 500;

  id = "feedback";

  reveal = undefined;
  config = undefined;

  engine = {
    api: undefined,
    deckId: undefined,
    token: undefined
  };

  open_button = undefined;
  button_badge = undefined;

  position = undefined;

  menu = {
    container: undefined,
    badge: undefined,
    token_input: undefined,
    token_lock: undefined,
    close_button: undefined,
    feedback_list: undefined,
    feedback_input: undefined,
    feedback_login_area: undefined,
    feedback_login_button: undefined,
    feedback_credentials: {
      container: undefined,
      username_input: undefined,
      password_input: undefined,
    }
  }

  constructor(position) {
    this.position = position;
  }

  /**
   * Returns only the very basic url of the current window.
   */
  get deckURL() {
    let url = new URL(window.location);
    url.hash = "";
    url.query = "";
    url.username = "";
    url.password = "";
    return url.toString;
  }

  /**
   * Tries to establish a connection to the engine by downloading the implementation
   * from the given base. The deckId is a unique identifier that identifies this
   * deck and can be set in either the deck.yaml or the markdown of the deck.
   * @param {*} base A URL.
   * @param {*} deckId A unique id from deck.yaml or the deck markdown.
   */
  contactEngine(base, deckId) {
    this.engine.deckId = deckId || this.deckURL;
    import(base + "/decker-util.js")
    .then((util) => {
      console.log("Decker engine contacted at: ", base);
      this.engine.api = util.buildApi(base);
      this.prepareEngine();
    }).catch((e) => {
      console.log("Can't contact decker engine: " + e);
    });
  }

  /**
   * Sets up the engine fetched previously by contactEngine.
   */
  prepareEngine() {
    this.engine.api
      .getToken()
      .then((token) => {
        // Globally set the server token.
        this.engine.token = token;
  
        // Build the panel, once Reval is ready.
        if (this.reveal.isReady()) {
          this.createInterface();
        } else {
          this.reveal.addEventListener("ready", (_) => {
            this.createInterface();
          });
        }
      })
      .catch((e) => {
        // Nothing goes without a token
        console.log("API function getToken() failed: " + e);
        throw e;
      });
  }

  /**
   * Turns the menu on or off, based on its current state.
   */
  toggleMenu() {
    if(this.menu.container.inert) this.openMenu();
    else this.closeMenu();
  }

  /**
   * Opens the menu and updates its content. Also focuses the first button in the menu.
   */
  openMenu() {
    this.menu.container.inert = false;
    this.open_button.classList.add("checked");
    this.menu.container.classList.add("open");
    this.menu.token_lock.focus();
    this.requestMenuContent();
    localStorage.setItem("feedback-state", "open");
  }

  /**
   * Closes the menu and focuses the button that opened it.
   */
  closeMenu() {
    this.menu.container.inert = true;
    this.open_button.classList.remove("checked");
    this.menu.container.classList.remove("open");
    this.open_button.focus();
    localStorage.removeItem("feedback-state");
  }

  /**
   * Disables or enables the token_input field.
   */
  toggleTokenInput() {
    if(this.menu.token_lock.classList.contains("checked")) {
      this.unlockTokenInput();
    } else {
      if(this.menu.token_input.value) { //disallow empty token
        this.lockTokenInput();
      }
    }
  }

  /**
   * Disables the token input field and stores its value in the local storage.
   */
  lockTokenInput() {
    this.menu.token_input.setAttribute("disabled", true);
    this.menu.token_input.type = "password";
    this.menu.token_lock.classList.add("checked");
    window.localStorage.setItem("feedback-user-token", this.menu.token_input.value);
  }

  /**
   * Enables input on the token input field and delets the token from the local
   * storage until it is locked again.
   */
  unlockTokenInput() {
    this.menu.token_input.setAttribute("disabled", false);
    this.menu.token_input.type = "text";
    this.menu.token_input.classList.remove("hidden");
    this.menu.token_lock.classList.remove("checked");
    window.localStorage.removeItem("feedback-user-token");
  }

  /**
   * Completely hides the token input if. Called if you are the admin.
   */
  hideTokenInput() {
    this.lockTokenInput();
    this.menu.token_input.classList.add("hidden");
    this.menu.token_lock.classList.add("hidden");
  }

  /**
   * Turns on or off the login credentials area in the footer.
   */
  toggleLoginArea() {
    if(this.menu.feedback_login_area.classList.contains("admin")) {
      this.engine.token.admin = null;
      this.menu.feedback_credentials.username_input.value = "";
      this.menu.feedback_credentials.password_input.value = "";
      this.menu.feedback_login_area.classList.remove("admin");
      this.menu.feedback_credentials.container.classList.remove("visible");
      this.requestMenuContent();
    } else {
      if(this.menu.feedback_credentials.container.classList.contains("visible")) {
        this.menu.feedback_credentials.container.classList.remove("visible");
      } else {
        this.menu.feedback_credentials.container.classList.add("visible");
        this.menu.feedback_credentials.username_input.focus();
      }
    }
  }

  /**
   * Tries to perfom a login with the entered credentials.
   */
  sendLogin(event) {
    if(event.key === "Enter"){
      let credentials = {
        login: this.menu.feedback_credentials.username_input.value,
        password: this.menu.feedback_credentials.password_input.value,
        deck: this.engine.deckId
      };
      this.engine.api.getLogin(credentials).then((token) => {
        this.engine.token.admin = token.admin;
        this.menu.feedback_login_area.classList.add("admin");
        this.menu.feedback_credentials.username_input.value = "";
        this.menu.feedback_credentials.password_input.value = "";
        this.menu.feedback_credentials.container.classList.remove("visible");
        this.requestMenuContent();
      }).catch((error) => {
        console.error(error);
        this.menu.feedback_credentials.password_input.value = "";
      })
    }
  }

  /**
   * Publishes a comment to the engine.
   * @param {*} event 
   */
  sendComment(event) {
    if(event.key === "Enter" && event.shiftKey) {
      let slideId = this.reveal.getCurrentSlide().id;
      if(this.menu.feedback_input.hasAttribute("answer")) {
        this.engine.api.postAnswer(this.menu.feedback_input.commentId, this.engine.token.admin, this.menu.feedback_input.value, null)
        .then(() => this.clearTextArea())
        .then(() => this.requestMenuContent())
        .then(() => this.requestSlideMenuUpdate())
        .catch(console.log);
      } else {
        this.engine.api.submitComment(
          this.engine.deckId,
          slideId,
          this.engine.token.admin || this.menu.token_input.value,
          this.menu.feedback_input.value,
          this.menu.feedback_input.commentId,
          window.location.toString()
        )
        .then(() => this.clearTextArea())
        .then(() => this.requestMenuContent())
        .then(() => this.requestSlideMenuUpdate())
        .catch(console.log);
      }
      event.stopPropagation();
      event.preventDefault();
    }
  }

  /**
   * Clears the text area and resets its state.
   */
  clearTextArea() {
    this.menu.feedback_input.value = "";
    this.menu.feedback_input.placeholder = "Type question, ⇧⏎ (Shift-Return) to enter. Use Markdown for formatting.";
    this.menu.feedback_input.commentId = null;
    this.menu.feedback_input.removeAttribute("answer");
  }

  /**
   * Updates the badges of the open-button and the menu header.
   * @param {*} value 
   * @param {*} answered 
   */
  updateBadges(value, answered) {
    this.button_badge.textContent = value;
    this.button_badge.setAttribute("data-count", value);
    this.menu.badge.textContent = value;
    this.menu.badge.setAttribute("data-count", value);
    if(answered) {
      this.button_badge.classList.add("answered");
      this.menu.badge.classList.add("answered");
    } else {
      this.button_badge.classList.remove("answered");
      this.menu.badge.classList.remove("answered");
    }
  }

  /**
   * Simply removes all visible questions (from the UI, not the engine)
   */
  clearQuestionList() {
    while(this.menu.feedback_list.firstChild) {
      this.menu.feedback_list.removeChild(this.menu.feedback_list.lastChild);
    }
  }

  /**
   * Sends an async request to the engine to get the questions of the current slide.
   */
  requestMenuContent() {
    let slideId = this.reveal.getCurrentSlide().id;
    this.engine.api
      .getComments(this.engine.deckId, slideId, this.engine.token.admin || this.menu.token_input.value)
      .then((list) => this.updateMenuContent(list))
      .catch(console.log);
  }

  /**
   * Prepares the text area to answer a question.
   * @param {*} comment 
   */
  answerQuestion(comment) {
    let text = {
      placeholder: "Type answer, ⇧⏎ (Shift-Return) to enter. Use Markdown for formatting.",
    }
    this.menu.feedback_input.value = "";
    this.menu.feedback_input.commentId = comment.id;
    this.menu.feedback_input.setAttribute("answer", true);
    this.menu.feedback_input.placeholder = text.placeholder;
    this.menu.feedback_input.focus();
  }

  /**
   * Prepares the text area to edit a question.
   * @param {*} comment 
   */
  editQuestion(comment) {
    this.menu.feedback_input.value = comment.markdown;
    this.menu.feedback_input.commentId = comment.id;
    this.menu.feedback_input.removeAttribute("answer");
    this.menu.feedback_input.placeholder =
      "Type question, ⇧⏎ (Shift-Return) to enter. Use Markdown for formatting.";
      this.menu.feedback_input.focus();
  }

  /**
   * Deletes the passed comment.
   * @param {*} comment 
   */
  deleteQuestion(comment) {
    this.engine.api.deleteComment(comment.id, this.engine.token.admin || this.menu.token_input.value)
      .then(() => this.requestMenuContent())
      .then(() => this.requestSlideMenuUpdate());
  }

  /**
   * Deletes all answers to a comment.
   * @param {*} comment 
   */
  resetAnswers(comment) {
    let chain = Promise.resolve();
    for(let answer of comment.answers) {
      chain.then(() => this.engine.api.deleteAnswer(answer.id, this.engine.token.admin));
    }
    chain.then(() => this.requestMenuContent());
  }

  /**
   * Deletes the passed answer.
   * @param {*} answer 
   */
  deleteAnswer(answer) {
    this.engine.api.deleteAnswer(answer.id, this.engine.token.admin)
      .then(() => this.requestMenuContent());
  }

  /**
   * Marks a question as answered (does not need to have a written answer)
   * @param {*} comment 
   */
  markQuestion(comment) {
    this.engine.api.postAnswer(comment.id, this.engine.token.admin)
      .then(() => this.requestMenuContent());
  }

  /**
   * Gives the comment a +1 or revokes it if this user has already done so.
   * @param {*} comment 
   */
  voteComment(comment) {
    let vote = {
      comment: comment.id,
      voter: this.menu.token_input.value,
    };
    this.engine.api.voteComment(vote).then(() => this.requestMenuContent());
  }

  /**
   * Creates a question list item that represents a question.
   * @param {*} comment 
   * @returns 
   */
  createQuestionContainer(comment) {
    let text = {
      upvote: "Up-vote question",
      downvote: "Down-vote question",
      edit: "Edit question",
      delete: "Delete question",
      add: "Add answer",
      mark: "Mark as answered",
      reset: "Mark as not answered",
      answered: "Question has been answered",
      notanswered: "Question has not been answered",
    }

    let isAdmin = this.engine.token.admin != null;
    let isAuthor = comment.author === this.menu.token_input.value;
    let isDeletable = isAdmin || (isAuthor) && (comment.answers.length == 0);
    let isAnswered = comment.answers && comment.answers.length > 0;

    let template = document.createElement("template");
    template.innerHTML = String.raw
`<div class="feedback-item">
  <div class="feedback-content">
    <div class="feedback-controls">
      <span class="votes">${comment.votes > 0 ? comment.votes : ""}</span>
      <button class="${comment.didvote ? "fas" : "far"} fa-thumbs-up vote ${!isAuthor ? "canvote" : "cantvote"} ${comment.didvote ? "didvote" : ""}"
        title="${comment.didvote ? text.downvote : text.upvote}"
        aria-label="${comment.didvote ? text.downvote : text.upvote}">
      </button>
      ${isDeletable ? `<button class="fas fa-edit feedback-edit-question-button" title="${text.edit}" aria-label="${text.edit}"></button>` : ""}
      ${isDeletable ? `<button class="fas fa-trash-alt feedback-delete-question-button" title=${text.delete} aria-label="${text.delete}"></button>` : ""}
      ${isAdmin ? `<button class="far fa-plus-square feedback-answer-question-button" title="${text.add}" aria-label="${text.add}">` : ""}
      ${isAnswered ? 
      `<button class="far fa-check-circle answered feedback-reset-answers-button" title="${isDeletable ? text.reset : text.answered}" ${!isDeletable ? "disabled" : ""}></button>`
        :
      `<button class="far fa-circle notanswered feedback-mark-answered-button" title="${isDeletable ? text.mark : text.notanswered}" ${!isDeletable ? "disabled" : ""}></button>`}
    </div>
    ${comment.html}
  </div>
</div>`
    let question = template.content.firstElementChild;
    if(!isAuthor) {
      let voteButton = question.querySelector(".vote");
      voteButton.addEventListener("click", (event) => this.voteComment(comment));
    }
    if(isDeletable) {
      let editButton = question.querySelector(".feedback-edit-question-button");
      editButton.addEventListener("click", (event) => this.editQuestion(comment));
      let deleteButton = question.querySelector(".feedback-delete-question-button");
      deleteButton.addEventListener("click", (event) => this.deleteQuestion(comment));
      if(isAnswered && isAdmin) {
        let resetButton = question.querySelector(".feedback-reset-answers-button");
        resetButton.addEventListener("click", (event) => this.resetAnswers(comment));
      }
      if(!isAnswered && isAdmin) {
        let answerButton = question.querySelector(".feedback-mark-answered-button");
        answerButton.addEventListener("click", (event) => this.markQuestion(comment));
      }
    }
    if(isAdmin) {
      let addButton = question.querySelector(".feedback-answer-question-button");
      addButton.addEventListener("click", (event) => this.answerQuestion(comment));
    }
    MathJax.typeset([question]);
    return question;
  }

  /**
   * Creates a question list item that represents an answer.
   * @param {*} answer 
   * @returns 
   */
  createAnswerContainer(answer) {
    let isAdmin = this.engine.token.admin != null;
    let text = {
      delete: "Delete answer.",
    };
    let url = answer.link ? new URL(answer.link) : undefined;
    let html = answer.html ? answer.html : undefined;
    let template = document.createElement("template");
    template.innerHTML = String.raw
`<div class="feedback-item answer">
    <div class="feedback-content">
    ${isAdmin ? `<div class="feedback-controls"><button class="fas fa-trash-alt feedback-delete-answer-button" \
    title="${text.delete}" aria-label="${text.delete}"></div>` : ""}
    ${url ? `<div class="link"><a href="${url}" target="_blank"><i class="fas fa-external-link-alt"></i></a></div>` : ""}
    ${html ? `<div class="description">${html}</div>` : ""}
</div>`
    let item = template.content.firstChild;
    if(isAdmin) {
      let deleteButton = item.querySelector(".feedback-delete-answer-button");
      deleteButton.addEventListener("click", () => this.deleteAnswer(answer));
    }
    MathJax.typeset([item]);
    return item;
  }

  /**
   * Updates the question menu content with the question list.
   * @param {*} list 
   */
  updateMenuContent(list) {
    let allAnswered = true;
    for (let comment of list) {
      const isAnswered = comment.answers && comment.answers.length > 0;
      if (!isAnswered) {
        allAnswered = false;
        break;
      }
    }

    this.updateBadges(list.length, allAnswered);
    
    this.clearQuestionList();

    // re-fill question container
    for (let comment of list) {
      let item = this.createQuestionContainer(comment);
      this.menu.feedback_list.appendChild(item);
      for (let answer of comment.answers) {
        let block = this.createAnswerContainer(answer);
        this.menu.feedback_list.appendChild(block);
      }
    }

    this.menu.feedback_list.scrollTop = 0;
  }

  /**
   * Initializes the value of the token_input field based on stored values or
   * a random value if none stored was found.
   */
  initializeUsertoken() {
    let localToken = window.localStorage.getItem("feedback-user-token");
    if(this.engine && this.engine.token && this.engine.token.authorized) {
      this.menu.token_input.value = this.engine.token.authorized;
      this.menu.container.classList.add("authorized");
      this.menu.login_panel.classList.add("admin");
      this.hideTokenInput();
    } else if(localToken) {
      this.menu.token_input.value = localToken;
      this.lockTokenInput();
    } else {
      this.menu.token_input.value = this.engine.token.random;
      this.lockTokenInput();
    }
  }

  /**
   * Requests the api to send a list of questions to update the main slide menu.
   */
  requestSlideMenuUpdate() {
    this.engine.api
    .getComments(this.engine.deckId)
    .then((list) => this.updateSlideMenu(list))
    .catch(console.log);
  }

  /**
   * Counts the questions for each slide and adds a badge in the slide menu.
   * Now talks to the slide menu via its plugin instead of using querySelectors.
   * @param {*} list 
   */
  updateSlideMenu(list) {
    let menu_plugin = this.reveal.getPlugin("decker-menu");
    let slide_list = menu_plugin.getSlideList();
    for(let item of slide_list) {
      item.removeAttribute("data-questions");
      item.removeAttribute("data-answered");
    }
    for(let comment of list) {
      const slideID = comment.slide;
      const slide = document.getElementById(slideID);
      if(slide) {
        const indices = this.reveal.getIndices(slide);
        let item = undefined;
        if(indices.v) {
          item = menu_plugin.getListItem(indices.h, indices.v);
        } else {
          item = menu_plugin.getListItem(indices.h);
        }
        if(item) {
          let questions = item.hasAttribute("data-questions")
            ? parseInt(item.getAttribute("data-questions"))
            : 0;
          let answered = item.hasAttribute("data-answered")
            ? item.getAttribute("data-answered") === "true"
            : true;
  
          questions = questions + 1;
          answered = answered && comment.answers.length > 0;
  
          item.setAttribute("data-questions", questions);
          item.setAttribute("data-answered", answered);
        }
      } else { //Major error: Slide not found
        console.warn("Could not find slide with ID: "+ slideID);
      }
    }
  }

  /**
   * Sets up the whole interface: Button and the right hidden question menu.
   */
  createInterface() {
    let button_string = String.raw
    `<button class="open-button decker-button" title="Open Feedback Menu" aria-label="Open Feedback Menu">
      <i class="fas fa-question-circle"></i>
      <div class="open-badge badge"></div>
    </button>`
  
    let menu_string = String.raw
    `<div class="feedback-menu" inert>
      <div class="feedback-header">
        <div class="counter badge">0</div>
        <div class="feedback-title">Questions</div>
        <input class="feedback-token-input" type="password" placeholder="User Token" disabled="true"></input>
        <button class="feedback-lock" title="Store user token (session)">
          <i class="fas fa-lock lock" title="Unlock Token"></i>
          <i class="fas fa-unlock unlock" title="Lock Token"></i>
        </button>
        <button class="feedback-close" title="Close Feedback Menu">
          <i class="fas fa-times-circle"></i>
        </button>
      </div>
      <div class="feedback-list"></div>
      <div class="feedback-question-input">
        <textarea wrap="hard" placeholder="Type question, ⇧⏎ (Shift-Return) to enter. Use Markdown for formatting." tabindex="0"></textarea> 
      </div>
      <div class="feedback-footer">
        <div class="feedback-login">
          <button id="feedback-login-button" class="fas fa-sign-in-alt" title="Login as admin"></button>
        </div>
        <div class="feedback-credentials">
          <input id="feedback-username" placeholder="Login">
          <input id="feedback-password" placeholder="Password" type="password">
        </div>
      </div>
    </div>`
  
    let button_template = document.createElement("template");
    let menu_template = document.createElement("template");
    button_template.innerHTML = button_string;
    menu_template.innerHTML = menu_string;
    let button = button_template.content.firstChild;
    let menu = menu_template.content.firstChild;
  
    /* Setup references */

    this.open_button = button;
    this.menu.container = menu;
  
    this.button_badge = button.querySelector(".badge");

    this.menu.feedback_input = menu.querySelector(".feedback-question-input textarea");
    this.menu.badge = menu.querySelector(".counter");
    this.menu.feedback_list = menu.querySelector(".feedback-list");
    this.menu.token_input = menu.querySelector(".feedback-token-input");
    this.menu.token_lock = menu.querySelector(".feedback-lock");
    this.menu.close_button = menu.querySelector(".feedback-close");
    this.menu.feedback_login_area = menu.querySelector(".feedback-login");
    this.menu.feedback_login_button = menu.querySelector("#feedback-login-button");
    this.menu.feedback_credentials.container = menu.querySelector(".feedback-credentials");
    this.menu.feedback_credentials.username_input = menu.querySelector("#feedback-username");
    this.menu.feedback_credentials.password_input = menu.querySelector("#feedback-password");

    /* Add EventListeners */

    this.open_button.addEventListener("click", () => this.openMenu());

    this.menu.feedback_input.addEventListener("keypress", (e) => e.stopPropagation());
    this.menu.close_button.addEventListener("click", (event) => this.closeMenu());
    this.menu.feedback_login_button.addEventListener("click", (event) => this.toggleLoginArea());
    this.menu.token_lock.addEventListener("click", (event) => this.toggleTokenInput());
    this.menu.feedback_input.addEventListener("keydown", (event) => this.sendComment(event));

    this.menu.feedback_credentials.password_input.addEventListener("keydown", (event) => this.sendLogin(event));

    this.reveal.addEventListener("slidechanged", () => this.requestMenuContent());
    this.reveal.addEventListener("slidechanged", () => this.requestSlideMenuUpdate());

    /* Place Button in UI */

    if(this.reveal.hasPlugin("decker-plugins")) {
      let manager = this.reveal.getPlugin("decker-plugins");
      manager.placeButton( this.open_button, this.position );
    }
    let reveal_element = document.querySelector(".reveal");
    reveal_element.appendChild(this.menu.container);

    /* Finish setup before presentation */

    this.initializeUsertoken();

    this.requestMenuContent();
    this.requestSlideMenuUpdate();
  }

  /**
   * Reveal.js Plugin init.
   * @param {*} reveal 
   */
  init(reveal) {
    this.reveal = reveal;
    this.config = reveal.getConfig().feedback;
    let url = this.config?.server || this.config?.["base-url"];
    let id = this.config?.deckID || this.config?.["deck-id"];
    console.log("feedback", url, id);
    if(url) this.contactEngine(url, id);
  }
}

let instance = new Feedback("TOP_RIGHT");

export default instance;
=======
// reference to Reveal deck
let Reveal;

// Start with a 0.5 s retry interval. Back off exponentially.
let timeout = 500;

let engine = {
  api: undefined,
  deckId: undefined, // The unique deck identifier.
  token: undefined,
};

// Contacts the engine API at base.
function contactEngine(base, deckId) {
  engine.deckId = deckId || deckUrl();

  // Try to import the API utility module. We need to do this dynamically
  // because the URL is constructed from configuration data.
  import(base + "/decker-util.js")
    .then((util) => {
      console.log("Decker engine contacted at: ", base);
      engine.api = util.buildApi(base);
      prepareEngine();
    })
    .catch((e) => {
      console.log("Can't contact decker engine:" + e);
      // console.log("Retrying ..." + e);
      // setTimeout(() => contactEngine(base, deckId), (timeout *= 2));
    });
}

// Strips the document URI from everything that can not be part of the deck id.
function deckUrl() {
  let url = new URL(window.location);
  url.hash = "";
  url.query = "";
  url.username = "";
  url.password = "";
  return url.toString();
}

// Prepares the questions panel for operation.
function prepareEngine() {
  engine.api
    .getToken()
    .then((token) => {
      // Globally set the server token.
      engine.token = token;

      // Build the panel, once Reval is ready.
      if (Reveal.isReady()) {
        buildInterface();
      } else {
        Reveal.addEventListener("ready", (_) => {
          buildInterface();
        });
      }
    })
    .catch((e) => {
      // Nothing goes without a token
      console.log("API function getToken() failed: " + e);
      throw e;
    });
}

// Builds the panel and sets up event handlers.
function buildInterface() {
  let open = document.createElement("div");
  let badge = document.createElement("div");

  let panel = document.createElement("div");
  let header = document.createElement("div");
  let title = document.createElement("div");
  let counter = document.createElement("div");
  let user = document.createElement("input");
  let check = document.createElement("button");
  let close = document.createElement("button");
  let container = document.createElement("div");
  let input = document.createElement("div");
  let text = document.createElement("textarea");
  let footer = document.createElement("div");
  let login = document.createElement("div");
  let credentials = document.createElement("div");
  let username = document.createElement("input");
  let password = document.createElement("input");

  let cross = document.createElement("i");
  cross.classList.add("fas", "fa-times-circle");
  cross.setAttribute("title", "Close panel");

  let lock = document.createElement("i");
  lock.classList.add("fas", "fa-lock", "lock");
  lock.setAttribute("title", "Lock user ");

  let unlock = document.createElement("i");
  unlock.classList.add("fas", "fa-unlock", "unlock");
  unlock.setAttribute("title", "User token is locked");

  let gear = document.createElement("i");
  gear.classList.add("fas", "fa-cog", "gears");
  gear.setAttribute("title", "Login as admin");

  let signin = document.createElement("i");
  signin.classList.add("fas", "fa-sign-in-alt", "gears");
  signin.setAttribute("title", "Login as admin");

  let signout = document.createElement("i");
  signout.classList.add("fas", "fa-sign-out-alt", "gears");
  signout.setAttribute("title", "Logout admin");

  let qmark = document.createElement("i");
  qmark.classList.add("fas", "fa-question-circle");

  panel.classList.add("q-panel");
  open.appendChild(qmark);
  open.appendChild(badge);
  open.classList.add("open-button");
  open.setAttribute("title", "Open questions panel");
  badge.classList.add("open-badge", "badge");

  header.classList.add("q-header");
  title.textContent = "Questions";
  title.classList.add("q-title");
  counter.textContent = "0";
  counter.classList.add("counter", "badge");
  user.setAttribute("type", "text");
  user.setAttribute("placeholder", "Enter user token");
  check.setAttribute("title", "Store user token (session)");
  check.classList.add("q-check");
  check.appendChild(lock);
  check.appendChild(unlock);
  header.appendChild(counter);
  header.appendChild(title);
  header.appendChild(user);
  header.appendChild(check);
  header.appendChild(close);
  close.classList.add("q-close");
  close.appendChild(cross);

  container.classList.add("q-list");

  input.classList.add("q-input");
  input.appendChild(text);
  text.setAttribute("wrap", "hard");
  text.placeholder =
    "Type question, ⇧⏎ (Shift-Return) to enter. Use Markdown for formatting.";

  // prevent propagating keypress up to Reveal, since otherwise '?'
  // triggers the help dialog.
  text.addEventListener("keypress", (e) => {
    e.stopPropagation();
  });

  footer.classList.add("q-footer");
  username.setAttribute("placeholder", "Login");
  password.setAttribute("placeholder", "Password");
  password.type = "password";

  login.appendChild(signin);
  login.classList.add("q-login");

  footer.appendChild(login);
  footer.appendChild(credentials);
  credentials.appendChild(username);
  credentials.appendChild(password);
  credentials.classList.add("credentials");

  panel.appendChild(header);
  panel.appendChild(container);
  panel.appendChild(input);
  panel.appendChild(footer);

  document.body.appendChild(open);
  document.body.appendChild(panel);

  function initUser() {
    let localToken = window.localStorage.getItem("token");
    if (engine.token && engine.token.authorized) {
      // Some higher power has authorized this user. Lock token in.
      user.value = engine.token.authorized;
      user.setAttribute("disabled", true);
      check.classList.add("checked");
      user.type = "password";
      check.classList.add("hidden");
      user.classList.add("hidden");
      panel.classList.add("authorized");
      login.classList.add("admin");
    } else if (localToken) {
      user.value = localToken;
      user.setAttribute("disabled", true);
      check.classList.add("checked");
      user.type = "password";
    } else {
      user.value = engine.token.random;
      user.removeAttribute("disabled");
      check.classList.remove("checked");
      user.type = "text";
    }
  }

  function updateComments() {
    let slideId = Reveal.getCurrentSlide().id;
    engine.api
      .getComments(engine.deckId, slideId, engine.token.admin || user.value)
      .then(renderList)
      .catch(console.log);
  }

  function renderSubmit() {
    updateCommentsAndMenu();
    text.value = "";
    text.commentId = null;
    text.removeAttribute("answer");
    text.placeholder =
      "Type question, ⇧⏎ (Shift-Return) to enter. Use Markdown for formatting.";
  }

  // given the list of questions, update question counter of menu items
  function updateMenuItems(list) {
    document.querySelectorAll("ul.slide-list > li.slide-list-item").forEach((li) => {
        li.removeAttribute("data-questions");
        li.removeAttribute("data-answered");
      });

    for (let comment of list) {
      // get slide info
      const slideID = comment.slide;
      const slide = document.getElementById(slideID);
      if (slide) {
        const indices = Reveal.getIndices(slide);

        // build query string, get menu item
        let query = ".slide-list-item";
        if (indices.h) query += '[data-slide-h="' + indices.h + '"]';
        if (indices.v) query += '[data-slide-v="' + indices.v + '"]';
        let li = document.querySelector(query);

        // update question counter
        if (li) {
          let questions = li.hasAttribute("data-questions")
            ? parseInt(li.getAttribute("data-questions"))
            : 0;
          let answered = li.hasAttribute("data-answered")
            ? li.getAttribute("data-answered") === "true"
            : true;

          questions = questions + 1;
          answered = answered && comment.answers.length > 0;

          li.setAttribute("data-questions", questions);
          li.setAttribute("data-answered", answered);
        }
      } else {
        // slide not found. should not happen. user probably used wrong (duplicate) deckID.
        console.warn("Could not find slide " + slideID);
      }
    }
  }

  // query list of questions, then update menu items
  function updateMenu() {
    engine.api
      .getComments(engine.deckId)
      .then(updateMenuItems)
      .catch(console.log);
  }

  function updateCommentsAndMenu() {
    updateComments();
    updateMenu();
  }

  function isAdmin() {
    return engine.token.admin !== null;
  }

  function isAuthor(comment) {
    return comment.author === user.value;
  }

  function canDelete(comment) {
    return isAdmin() || (isAuthor(comment) && comment.answers.length == 0);
  }

  function renderList(list) {
    // have all questions been answered?
    let allAnswered = true;
    for (let comment of list) {
      const isAnswered = comment.answers && comment.answers.length > 0;
      if (!isAnswered) {
        allAnswered = false;
        break;
      }
    }

    // counter badge
    counter.textContent = list.length;
    counter.setAttribute("data-count", list.length);
    badge.textContent = list.length;
    badge.setAttribute("data-count", list.length);
    if (allAnswered) {
      counter.classList.add("answered");
      badge.classList.add("answered");
    } else {
      counter.classList.remove("answered");
      badge.classList.remove("answered");
    }

    // clear question container
    while (container.firstChild) {
      container.removeChild(container.lastChild);
    }

    // re-fill question container
    for (let comment of list) {
      // create question item
      let item = document.createElement("div");
      item.classList.add("item");

      // question content
      let content = document.createElement("div");
      content.classList.add("content");
      content.innerHTML = comment.html;
      item.appendChild(content);

      // question controls
      let box = document.createElement("div");
      box.classList.add("controls");
      content.insertBefore(box, content.firstChild);

      // Number of upvotes
      let votes = document.createElement("span");
      votes.textContent = comment.votes > 0 ? comment.votes : "";
      votes.classList.add("votes");
      box.appendChild(votes);

      // Upvote button
      let vote = document.createElement("button");
      if (comment.didvote) {
        vote.className = "fas fa-thumbs-up";
        vote.title = "Down-vote question";
      } else {
        vote.className = "far fa-thumbs-up";
        vote.title = "Up-vote question";
      }
      vote.classList.add("vote");
      if (!isAuthor(comment)) {
        vote.classList.add("canvote");
        if (comment.didvote) {
          vote.classList.add("didvote");
        }
        vote.addEventListener("click", (_) => {
          let vote = {
            comment: comment.id,
            voter: user.value,
          };
          engine.api.voteComment(vote).then(updateComments);
        });
      } else {
        vote.classList.add("cantvote");
      }
      box.appendChild(vote);

      if (canDelete(comment)) {
        // Edit button
        let mod = document.createElement("button");
        mod.className = "fas fa-edit";
        mod.title = "Edit question";
        mod.addEventListener("click", (_) => {
          text.value = comment.markdown;
          text.commentId = comment.id;
          text.removeAttribute("answer");
          text.placeholder =
            "Type question, ⇧⏎ (Shift-Return) to enter. Use Markdown for formatting.";
          text.focus();
        });
        box.appendChild(mod);

        // Delete button
        let del = document.createElement("button");
        del.className = "fas fa-trash-alt";
        del.title = "Delete question";
        del.addEventListener("click", (_) => {
          engine.api
            .deleteComment(comment.id, engine.token.admin || user.value)
            .then(updateCommentsAndMenu);
        });
        box.appendChild(del);
      }

      if (isAdmin()) {
        let add = document.createElement("button");
        add.className = "far fa-plus-square";
        add.title = "Add answer";
        add.addEventListener("click", (_) => {
          text.value = "";
          text.commentId = comment.id;
          text.setAttribute("answer", "true");
          text.placeholder =
            "Type answer, ⇧⏎ (Shift-Return) to enter. Use Markdown for formatting.";
          text.focus();
        });

        box.appendChild(add);
      }

      // Answered button
      let answeredButton = document.createElement("button");
      const isAnswered = comment.answers && comment.answers.length > 0;
      const canAnswer = canDelete(comment);
      if (isAnswered) {
        answeredButton.className = "far fa-check-circle answered";
        answeredButton.title = canAnswer
          ? "Mark as not answered"
          : "Question has been answered";
        if (isAdmin()) {
          answeredButton.addEventListener("click", (_) => {
            let chain = Promise.resolve();
            for (let answer of comment.answers) {
              chain = chain.then(() =>
                engine.api.deleteAnswer(answer.id, engine.token.admin)
              );
            }
            chain.then(updateCommentsAndMenu);
          });
        }
      } else {
        answeredButton.className = "far fa-circle notanswered";
        answeredButton.title = canAnswer
          ? "Mark as answered"
          : "Question has not been answered";
        if (isAdmin()) {
          answeredButton.addEventListener("click", (_) => {
            engine.api
              .postAnswer(comment.id, engine.token.admin)
              .then(updateCommentsAndMenu);
          });
        }
      }
      answeredButton.disabled = !canAnswer;
      box.appendChild(answeredButton);

      // add question to container
      container.appendChild(item);
      MathJax.typeset([item]);

      // add answers after the question
      for (let answer of comment.answers) {
        if (!answer.link && !answer.html) break;
        let answerBlock = document.createElement("div");
        answerBlock.classList.add("item", "answer");

        if (isAdmin()) {
          // answer controls
          let abox = document.createElement("div");
          abox.classList.add("controls");
          answerBlock.insertBefore(abox, answerBlock.firstChild);

          // Delete button
          let del = document.createElement("button");
          del.className = "fas fa-trash-alt";
          del.title = "Delete answer";
          del.addEventListener("click", (_) => {
            engine.api
              .deleteAnswer(answer.id, engine.token.admin)
              .then(updateCommentsAndMenu);
          });
          abox.appendChild(del);
        }
        if (answer.link) {
          try {
            let url = new URL(answer.link);
            answerBlock.insertAdjacentHTML(
              "beforeend",
              `<div class="link">
                <a href="${url}" target="_blank">
                  <i class="fas fa-external-link-alt"></i>
                </a>
               </div>`
            );
          } catch (_) {}
        }
        if (answer.html) {
          answerBlock.insertAdjacentHTML(
            "beforeend",
            `<div class="description">${answer.html}</div>`
          );
        }
        container.appendChild(answerBlock);
        MathJax.typeset([answerBlock]);
      }
    }

    container.scrollTop = 0;

    if (localStorage.getItem("question-panel") == "open") {
      open.classList.add("checked");
      panel.classList.add("open");
    }
  }

  close.addEventListener("click", (_) => {
    open.classList.remove("checked");
    panel.classList.remove("open");
    localStorage.removeItem("question-panel");
  });

  open.addEventListener("click", (_) => {
    open.classList.add("checked");
    panel.classList.add("open");
    updateComments();
    document.activeElement.blur();
    localStorage.setItem("question-panel", "open");
  });

  login.addEventListener("click", (_) => {
    if (login.classList.contains("admin")) {
      engine.token.admin = null;
      username.value = "";
      password.value = "";
      login.classList.remove("admin");
      credentials.classList.remove("visible");
      updateComments();
    } else {
      if (credentials.classList.contains("visible")) {
        credentials.classList.remove("visible");
      } else {
        credentials.classList.add("visible");
        username.focus();
      }
    }
  });

  password.addEventListener("keydown", (e) => {
    if (e.key !== "Enter") return;

    if (login.classList.contains("admin")) {
      engine.token.admin = null;
      username.value = "";
      password.value = "";
      login.classList.remove("admin");
      credentials.classList.remove("visible");
      updateComments();
    } else {
      engine.api
        .getLogin({
          login: username.value,
          password: password.value,
          deck: engine.deckId,
        })
        .then((token) => {
          engine.token.admin = token.admin;
          login.classList.add("admin");
          username.value = "";
          password.value = "";
          credentials.classList.remove("visible");
          updateComments();
        })
        .catch((_) => {
          password.value = "";
        });
    }
  });

  if (!engine.token.authorized) {
    user.addEventListener("keydown", (e) => {
      if (e.key === "Enter") {
        updateComments();
        e.stopPropagation();
        document.activeElement.blur();
      }
    });

    check.addEventListener("click", (_) => {
      if (check.classList.contains("checked")) {
        check.classList.remove("checked");
        window.localStorage.removeItem("token");
        user.removeAttribute("disabled");
        user.type = "text";
      } else {
        if (user.value) {
          check.classList.add("checked");
          window.localStorage.setItem("token", user.value);
          user.setAttribute("disabled", true);
          user.type = "password";
        }
      }
      updateComments();
    });
  }

  text.addEventListener("keydown", (e) => {
    if (e.key === "Enter" && e.shiftKey) {
      let slideId = Reveal.getCurrentSlide().id;
      if (text.hasAttribute("answer")) {
        engine.api
          .postAnswer(text.commentId, engine.token.admin, text.value, null)
          .then(renderSubmit)
          .catch(console.log);
      } else {
        engine.api
          .submitComment(
            engine.deckId,
            slideId,
            engine.token.admin || user.value,
            text.value,
            text.commentId,
            window.location.toString()
          )
          .then(renderSubmit)
          .catch(console.log);
      }
      e.stopPropagation();
      e.preventDefault();
      document.activeElement.blur();
    }
  });

  Reveal.addEventListener("slidechanged", updateCommentsAndMenu);

  initUser();
  updateCommentsAndMenu();
}

const Plugin = {
  id: "feedback",
  init: (deck) => {
    Reveal = deck;
    const config = Reveal.getConfig().feedback;
    const url = config?.server || config?.["base-url"];
    const id = config?.deckID || config?.["deck-id"];
    // console.log("feedback", url, id);
    if (url) contactEngine(url, id);
  },
};

export default Plugin;
>>>>>>> a11y-new-menu
