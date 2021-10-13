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

      // Build the panel, once Reval is ready. (HAUER: This might ruin the DOM order so I moved this to init)
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

function initializeUsertoken(token_input, token_lock, menu, login_panel) {
  let localToken = window.localStorage.getItem("feedback-user-token");
  if(engine && engine.token && engine.token.authorized) {
    token_input.value = engine.token.authorized;
    token_input.setAttribute("disabled", true);
    token_input.type = "password";
    token_input.classList.add("hidden");
    token_lock.classList.add("checked");
    token_lock.classList.add("hidden");
    menu.classList.add("authorized");
    login_panel.classList.add("admin");
  } else if(localToken) {
    token_input.value = localToken;
    token_input.setAttribute("disabled", true);
    token_input.type = "password";
    token_lock.classList.add("checked");
  } else {
    token_input.value = engine.token.random;
    token_input.removeAttribute("disabled");
    token_input.type = "text";
    token_lock.classList.remove("checked");
  }
}

function clear_after_submit(input, placeholder) {
  return function () {
    refresh_comments_and_menu();
    input.value = "";
    input.commentId = null;
    input.removeAttribute("answer");
    input.placeholder = placeholder;
  }
}

/* Returns a function that has the parameter values bound and accessible */
function refresh_comments(counter, badge, container, text, token) {
  return function() {
    let slideId = Reveal.getCurrentSlide().id;
    engine.api
      .getComments(engine.deckId, slideId, engine.token.admin || token)
      .then(refresh_comment_list(counter, badge, container, text, token))
      .catch(console.log);
  }
}

function refresh_slide_menu() {
  engine.api
  .getComments(engine.deckId)
  .then(refresh_slide_menu_items)
  .catch(console.log);
}

function refresh_slide_menu_items(list) {
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

function refresh_comments_and_menu(menuCounter, buttonBadge, listContainer, textareaInput, usertoken) {
  refresh_comments(menuCounter, buttonBadge, listContainer, textareaInput, usertoken)();
  refresh_slide_menu();
}

//Can the be part of the engine code? These feel more like methods than functions
function isAdmin() {
  return engine.token.admin !== null;
}

function isAuthor(comment, usertoken) {
  return comment.author === usertoken;
}

function canDelete(comment, usertoken) {
  return isAdmin() || (isAuthor(comment, usertoken) && comment.answers.length == 0);
}

function refresh_comment_list(menuCounter, buttonBadge, listContainer, textareaInput, usertoken) {
  return function(list) {
    console.log("refreshing comment list");
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
    menuCounter.textContent = list.length;
    menuCounter.setAttribute("data-count", list.length);
    buttonBadge.textContent = list.length;
    buttonBadge.setAttribute("data-count", list.length);
    if (allAnswered) {
      menuCounter.classList.add("answered");
      buttonBadge.classList.add("answered");
    } else {
      menuCounter.classList.remove("answered");
      buttonBadge.classList.remove("answered");
    }

    // clear question container
    while (listContainer.firstChild) {
      listContainer.removeChild(listContainer.lastChild);
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
      if (!isAuthor(comment, usertoken)) {
        vote.classList.add("canvote");
        if (comment.didvote) {
          vote.classList.add("didvote");
        }
        vote.addEventListener("click", (_) => {
          let vote = {
            comment: comment.id,
            voter: usertoken,
          };
          engine.api.voteComment(vote).then(refresh_comments(menuCounter, buttonBadge, listContainer, textareaInput, usertoken));
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
          textareaInput.value = comment.markdown;
          textareaInput.commentId = comment.id;
          textareaInput.removeAttribute("answer");
          textareaInput.placeholder =
            "Type question, ⇧⏎ (Shift-Return) to enter. Use Markdown for formatting.";
          textareaInput.focus();
        });
        box.appendChild(mod);

        // Delete button
        let del = document.createElement("button");
        del.className = "fas fa-trash-alt";
        del.title = "Delete question";
        del.addEventListener("click", (_) => {
          engine.api
            .deleteComment(comment.id, engine.token.admin || usertoken)
            .then(refresh_comments_and_menu);
        });
        box.appendChild(del);
      }

      if (isAdmin()) {
        let add = document.createElement("button");
        add.className = "far fa-plus-square";
        add.title = "Add answer";
        add.addEventListener("click", (_) => {
          textareaInput.value = "";
          textareaInput.commentId = comment.id;
          textareaInput.setAttribute("answer", "true");
          textareaInput.placeholder =
            "Type answer, ⇧⏎ (Shift-Return) to enter. Use Markdown for formatting.";
          textareaInput.focus();
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
            chain.then(refresh_comments_and_menu);
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
              .then(refresh_comments_and_menu);
          });
        }
      }
      answeredButton.disabled = !canAnswer;
      box.appendChild(answeredButton);

      // add question to container
      listContainer.appendChild(item);
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
              .then(refresh_comments_and_menu);
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
        listContainer.appendChild(answerBlock);
        MathJax.typeset([answerBlock]);
      }
    }

    listContainer.scrollTop = 0;

    //HAUER: I do not know why this is here and it makes the signature of this function even larger, so I will just remove it
/*    if (localStorage.getItem("question-panel") == "open") {
      open.classList.add("checked");
      panel.inert = false;
      panel.classList.add("open");
    } */
  }
}

// Builds the panel and sets up event handlers.
function buildInterface() {
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

  let openButton = button;
  let menuPanel = menu;

  let textareaInput = menu.querySelector(".feedback-question-input textarea");
  let menuCounter = menu.querySelector(".counter");
  let buttonBadge = button.querySelector(".badge");
  let listContainer = menu.querySelector(".feedback-list");
  let usertokenInput = menu.querySelector(".feedback-token-input");
  let lockTokenButton = menu.querySelector(".feedback-lock");
  let closeButton = menu.querySelector(".feedback-close");
  let loginArea = menu.querySelector(".feedback-login");
  let loginUsernameInput = menu.querySelector("#feedback-username");
  let loginPasswordInput = menu.querySelector("#feedback-password");
  let credentialsArea = menu.querySelector(".feedback-credentials");

  textareaInput.addEventListener("keypress", (e) => {
    e.stopPropagation();
  });

  if(Reveal.hasPlugin("decker-plugins")) {
    let manager = Reveal.getPlugin("decker-plugins");
    manager.registerPlugin({decker_button: button, decker_anchor: "TOP_RIGHT"});
  }
  let reveal_element = document.querySelector(".reveal");
  reveal_element.appendChild(menu);

  closeButton.addEventListener("click", (_) => {
    openButton.classList.remove("checked");
    menuPanel.classList.remove("open");
    menuPanel.inert = true;
    openButton.focus();
    localStorage.removeItem("question-panel");
  });

  openButton.addEventListener("click", (_) => {
    openButton.classList.add("checked");
    menuPanel.classList.add("open");
    menuPanel.inert = false;
    lockTokenButton.focus();
    refresh_comments(menuCounter, buttonBadge, listContainer, textareaInput, usertokenInput.value)();
    localStorage.setItem("question-panel", "open");
  });

  loginArea.addEventListener("click", (_) => {
    if (loginArea.classList.contains("admin")) {
      engine.token.admin = null;
      loginUsernameInput.value = "";
      loginPasswordInput.value = "";
      loginArea.classList.remove("admin");
      credentialsArea.classList.remove("visible");
      refresh_comments(menuCounter, buttonBadge, listContainer, textareaInput, usertokenInput.value)();
    } else {
      if (credentialsArea.classList.contains("visible")) {
        credentialsArea.classList.remove("visible");
      } else {
        credentialsArea.classList.add("visible");
        loginUsernameInput.focus();
      }
    }
  });

  loginPasswordInput.addEventListener("keydown", (e) => {
    if (e.key !== "Enter") return;

    if (loginArea.classList.contains("admin")) {
      engine.token.admin = null;
      loginUsernameInput.value = "";
      loginPasswordInput.value = "";
      loginArea.classList.remove("admin");
      credentialsArea.classList.remove("visible");
      refresh_comments(menuCounter, buttonBadge, listContainer, textareaInput, usertokenInput.value)();
    } else {
      engine.api
        .getLogin({
          login: loginUsernameInput.value,
          password: loginPasswordInput.value,
          deck: engine.deckId,
        })
        .then((token) => {
          engine.token.admin = token.admin;
          loginArea.classList.add("admin");
          loginUsernameInput.value = "";
          loginPasswordInput.value = "";
          credentialsArea.classList.remove("visible");
          refresh_comments(menuCounter, buttonBadge, listContainer, textareaInput, usertokenInput.value)();
        })
        .catch((_) => {
          loginPasswordInput.value = "";
        });
    }
  });

  if (engine.token && !engine.token.authorized) {
    usertokenInput.addEventListener("keydown", (e) => {
      if (e.key === "Enter") {
        refresh_comments(menuCounter, buttonBadge, listContainer, textareaInput, usertokenInput.value)();
        e.stopPropagation();
        document.activeElement.blur();
      }
    });

    lockTokenButton.addEventListener("click", (_) => {
      if (lockTokenButton.classList.contains("checked")) {
        lockTokenButton.classList.remove("checked");
        window.localStorage.removeItem("token");
        usertokenInput.removeAttribute("disabled");
        usertokenInput.type = "text";
      } else {
        if (usertokenInput.value) {
          lockTokenButton.classList.add("checked");
          window.localStorage.setItem("token", usertokenInput.value);
          usertokenInput.setAttribute("disabled", true);
          usertokenInput.type = "password";
        }
      }
      refresh_comments(menuCounter, buttonBadge, listContainer, textareaInput, usertokenInput.value)();
    });
  }

  textareaInput.addEventListener("keydown", (e) => {
    if (e.key === "Enter" && e.shiftKey) {
      let slideId = Reveal.getCurrentSlide().id;
      if (textareaInput.hasAttribute("answer")) {
        engine.api
          .postAnswer(textareaInput.commentId, engine.token.admin, textareaInput.value, null)
          .then(clear_after_submit(textareaInput, "Type question, ⇧⏎ (Shift-Return) to enter. Use Markdown for formatting."))
          .catch(console.log);
      } else {
        engine.api
          .submitComment(
            engine.deckId,
            slideId,
            engine.token.admin || usertokenInput.value,
            textareaInput.value,
            textareaInput.commentId,
            window.location.toString()
          )
          .then(clear_after_submit(textareaInput, "Type question, ⇧⏎ (Shift-Return) to enter. Use Markdown for formatting."))
          .catch(console.log);
      }
      e.stopPropagation();
      e.preventDefault();
      document.activeElement.blur();
    }
  });

  Reveal.addEventListener("slidechanged", () => refresh_comments_and_menu(menuCounter, buttonBadge, listContainer, textareaInput, usertokenInput.value));

  initializeUsertoken(usertokenInput, lockTokenButton, menu, loginArea);
  refresh_comments_and_menu(menuCounter, buttonBadge, listContainer, textareaInput, usertokenInput.value);
}

const Plugin = {
  id: "feedback",
  init: (deck) => {
    Reveal = deck;
    const config = Reveal.getConfig().feedback;
    const url = config?.server || config?.["base-url"];
    const id = config?.deckID || config?.["deck-id"];
    console.log("feedback", url, id);
    if (url) contactEngine(url, id);
  },
};

export default Plugin;
