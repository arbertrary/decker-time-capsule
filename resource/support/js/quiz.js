var quizModule = {
    quiz: function () {
        multipleChoice();
        blanktextButtons();
        freetextAnswerButtons();
        let initialMatchings = initMatching();
        matchings(initialMatchings);
    }
}
/* ************************
  MULTIPLE CHOICE QUESTIONS
*************************** */
function multipleChoice() {
    const questions = document.getElementsByClassName("survey");
    for (let question of questions) {
        // add event listener to color the chosen answer(s)
        const allAnswers = question.getElementsByTagName("li");
        var defBorder = allAnswers[0].style.border;
        for (let answer of allAnswers) {
            answer.addEventListener("click", function () {
                if (!this.classList.contains("selected")) {
                    this.classList.add("selected");
                    this.style.border = "solid black";
                    this.style.backgroundColor = "#dcdcdc";
                } else {
                    this.classList.remove("selected");
                    this.style.border = defBorder;
                    this.style.backgroundColor = "#FFF";
                }
            });
        }
        var answerButton = question.querySelector(".mcAnswerButton");
        if (answerButton != null) {
            answerButton.onclick = function () {
                var selectedAnswer = null;
                for (let answer of allAnswers) {
                    if (answer.classList.contains("selected")) {
                        selectedAnswer = FormatChoiceResponse(answer.querySelector('p').innerHTML);
                    } else { continue; }
                }
                if (selectedAnswer != null) {
                    this.disabled = true;
                    for (let answer of allAnswers) {
                        var answer_div = answer.querySelector(".answer");
                        answer_div.classList.contains("right") ? answer.style.backgroundColor = "#97ff7a" : answer.style.backgroundColor = "#ff7a7a";
                        const tooltips = answer.getElementsByClassName("tooltip");
                        for (let tooltip of tooltips) {
                            tooltip.style.display = "inline";
                        }
                        answer.style.pointerEvents = "none";
                    }
                } else {
                    alert("No answer chosen!");
                    return false;
                }
            }
        }
    }
}
function FormatChoiceResponse(value) {
    var newValue = new String(value);
    newValue = newValue.replace(/^_/, "");
    newValue = newValue.replace(/(?=.)(^\[)?((\\n)+)?(\])?(\\n)/, "");
    return newValue;
}
/* ************************
     BLANK TEXT QUESTIONS
*************************** */
function blanktextButtons() {
    var btButtons = document.getElementsByClassName("btAnswerButton");
    for (let i = 0; i < btButtons.length; i++) {
        const button = btButtons[i];
        button.onclick = function () {
            var blanktext = this.closest(".blankText");
            var selects = blanktext.getElementsByClassName("blankSelect");
            var inputs = blanktext.getElementsByClassName("blankInput");
            for (let input of inputs) {
                let correctAnswer = input.getAttribute("answer").toLowerCase().trim();
                let selectedAnswer = input.value.toLowerCase().trim();
                if (selectedAnswer) {
                    input.disabled = true;
                    if (selectedAnswer == correctAnswer) {
                        input.style.backgroundColor = "rgb(151, 255, 122)";
                        input.setAttribute("size", input.value.length);
                    } else {
                        input.style.backgroundColor = "rgb(255, 122, 122)";
                        input.value += " (" + input.getAttribute("answer") + ")";
                        input.setAttribute("size", input.value.length);
                    }
                } else {
                    alert("Please complete all questions.");
                    return false;
                }
            }
            for (let select of selects) {
                var correctAnswer;
                for (let o of select.options) {
                    if (o.getAttribute("answer") == "true") {
                        correctAnswer = o;
                    }
                }
                var selectedAnswer = select.options[select.selectedIndex];
            }

            if (selectedAnswer) {
                for (let s of selects) {
                    if (s == correctAnswer) {
                        s.style.backgroundColor = "rgb(151, 255, 122)";
                        s.textContent += " ✓";
                    } else {
                        s.style.backgroundColor = "rgb(250, 121, 121)";
                        s.textContent += " ✗";
                    }
                }
            } else {
                alert("Please complete all questions.");
                return false;
            }
        }
    }
}
/* ************************
     FREE TEXT QUESTIONS
*************************** */
function freetextAnswerButtons() {
    const answerButtons = document.getElementsByClassName('freetextAnswerButton');
    for (let button of answerButtons) {
        button.onclick = function () {
            var input = this.parentElement.querySelector('.freetextInput');
            // Has the user entered anything?
            let correctAnswer = input.getAttribute("answer").toLowerCase().trim();
            let selectedAnswer = input.value.toLowerCase().trim();
            if (selectedAnswer) {
                var answer = input.getAttribute("answer").trim();
                if (selectedAnswer == correctAnswer) {
                    input.style.backgroundColor = "rgb(151, 255, 122)";
                }
                else {
                    input.style.backgroundColor = "rgb(255, 122, 122)";
                    input.value += " (" + answer + ")";
                }
                input.setAttribute("size", input.value.length);
                input.disabled = true;
                this.disabled = true;
            }
            else {
                alert("No answer entered!");
                return false;
            }
        }
    }
}
/* ************************
     MATCHING QUESTIONS
*************************** */
// Save the initial state of matching questions for retry and show solution buttons
function initMatching() {
    // Manual deep copy of the initial states of all matching questions
    const m = document.getElementsByClassName("matching");
    var initialMatchings = [];
    for (let i of m) {
        // Replace reveal.js data-src with src to avoid lazy loading
        var imgs = i.getElementsByTagName("img");
        for (let img of imgs) {
            var src = img.getAttribute("data-src");
            if (src) {
                img.setAttribute("src", src);
                img.removeAttribute("data-src");
            }
        }
        var node = i.cloneNode(true);
        initialMatchings.push(node);
    }
    return initialMatchings;
}
// Adds event listeners for dragging and dropping to the elements of "matching" questions
function matchings(initialMatchings) {
    var dropzones = document.getElementsByClassName("dropzone");
    var draggables = document.getElementsByClassName("draggable");

    for (var i = 0; i < dropzones.length; i++) {
        dropzones[i].id = "drop".concat(i.toString());
        dropzones[i].addEventListener("drop", drop);
        dropzones[i].addEventListener("dragover", allowDrop);

        for (let child of dropzones[i].children) {
            if (!child.classList.contains("draggable")) {
                child.setAttribute("style", "pointer-events:none");
            }
        }
    }

    for (i = 0; i < draggables.length; i++) {
        draggables[i].id = "drag".concat(i.toString());
        draggables[i].addEventListener("dragstart", drag);

        // disable children (e.g. images) from being dragged themselves
        for (let child of draggables[i].children) {
            child.setAttribute('draggable', "false");
            child.className = "draggableChild";
        }
    }
    // Order of execution here is important. 
    // matchingAnswerButton has to be first so the sample solution is in the correct order. Very dubious hack

    matchingAnswerButtons(initialMatchings);
    shuffleDraggables();
    retryButtons(initialMatchings);
}
// Copied from revealjs/math.js
function reloadMath() {
    // Typeset followed by an immediate reveal.js layout since
    // the typesetting process could affect slide height
    MathJax.Hub.Queue(['Typeset', MathJax.Hub]);
    MathJax.Hub.Queue(Reveal.layout);

    // Reprocess equations in slides when they turn visible
    Reveal.addEventListener('slidechanged', function (event) {

        MathJax.Hub.Queue(['Typeset', MathJax.Hub, event.currentSlide]);

    });
}
// Configure retryButtons
function retryButtons(initialMatchings) {
    var buttons = document.getElementsByClassName("retryButton");

    for (i = 0; i < buttons.length; i++) {
        const initial = initialMatchings[i].cloneNode(true);

        buttons[i].onclick = function () {
            var curr = this.closest(".matching");
            curr.parentNode.replaceChild(initial, curr);
            // Call matchings once again to reset everything. e.g the shuffling etc
            matchings(initialMatchings);
            reloadMath();
        }
    }
}
// Shuffle draggables so the correct pairings aren't always directly below each other
function shuffleDraggables() {
    var dragzones = document.getElementsByClassName("dragzone");
    for (let container of dragzones) {
        var elementsArray = Array.prototype.slice.call(container.getElementsByClassName('draggable'));
        elementsArray.forEach(function (element) {
            container.removeChild(element);
        })
        shuffleArray(elementsArray);
        elementsArray.forEach(function (element) {
            container.appendChild(element);
        })
    }
}
// Fisher-Yates (aka Knuth) Shuffle (from stackoverflow)
function shuffleArray(array) {
    var currentIndex = array.length, temporaryValue, randomIndex;

    // While there remain elements to shuffle...
    while (0 !== currentIndex) {
        // Pick a remaining element...
        randomIndex = Math.floor(Math.random() * currentIndex);
        currentIndex -= 1;

        // And swap it with the current element.
        temporaryValue = array[currentIndex];
        array[currentIndex] = array[randomIndex];
        array[randomIndex] = temporaryValue;
    }

    return array;
}
function matchingAnswerButtons(initialMatchings) {
    var answerButtons = document.getElementsByClassName("matchingAnswerButton");
    for (let button of answerButtons) {
        button.onclick = function () {
            var matchingField = this.closest(".matching");
            var dropzones = matchingField.getElementsByClassName("dropzone");

            // Alert if there's any empty dropzone (i.e. not all pairs are completed)
            for (let drop of dropzones) {
                var draggables = drop.getElementsByClassName("draggable");
                if (draggables.length == 0) {
                    alert("Please complete all pairs.");
                    return;
                }
            }

            // Correct match pairs
            for (let drop of dropzones) {
                var first = drop.getElementsByClassName("draggable")[0];
                var dropID = drop.id.replace("drop", "");

                if (first.id.replace("drag", "") == dropID) {  // if correct
                    drop.style.backgroundColor = "rgb(151, 255, 122)";
                    first.setAttribute("draggable", "false");
                } else {
                    drop.style.backgroundColor = "rgb(255, 122, 122)";
                    first.setAttribute("draggable", "false");
                }
            }
            // Get the initial and current states of the dragzones
            const j = Array.prototype.slice.call(answerButtons).indexOf(this);
            var initialDragzone = initialMatchings[j].getElementsByClassName("dragzone")[0].cloneNode(true);
            var currDragzone = matchingField.getElementsByClassName("dragzone")[0];

            // replace the empty dropzone with the correct/sample solution
            matchingField.replaceChild(initialDragzone, currDragzone);
            reloadMath();

            this.disabled = true;
            this.nextSibling.disabled = true;
        }
    }
}
function allowDrop(ev) {
    ev.preventDefault();
}
function drag(ev) {
    ev.dataTransfer.setData("text", ev.target.id);
}
function drop(ev) {
    ev.preventDefault();
    var data = ev.dataTransfer.getData("text");
    if (ev.target.className == "draggable") {
        return false;
    }
    ev.target.appendChild(document.getElementById(data));
    ev.target.disabled = true;
}