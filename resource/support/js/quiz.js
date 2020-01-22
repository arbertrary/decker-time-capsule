var quizModule = {
    quiz: function () {
        multipleChoice();
        blanktextButtons();
        freetextAnswerButtons();
        let initialMatchings = initMatching();
        matchings(initialMatchings);
    }
}

var questionArray = [];
var totalEarned = 0; var totalPossible = 0; var question_num = 1;
var graded = document.getElementById('slideContent').getAttribute("graded");
class Question {
    constructor(id, type, selectedAnswers, correctAnswers, weight, earned, result) {
        this.id = id;
        this.type = type;
        this.selectedAnswers = selectedAnswers;
        this.correctAnswers = correctAnswers;
        this.weight = weight;
        this.earned = earned;
        this.result = result;
    }
}
/* ************************
  MULTIPLE CHOICE QUESTIONS
*************************** */
function multipleChoice() {
    const questions = document.getElementsByClassName("survey");
    let survey_num = 0;
    for (let question of questions) {
        question.setAttribute("data-survey-num", survey_num.toString());
        survey_num += 1;
        question.id = question_num.toString();
        question_num++;
        // add event listener to color the chosen answer(s)
        const allAnswers = question.getElementsByTagName("li");
        var defBorder = allAnswers[0].style.border;
        for (let answer of allAnswers) {
            answer.addEventListener("click", function () {
                if (!this.classList.contains("selected")) {
                    this.classList.add("selected");
                    this.style.border = "thick solid black";
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
                var selectedAnswer = null; var correctAnswer;
                for (let answer of allAnswers) {
                    if (answer.classList.contains("selected")) {
                        selectedAnswer = FormatChoiceResponse(answer.querySelector('p').innerHTML);
                    } else { continue; }
                }
                if (selectedAnswer != null) {
                    this.disabled = true;
                    for (let answer of allAnswers) {
                        var result = "";
                        var answer_div = answer.querySelector(".answer");
                        if (answer_div.classList.contains("right")) {
                            correctAnswer = FormatChoiceResponse(answer.querySelector('p').innerHTML);
                            answer.style.backgroundColor = "#97ff7a";
                            result = "correct";
                        } else {
                            answer.style.backgroundColor = "#ff7a7a";
                            result = "wrong";
                        }
                        const tooltips = answer.getElementsByClassName("tooltip");
                        for (let tooltip of tooltips) {
                            tooltip.style.display = "inline";
                        }
                        answer.style.pointerEvents = "none";
                    }
                }
                else {
                    alert("No answer chosen!");
                    return false;
                }
                questionArray.push(new Question(question.id, "choice", selectedAnswer, correctAnswer, 0, 0, result));
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
function scormMC() {
    var weight = 0; var earned = 0;

    const questions = document.getElementsByClassName("survey");
    for (let question of questions) {
        question.id = question_num.toString();
        question_num++;
        weight = Number(question.parentElement.getAttribute('data-points'));
        let result = "wrong";
        const allAnswers = question.getElementsByClassName("answer");
        var selectedAnswer = null; var correctAnswer;

        // get correct and selected answers
        for (let answer of allAnswers) {
            answer.parentElement.style.pointerEvents = "none";
            let answerText = FormatChoiceResponse(answer.querySelector('p').innerHTML);
            if (answer.parentElement.classList.contains("selected")) {              // if selected
                selectedAnswer = answerText;
                if (answer.classList.contains("right")) {                           // and correct
                    correctAnswer = answerText;
                    answer.parentElement.style.backgroundColor = "rgb(151, 255, 122)";
                } else {                                                            // and incorrect
                    answer.parentElement.style.backgroundColor = "rgb(250, 121, 121)";
                }
            } else {                                                                // if not selected
                if (answer.classList.contains("right")) {                           // and correct
                    correctAnswer = answerText;
                    answer.parentElement.style.backgroundColor = "rgb(151, 255, 122)";
                }
            }
        }
        if (selectedAnswer) {
            if (selectedAnswer == correctAnswer) {
                result = "correct";
                earned = weight;
            }
        } else {
            selectedAnswer = "no answer given";
        }

        totalEarned += earned;
        totalPossible += weight;
        questionArray.push(new Question(question.id, "choice", selectedAnswer, correctAnswer, weight, earned, result));
        // console.log("pushing question " + question.id + ", choice, weight: " + weight + ", earned: " + earned + ", result: " + result);
        // console.log(" selected: " + selectedAnswer.toString());
        // console.log(" correct: " + correctAnswer.toString());
    }
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
                input.id = question_num.toString();
                question_num++;
                let result = null;
                let correctAnswer = input.getAttribute("answer").toLowerCase().trim();
                let selectedAnswer = input.value.toLowerCase().trim();
                if (selectedAnswer) {
                    input.disabled = true;
                    if (selectedAnswer == correctAnswer) {
                        input.style.backgroundColor = "rgb(151, 255, 122)";
                        result = "correct";
                    } else {
                        input.style.backgroundColor = "rgb(250, 121, 121)";
                        result = "wrong";
                    }
                } else {
                    alert("Please complete all questions.");
                    return false;
                }
                questionArray.push(new Question(input.id, "fill-in", selectedAnswer, correctAnswer, 0, 0, result));
            }
            for (let select of selects) {
                select.id = question_num.toString();
                question_num++;
                let result = null;
                let correctAnswer = "";
                for (let o of select.options) {
                    if (o.getAttribute("answer") == "true") {
                        correctAnswer = o.value;
                    }
                }
                let selectedAnswer = select.options[select.selectedIndex].value;
                if (selectedAnswer) {
                    select.disabled = true;
                    if (selectedAnswer == correctAnswer) {
                        select.style.backgroundColor = "rgb(151, 255, 122)";
                        result = "correct";
                    } else {
                        select.style.backgroundColor = "rgb(250, 121, 121)";
                        result = "wrong";
                    }
                } else {
                    alert("Please complete all questions.");
                    return false;
                }
                questionArray.push(new Question(select.id, "choice", selectedAnswer, correctAnswer, 0, 0, result));
            }
        }
    }
}
function scormBlanktext() {
    var weight = 0;
    var result;
    var questions = document.getElementsByClassName("blankText");
    for (let question of questions) {
        // get selected/corrected for blanks
        var inputs = question.getElementsByTagName('input');
        var selects = question.getElementsByTagName('select');
        var totalPoints = Number(question.parentElement.getAttribute('data-points'));
        totalPossible += totalPoints;
        var weight = totalPoints / (inputs.length + selects.length);
        for (let input of inputs) {
            var earned = 0;
            input.id = question_num.toString();
            question_num++;
            result = null;
            input.disabled = true;
            let correctAnswer = input.getAttribute("answer").toLowerCase().trim();
            let selectedAnswer = input.value.toLowerCase().trim();
            if (selectedAnswer) {
                if (selectedAnswer == correctAnswer) {
                    input.style.backgroundColor = "rgb(151, 255, 122)";
                    earned = weight;
                    totalEarned += earned;
                    result = "correct";
                } else {
                    input.style.backgroundColor = "rgb(250, 121, 121)";
                    result = "wrong";
                }
            } else {
                selectedAnswer = "no answer given";
                input.style.backgroundColor = "rgb(250, 121, 121)";
                result = "wrong";
            }
            questionArray.push(new Question(input.id, "fill-in", selectedAnswer, correctAnswer, weight, earned, result));
            // console.log("pushing question " + input.id + ", blank fill in, weight: " + weight + ", earned: " + earned + ", result: " + result);
            // console.log(" selected: " + selectedAnswer.toString());
            // console.log(" correct: " + correctAnswer.toString());
        }

        // get selected/corrected for selects
        for (let select of selects) {
            var earned = 0;
            select.id = question_num.toString();
            question_num++;
            result = null;
            select.disabled = true;
            let correctAnswer = "";
            for (let o of select.options) {
                if (o.getAttribute("answer") == "true") {
                    correctAnswer = o.value;
                }
            }
            let selectedAnswer = select.options[select.selectedIndex].value;
            if (selectedAnswer) {
                if (selectedAnswer == correctAnswer) {
                    select.style.backgroundColor = "rgb(151, 255, 122)";
                    earned = weight;
                    totalEarned += earned;
                    result = "correct";
                } else {
                    select.style.backgroundColor = "rgb(250, 121, 121)";
                    result = "wrong";
                }
            } else {
                selectedAnswer = "no answer given";
                result = "wrong";
                select.style.backgroundColor = "rgb(250, 121, 121)";
            }
            questionArray.push(new Question(select.id, "choice", selectedAnswer, correctAnswer, weight, earned, result));
            // console.log("pushing question " + select.id + ", blank choice, weight: " + weight + ", earned: " + earned + ", result: " + result);
            // console.log(" selected: " + selectedAnswer.toString());
            // console.log(" correct: " + correctAnswer.toString());
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
            input.id = question_num.toString();
            question_num++;
            let result = null;
            // Has the user entered anything?
            let correctAnswer = input.getAttribute("answer").toLowerCase().trim();
            let selectedAnswer = input.value.toLowerCase().trim();
            if (selectedAnswer) {
                var answer = input.getAttribute("answer").trim();
                if (selectedAnswer == correctAnswer) {
                    input.style.backgroundColor = "rgb(151, 255, 122)";
                    result = "correct";
                }
                else {
                    input.style.backgroundColor = "rgb(255, 122, 122)";
                    input.value += " (" + answer + ")";
                    result = "wrong";
                }
                input.setAttribute("size", input.value.length);
                input.disabled = true;
                this.disabled = true;
            }
            else {
                alert("No answer entered!");
                return false;
            }
            questionArray.push(new Question(input.id, "fill-in", selectedAnswer, correctAnswer, 0, 0, result));
        }
    }
}
function scormFreetext() {
    var weight = 0;
    var questions = document.getElementsByClassName("freetextQuestion");
    for (let question of questions) {
        var totalWeight = Number(question.parentElement.getAttribute('data-points'));
        totalPossible += totalWeight;

        var inputs = question.getElementsByTagName('input');
        var weight = totalWeight / inputs.length;
        for (let input of inputs) {
            var earned = 0;
            input.id = question_num.toString();
            question_num++;
            var result = null;
            input.disabled = true;
            let correctAnswer = input.getAttribute("answer").toLowerCase().trim();
            let selectedAnswer = input.value.toLowerCase().trim();
            if (selectedAnswer) {
                if (selectedAnswer == correctAnswer) {
                    input.style.backgroundColor = "rgb(151, 255, 122)";
                    earned = weight;
                    totalEarned += earned;
                    result = "correct";
                } else {
                    input.style.backgroundColor = "rgb(250, 121, 121)";
                    result = "wrong";
                }
            } else {
                selectedAnswer = "no answer given";
                input.style.backgroundColor = "rgb(250, 121, 121)";
                result = "wrong";
            }
            questionArray.push(new Question(input.id, "fill-in", selectedAnswer, correctAnswer, weight, earned, result));
            // console.log("pushing question " + input.id + ", freetext, weight: " + weight + ", earned: " + earned + ", result: " + result);
            // console.log(" selected: " + selectedAnswer.toString());
            // console.log(" correct: " + correctAnswer.toString());
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
function getAns(drop) {
    var dropID = drop.id.replace("drop", "");
    var dropImgID = "dropImg" + dropID;
    var img = drop.querySelector('img');
    var ans;
    if (img == null) {
        ans = FormatChoiceResponse(drop.innerText);
    } else {
        ans = img.src.replace(/^.*[\\\/]/, '') + ":" + FormatChoiceResponse(drop.querySelector('div').innerHTML);
        img.id = dropImgID;
    }
    return ans;
}
function matchingAnswerButtons(initialMatchings) {
    var answerButtons = document.getElementsByClassName("matchingAnswerButton");
    for (let button of answerButtons) {
        button.onclick = function () {
            var matchingField = this.closest(".matching");
            matchingField.id = question_num.toString();
            question_num++;
            var selectedAnswers = []; var correctAnswers = [];
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
                selectedAnswers.push(getAns(drop));
                var dropID = drop.id.replace("drop", "");
                var img = drop.querySelector('img');
                var dropImgID = "dropImg" + dropID;

                if (first.id.replace("drag", "") == dropID) {  // if correct
                    drop.style.backgroundColor = "rgb(151, 255, 122)";
                    first.setAttribute("draggable", "false");
                    correctAnswers.push(getAns(drop));
                } else {
                    drop.style.backgroundColor = "rgb(255, 122, 122)";
                    first.setAttribute("draggable", "false");
                    var dragText = FormatChoiceResponse(document.getElementById("drag" + dropID).innerText);
                    var dropText = (img == null) ? drop.childNodes[0].nodeValue.toString() : document.getElementById(dropImgID).src.replace(/^.*[\\\/]/, '');
                    correctAnswers.push(FormatChoiceResponse(dropText + ":" + dragText));
                }
            }
            var result = "correct";
            for (var i = 0; i < selectedAnswers.length; i++) {
                if (selectedAnswers[i] !== correctAnswers[i]) {
                    result = "wrong";
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
            questionArray.push(new Question(matchingField.id, "matching", selectedAnswers, correctAnswers, 0, 0, result));
        }
    }
}
function scormMatching() {
    const questions = document.getElementsByClassName("matching");

    for (let question of questions) {
        question.id = question_num.toString();
        question_num++;
        var selectedAnswers = []; var correctAnswers = [];
        let earned = 0;
        var weight = Number(question.parentElement.getAttribute('data-points'));
        totalPossible += weight;
        var dropzones = question.getElementsByClassName("dropzone");

        // Correct match pairs
        for (let drop of dropzones) {
            var first = drop.getElementsByClassName("draggable")[0];
            if (first) {
                selectedAnswers.push(getAns(drop));
                var dropID = drop.id.replace("drop", "");
                var img = drop.querySelector('img');
                var dropImgID = "dropImg" + dropID;

                if (first.id.replace("drag", "") == dropID) {                       // if correct, push to correctAnswers
                    drop.style.backgroundColor = "rgb(151, 255, 122)";
                    first.setAttribute("draggable", "false");
                    correctAnswers.push(getAns(drop));
                } else {                                                            // if incorrect, get correct answers to push
                    drop.style.backgroundColor = "rgb(255, 122, 122)";
                    first.setAttribute("draggable", "false");
                    var dragText = FormatChoiceResponse(document.getElementById("drag" + dropID).innerText);
                    var dropText = (img == null) ? drop.childNodes[0].nodeValue.toString() : document.getElementById(dropImgID).src.replace(/^.*[\\\/]/, '');
                    correctAnswers.push(FormatChoiceResponse(dropText + ":" + dragText));
                }
            } else {
                selectedAnswers.push("no answer given");
            }
        }
        var result = "correct";
        for (var i = 0; i < selectedAnswers.length; i++) {                  // check if all matches are correct
            if (selectedAnswers[i] !== correctAnswers[i]) {
                result = "wrong";
            }
        }
        if (result == "correct") {
            earned = weight;
            totalEarned += earned;
        }
        questionArray.push(new Question(question.id, "matching", selectedAnswers, correctAnswers, weight, earned, result));
        // console.log("pushing question " + question.id + ", matching, weight: " + weight + ", earned: " + earned + ", result: " + result);
        // console.log(" selected: " + JSON.stringify(selectedAnswers));
        // console.log(" correct: " + JSON.stringify(correctAnswers));
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
/* ************************
        SCORM FUNCTIONS
*************************** */
function gradeQuiz() {
    scormMC();
    scormBlanktext();
    scormFreetext();
    scormMatching();

    document.getElementById("submitButton").style.pointerEvents = "none";
    let submitMessage = document.createElement('p');
    submitMessage.style.color = "#009933";
    submitMessage.innerHTML = "Your answers have been submitted.";
    document.getElementById("submitSlide").appendChild(submitMessage);

    RecordTest();
}
function RecordTest() {
    for (let question of questionArray) {
        var nextIndex = ScormProcessGetValue("cmi.interactions._count");
        ScormProcessSetValue("cmi.interactions." + nextIndex + ".id", question.id);
        ScormProcessSetValue("cmi.interactions." + nextIndex + ".type", question.type);
        ScormProcessSetValue("cmi.interactions." + nextIndex + ".student_response", question.selectedAnswers);
        ScormProcessSetValue("cmi.interactions." + nextIndex + ".correct_responses.0.pattern", question.correctAnswers);
        ScormProcessSetValue("cmi.interactions." + nextIndex + ".result", question.result);
        // console.log("cmi.interactions." + nextIndex + ".id: " + question.id);
        // console.log("cmi.interactions." + nextIndex + ".type: " + question.type);
        // console.log("cmi.interactions." + nextIndex + ".student_response: " + JSON.stringify(question.selectedAnswers));
        // console.log("cmi.interactions." + nextIndex + ".correct_responses.0.pattern: " + JSON.stringify(question.correctAnswers));
        // console.log("cmi.interactions." + nextIndex + ".result: " + question.result);
        if (graded) {
            ScormProcessSetValue("cmi.interactions." + nextIndex + ".weighting", question.weight);
            // console.log("cmi.interactions." + nextIndex + ".weighting: " + question.weight);
        }
    }
    if (graded) {
        let score = Math.round((totalEarned / totalPossible) * 100);
        ScormProcessSetValue("cmi.core.score.raw", score);
        ScormProcessSetValue("cmi.core.score.min", "0");
        ScormProcessSetValue("cmi.core.score.max", "100");
        // console.log(" score:" + score);
    }
}
function ScormProcessGetValue(element) {
    var result;

    if (initialized == false || finishCalled == true) { return; }

    result = API.LMSGetValue(element);

    if (result == "") {

        var errorNumber = API.LMSGetLastError();

        if (errorNumber != "0") {
            var errorString = API.LMSGetErrorString(errorNumber);
            var diagnostic = API.LMSGetDiagnostic(errorNumber);

            var errorDescription = "Number: " + errorNumber + "\nDescription: " + errorString + "\nDiagnostic: " + diagnostic;

            alert("Error - Could not retrieve a value from the LMS.\n\n" + errorDescription);
            return "";
        }
    }
    return result;
}
function ScormProcessSetValue(element, value) {
    var result;

    if (initialized == false || finishCalled == true) { return; }

    result = API.LMSSetValue(element, value);

    if (result == "false") {
        var errorNumber = API.LMSGetLastError();
        var errorString = API.LMSGetErrorString(errorNumber);
        var diagnostic = API.LMSGetDiagnostic(errorNumber);

        var errorDescription = "Number: " + errorNumber + "\nDescription: " + errorString + "\nDiagnostic: " + diagnostic;

        alert("Error - Could not store a value in the LMS.\n\nYour results may not be recorded.\n\n" + errorDescription);
        return;
    }
}
