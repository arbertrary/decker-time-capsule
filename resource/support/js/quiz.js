var quizModule = {
    quiz: function () {
        multipleChoice();
        blanktextButtons();
        freetextAnswerButtons();
        let initialMatchings = initMatching();
        matchings(initialMatchings);
    }
}

var question_num = 0;
function Question(id, type, selectedAnswers, correctAnswers, points, result) {
    this.id = id;
    this.type = type;
    this.selectedAnswers = selectedAnswers;
    this.correctAnswers = correctAnswers;
    this.points = points;
    this.result = result;
}
function QuizResult(q, e, p) {
    this.questions = q;
    this.earned = e;
    this.possible = p;
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
    }
}
function FormatChoiceResponse(value) {
    var newValue = new String(value);
    newValue = newValue.replace(/^_/, "");
    newValue = newValue.replace(/(?=.)(^\[)?((\\n)+)?(\])?/, "");
    return newValue;
}
function correctMC(button) {
    const survey = button.closest(".survey");
    const answers = survey.getElementsByTagName("li");
    const defBorder = answers[0].style.border;
    let answered = false;
    for (let answer of answers) {
        if (answer.style.border == defBorder) { continue; }
        else { answered = true; }
    }

    if (answered) {
        this.disabled = true;
        for (let answer of answers) {
            var answer_div = answer.querySelector(".answer");
            const is_right = answer_div.classList.contains("right");
            answer.style.backgroundColor = (is_right) ? "#97ff7a" : "#ff7a7a";
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
}
function correctMCScorm() {
    var questionArray = [];
    var possible = 0; var totalPossible = 0; var earned = 0; var totalEarned = 0;

    const questions = document.getElementsByClassName("survey");
    for (let question of questions) {
        question.id = question_num.toString();
        question_num++;
        possible = Number(question.parentElement.getAttribute('data-points'));
        totalPossible += possible;
        let result = null;
        const allAnswers = question.getElementsByClassName("answer");
        var selectedAnswer, correctAnswer;

        // disable answer, get learner response and correct response
        for (let answer of allAnswers) {                                        // for each answer  
            answer.parentElement.style.pointerEvents = "none";                  // deactivate answer button  
            let answerText = FormatChoiceResponse(answer.querySelector('p').innerHTML);
            if (answer.parentElement.classList.contains("selected")) {
                answer.parentElement.style.backgroundColor = "rgb(250, 121, 121)";
                result = "wrong";
                selectedAnswer = answerText;
            }
            if (answer.classList.contains("right")) {
                answer.parentElement.style.backgroundColor = "rgb(151, 255, 122)";
                result = "correct";
                earned = possible;
                correctAnswer = answerText;
            }
        }
        totalEarned += earned;
        totalPossible += possible;
        questionArray.push(new Question(question.id, "choice", selectedAnswer, correctAnswer, earned, result));
    }
    return new QuizResult(questionArray, totalEarned, totalPossible);
}
// function correctMCScormPoints() {
//     var questionArray = [];
//     var possible = 0; var totalPossible = 0; var earned = 0; var totalEarned = 0;

//     const questions = document.getElementsByClassName("survey");
//     var gradingScheme = document.getElementById("slideContent").getAttribute("data-grading-scheme");

//     for (let question of questions) {
//         question.id = question_num.toString();
//         question_num++;
//         var selectedAnswers = []; var correctAnswers = [];
//         const allAnswers = question.getElementsByClassName("answer");
//         let chosen, answerText;
//         let result = "correct";

//         // disable answer, get learner response and correct response
//         for (let answer of allAnswers) {                                        // for each answer  
//             answer.parentElement.style.pointerEvents = "none";                  // deactivate answer button  
//             answerText = FormatChoiceResponse(answer.querySelector('p').innerHTML);
//             var p = document.createElement('p');
//             p.style.color = "#009933";
//             var t = document.createTextNode('');
//             p.appendChild(t);
//             question.parentElement.insertBefore(p, question.parentElement.childNodes[0]);
//             chosen = answer.parentElement.classList.contains("selected");

//             // tally points for answers
//             if (answer.classList.contains("right")) {             // if correct answer
//                 correctAnswers.push(answerText);
//                 if (chosen) {                                   // if correct and chosen
//                     selectedAnswers.push(answerText);
//                     if (gradingScheme == "BV1" || gradingScheme == "BV2" || gradingScheme == "BV3") {
//                         earned++;
//                     }
//                 } else {                                        // if correct and not chosen
//                     if (gradingScheme == "BV1" || gradingScheme == "BV3") {
//                         earned--;
//                     }
//                 }
//             } else {                                            // if incorrect answer
//                 if (chosen) {                                   // if incorrect and chosen
//                     selectedAnswers.push(answerText);
//                     if (gradingScheme == "BV1" || gradingScheme == "BV3") {
//                         earned--;
//                     }
//                 } else {                                        // if incorrect and not chosen
//                     if (gradingScheme == "BV1" || gradingScheme == "BV2") {
//                         earned++;
//                     }
//                 }
//             }
//         }
//         // provide feedback to learner  - possible earned points varies by grading scheme
//         if (gradingScheme == "single" || gradingScheme == "BV4") {
//             possible = 1;
//             if (selectedAnswers.length == correctAnswers.length) {
//                 for (var i = 0; i < selectedAnswers.length; i++) {
//                     if (selectedAnswers[i] !== correctAnswers[i]) { result = "wrong"; }
//                 }
//             } else { result = "wrong"; }

//             if (result == "correct") {
//                 t.nodeValue = "Correct";
//                 earned = 1;
//             } else {
//                 t.nodeValue = "Incorrect";
//                 p.style.color = "#cc0000";
//             }
//         } else if (gradingScheme == "BV1" || gradingScheme == "BV2") {
//             possible = allAnswers.length;
//             if (earned < 0) { earned = 0; }
//             t.nodeValue = "You received " + earned + " out of " + possible + " possible points.";
//         } else {
//             possible = correctAnswers.length;
//             if (earned < 0) { earned = 0; }
//             t.nodeValue = "You received " + earned + " out of " + possible + " possible points.";
//         }
//         totalEarned += earned;
//         totalPossible += possible;
//         questionArray.push(new Question(question.id, "choice", selectedAnswers, correctAnswers, earned, result));
//     }
//     return new QuizResult(questionArray, totalEarned, totalPossible);
// }
/* ************************
     BLANK TEXT QUESTIONS
*************************** */
// For a given blanktext HTML Element returns a Map containing all wrong and correct selects and blanks
function blanktextCorrect(blanktext) {
    var selects = blanktext.getElementsByClassName("blankSelect");
    const blanks = blanktext.getElementsByClassName("blankInput");
    var wrongSelects = []; var correctSelects = []; var wrongBlanks = []; var correctBlanks = [];

    for (let s of selects) {
        const correct = s.options[s.selectedIndex].getAttribute("answer");
        if (correct == "true") {
            correctSelects.push(s);
        }
        else {
            wrongSelects.push(s);
        }
    }

    for (let b of blanks) {
        const correct = b.getAttribute("answer").trim();
        if (b.value.toLowerCase().trim() == correct.toLowerCase()) {
            correctBlanks.push(b);
        }
        else {
            wrongBlanks.push(b);
        }
    }
    const ret = new Map([["correctSelects", correctSelects], ["wrongSelects", wrongSelects], ["wrongBlanks", wrongBlanks], ["correctBlanks", correctBlanks]]);
    return ret;

}
function blanktextButtons() {
    var btButtons = document.getElementsByClassName("btAnswerButton");
    for (let i = 0; i < btButtons.length; i++) {
        const button = btButtons[i];
        button.onclick = function () {
            var blanktext = this.closest(".blankText");

            var results = blanktextCorrect(blanktext);
            var correctSelects = results.get("correctSelects");
            var wrongSelects = results.get("wrongSelects");
            var correctBlanks = results.get("correctBlanks");
            var wrongBlanks = results.get("wrongBlanks");

            for (let w of wrongSelects) {
                w.style.backgroundColor = "rgb(255, 122, 122)";
                for (let o of w.options) {
                    if (o.getAttribute("answer") == "true") {
                        o.textContent += " ✓";
                    } else {
                        o.textContent += " ✗";
                    }
                }
            }

            for (let c of correctSelects) {
                c.style.backgroundColor = "rgb(151, 255, 122)";
                for (let o of c.options) {
                    if (o.getAttribute("answer") == "true") {
                        o.textContent += " ✓";
                    } else {
                        o.textContent += " ✗";
                    }
                }
            }

            for (let w of wrongBlanks) {
                w.style.backgroundColor = "rgb(255, 122, 122)";
                w.value += " (" + w.getAttribute("answer") + ")";
                w.setAttribute("size", w.value.length);
                w.disabled = true;
            }

            for (let c of correctBlanks) {
                c.style.backgroundColor = "rgb(151, 255, 122)";
                c.setAttribute("size", c.value.length);
                c.disabled = true;
            }
            this.disabled = true;
        }
    }
}
function correctBlanks() {
    var questionArray = [];
    var possible = 0; var totalPossible = 0; var earned = 0; var totalEarned = 0;
    var result;

    var questions = document.getElementsByClassName("blankText");
    for (let question of questions) {
        possible = Number(question.parentElement.getAttribute('data-points'));
        totalPossible += possible;
        // get selected/corrected for blanks
        var inputs = question.getElementsByTagName('input');
        var selects = question.getElementsByTagName('select');
        var individualPoints = possible / (inputs.length + selects.length);
        for (let input of inputs) {
            input.id = question_num.toString();
            question_num++;
            result = "correct";
            input.disabled = true;
            let selectedAnswer = input.value.toLowerCase().trim();
            let correctAnswer = input.getAttribute("answer").toLowerCase().trim();
            if (selectedAnswer == correctAnswer) {
                input.style.backgroundColor = "rgb(151, 255, 122)";
                earned = individualPoints;
                totalEarned += earned;
            } else {
                input.style.backgroundColor = "rgb(250, 121, 121)";
                result = "wrong";
            }
            questionArray.push(new Question(input.id, "fill-in", selectedAnswer, correctAnswer, earned, result));
        }

        // get selected/corrected for selects
        for (let select of selects) {
            select.id = question_num.toString();
            question_num++;
            result = "correct";
            select.disabled = true;
            let selectedAnswer = select.options[select.selectedIndex].value;
            let correctAnswer = "";
            for (let o of select.options) {
                if (o.getAttribute("answer") == "true") {
                    correctAnswer = o.value;
                }
            }
            if (selectedAnswer == correctAnswer) {
                select.style.backgroundColor = "rgb(151, 255, 122)";
                earned = possible;
                totalEarned += earned;
            } else {
                select.style.backgroundColor = "rgb(250, 121, 121)";
                result = "wrong";
            }
            questionArray.push(new Question(select.id, "choice", selectedAnswer, correctAnswer, earned, result));
        }
    }
    return new QuizResult(questionArray, totalEarned, totalPossible);
}
/* ************************
     FREE TEXT QUESTIONS
*************************** */
function freetextAnswerButtons() {
    const answerButtons = document.getElementsByClassName('freetextAnswerButton');
    for (let button of answerButtons) {
        button.onclick = function () {
            var questionField = this.parentElement.getElementsByClassName('freetextInput')[0];
            // Has the user entered anything?
            if (questionField.value) {
                var answer = questionField.getAttribute("answer").trim();
                if (questionField.value.toLowerCase().trim() == answer.toLowerCase()) {
                    questionField.style.backgroundColor = "rgb(151, 255, 122)";
                }
                else {
                    questionField.style.backgroundColor = "rgb(255, 122, 122)";
                    questionField.value += " (" + answer + ")";
                }
                questionField.setAttribute("size", questionField.value.length);
                questionField.disabled = true;
                this.disabled = true;
            }
            else {
                alert("No answer entered!");
                return false;
            }
        }
    }
}
function correctFrees() {
    var questionArray = [];
    var possible = 0; var totalPossible = 0; var earned = 0; var totalEarned = 0;

    var questions = document.getElementsByClassName("freetextQuestion");
    for (let question of questions) {
        possible = Number(question.parentElement.getAttribute('data-points'));
        totalPossible += possible;

        var inputs = question.getElementsByTagName('input');
        var individualPoints = possible / inputs.length;
        for (let input of inputs) {
            input.id = question_num.toString();
            question_num++;
            var result = "correct";
            input.disabled = true;
            let selectedAnswer = input.value.toLowerCase().trim();
            let correctAnswer = input.getAttribute("answer").toLowerCase().trim();
            if (selectedAnswer == correctAnswer) {
                input.style.backgroundColor = "rgb(151, 255, 122)";
                earned = individualPoints;
                totalEarned += earned;
            } else {
                input.style.backgroundColor = "rgb(250, 121, 121)";
                result = "wrong";
            }
            questionArray.push(new Question(input.id, "fill-in", selectedAnswer, correctAnswer, earned, result));
        }
    }
    return new QuizResult(questionArray, totalEarned, totalPossible);
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

    var matches = document.getElementsByClassName("matching");
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
                if (first.id.replace("drag", "") == drop.id.replace("drop", "")) {  // if correct
                    drop.style.backgroundColor = "rgb(151, 255, 122)";
                    first.setAttribute("draggable", "false");
                } else {                                                            // if incorrect
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
        }
    }
}
function correctMatches() {
    const questions = document.getElementsByClassName("matching");
    var questionArray = [];
    var totalPossible = 0; var totalEarned = 0;

    for (let question of questions) {
        question.id = question_num.toString();
        question_num++;
        var selectedAnswers = []; var correctAnswers = [];
        let earned = 0;
        var possible = Number(question.parentElement.getAttribute('data-points'));
        totalPossible += possible;
        var dropzones = question.getElementsByClassName("dropzone");

        // Correct match pairs
        for (let drop of dropzones) {
            var first = drop.getElementsByClassName("draggable")[0];
            var dropID = drop.id.replace("drop", "");
            var img = drop.querySelector('img');
            var dropImgID = "dropImg" + dropID;
            selectedAnswers.push(getAns(drop));
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
        }
        var result = "correct";
        for (var i = 0; i < selectedAnswers.length; i++) {                  // check if all matches are correct
            if (selectedAnswers[i] !== correctAnswers[i]) {
                result = "wrong";
            }
        }
        if (result == "correct") {
            earned = possible;
            totalEarned += earned;
        }
        questionArray.push(new Question(question.id, "matching", selectedAnswers, correctAnswers, earned, result));
    }
    return new QuizResult(questionArray, totalEarned, totalPossible);
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
    document.getElementById("submitButton").style.pointerEvents = "none";
    let mcResults = correctMCScorm();
    let btResults = correctBlanks();
    let ftResults = correctFrees();
    let matchResults = correctMatches();
    let allQuestions = mcResults.questions + btResults.questions + ftResults.questions + matchResults.questions;
    let totalEarned = mcResults.earned + btResults.earned + ftResults.earned + matchResults.earned;
    let totalPossible = mcResults.possible + btResults.possible + ftResults.possible + matchResults.possible;
    let score = ((totalEarned / totalPossible) * 100).toPrecision(2);

    // add to final slide
    let submitMessage = document.createElement('p');
    submitMessage.style.color = "#009933";
    submitMessage.innerHTML = "Your answers have been submitted. Your score is " + score + "%.";
    document.getElementById("submitSlide").appendChild(submitMessage);

    RecordTest(allQuestions, totalEarned, totalPossible);
}
function RecordTest(questions, totalEarned, totalPossible) {
    for (let question of questions) {
        var nextIndex = ScormProcessGetValue("cmi.interactions._count");
        ScormProcessSetValue("cmi.interactions." + nextIndex + ".id", question.id);
        ScormProcessSetValue("cmi.interactions." + nextIndex + ".type", question.type);
        ScormProcessSetValue("cmi.interactions." + nextIndex + ".student_response", question.selectedAnswers);
        ScormProcessSetValue("cmi.interactions." + nextIndex + ".weighting", question.points);
        ScormProcessSetValue("cmi.interactions." + nextIndex + ".result", question.result);
        ScormProcessSetValue("cmi.interactions." + nextIndex + ".correct_responses.0.pattern", question.correctAnswers);
    }
    ScormProcessSetValue("cmi.core.score.raw", totalEarned);
    ScormProcessSetValue("cmi.core.score.min", "0");
    ScormProcessSetValue("cmi.core.score.max", totalPossible);

    var score = (totalEarned / totalPossible) * 100;
    var passingGrade = parseInt(document.getElementById("slideContent").getAttribute("data-grade"));
    if (score >= passingGrade) {
        ScormProcessSetValue("cmi.core.lesson_status", "passed");
    } else {
        ScormProcessSetValue("cmi.core.lesson_status", "failed");
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
