/* *************************************
    SCORM 1.2 API Discovery Algorithm
************************************** */
var findAPITries = 0;
function findAPI(win) {
    // Check to see if the window (win) contains the API
    // if the window (win) does not contain the API and
    // the window (win) has a parent window and the parent window
    // is not the same as the window (win)
    while ((win.API == null) && (win.parent != null) && (win.parent != win)) {
        // increment the number of findAPITries
        findAPITries++;

        // Note: 7 is an arbitrary number, but should be more than sufficient
        if (findAPITries > 7) {
            alert("Error finding API -- too deeply nested.");
            return null;
        }

        // set the variable that represents the window being
        // being searched to be the parent of the current window
        // then search for the API again
        win = win.parent;
    }
    return win.API;
}
function getAPI() {
    // start by looking for the API in the current window
    var theAPI = findAPI(window);

    // if the API is null (could not be found in the current window)
    // and the current window has an opener window
    if ((theAPI == null) && (window.opener != null) && (typeof (window.opener) != "undefined")) {
        // try to find the API in the current window’s opener
        theAPI = findAPI(window.opener);
    }
    // if the API has not been found
    if (theAPI == null) {
        // Alert the user that the API Adapter could not be found
        alert("Unable to find an API adapter");
    }
    return theAPI;
}

/* *************************************
    SCORM LMS Functions
************************************** */

var finishCalled = false;
var initialized = false;
var API = null;

function ScormProcessInitialize() {
    var result;
    API = getAPI();

    if (API == null) {
        alert("ERROR - Could not establish a connection with WUECampus.\n\nYour results may not be recorded.");
        return;
    }

    result = API.LMSInitialize("");

    if (result == "false") {
        var errorNumber = API.LMSGetLastError();
        var errorString = API.LMSGetErrorString(errorNumber);
        var diagnostic = API.LMSGetDiagnostic(errorNumber);

        var errorDescription = "Number: " + errorNumber + "\nDescription: " + errorString + "\nDiagnostic: " + diagnostic;

        alert("Error - Could not initialize communication with WUECampus.\n\nYour results may not be recorded.\n\n" + errorDescription);
        return;
    }
    initialized = true;
}
function ScormProcessFinish() {
    var result;

    //Don't terminate if we haven't initialized or if we've already terminated
    if (initialized == false || finishCalled == true) { return; }

    result = API.LMSFinish("");

    finishCalled = true;

    if (result == "false") {
        var errorNumber = API.LMSGetLastError();
        var errorString = API.LMSGetErrorString(errorNumber);
        var diagnostic = API.LMSGetDiagnostic(errorNumber);

        var errorDescription = "Number: " + errorNumber + "\nDescription: " + errorString + "\nDiagnostic: " + diagnostic;

        alert("Error - Could not terminate communication with the LMS.\n\nYour results may not be recorded.\n\n" + errorDescription);
        return;
    }
}
function ScormProcessGetValue(element, checkError) {
    var result;

    if (initialized == false || finishCalled == true) { return; }

    result = API.LMSGetValue(element);

    if (checkError == true && result == "") {

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

/* *************************************
     API Functions
************************************** */
class Question {
    constructor(type, selectedAnswer, correctAnswer, weight, result) {
        this.type = type;
        this.selectedAnswer = selectedAnswer;
        this.correctAnswer = correctAnswer;
        this.weight = weight;
        this.result = result;
    }
}

var initialMatches, startTimeStamp, reachedEnd, bookmark;
var h, v, f = null; var comments;
var totalPossible = 0; var totalEarned = 0;
var processedUnload = false;
var questionArray = [];
var totalEarned = 0; var totalPossible = 0;

function getMaxPoints() {
    let pointTags = document.querySelectorAll('[data-points]');
    for (let point of pointTags) {
        totalPossible += Number(point.getAttribute('data-points'));
    }
    if (document.getElementById('maxPoints')) {
        document.getElementById('maxPoints').innerHTML = totalPossible.toString();
    }
}
function doStart() {
    /* record the time that the learner started the SCO to report the total time 
       initialize communication with LMS   */
    startTimeStamp = new Date();
    ScormProcessInitialize();

    getMaxPoints()
    /* set the lesson status to incomplete at first launch (if the course is not already completed) */
    var completionStatus = ScormProcessGetValue("cmi.core.lesson_status");
    if (completionStatus == "not attempted") {
        ScormProcessSetValue("cmi.core.lesson_status", "incomplete");
    }

    /* see if the user stored a bookmark previously (don't check for errors
    because cmi.core.lesson_location may not be initialized
    returns string (h,v,f) */
    bookmark = ScormProcessGetValue("cmi.core.lesson_location");

    /* if there is a stored bookmark, sprompt the user to resume from the previous location */
    if (bookmark && confirm("Resume where you previously left off?")) {
        /* find horiz, vert, frag indices */
        var indexStr = bookmark.split(",");
        h = parseInt(indexStr[0], 10) || 0;
        v = parseInt(indexStr[1], 10) || 0;
        f = parseInt(indexStr[2], 10) || 0;
    }
    else {
        h = 0;
        v = 0;
        f = 0;
    }
    /* save the current location as the bookmark; */
    ScormProcessSetValue("cmi.core.lesson_location", [h, v, f].join());
    Reveal.slide(h, v, f);

    var slides = document.querySelector('.slides');
    if (slides.getAttribute('data-graded') == "true") {
        slides.removeAttribute("onbeforeunload");
    }
}
function doUnload() {
    console.log("Scorm has ended.");
    /* don't call this function twice */
    if (processedUnload == true) { return; }
    processedUnload = true;

    /* record the session time */
    var endTimeStamp = new Date();
    var totalMilliseconds = (endTimeStamp.getTime() - startTimeStamp.getTime());
    var scormTime = ConvertMilliSecondsToSCORMTime(totalMilliseconds, false);
    ScormProcessSetValue("cmi.core.session_time", scormTime);

    /* save the current location as the bookmark; */
    var currentSlide = document.querySelector('#slideContent>.slide.present');
    var indices = Reveal.getIndices(currentSlide);
    h = indices.h || 0;
    v = indices.v || 0;
    f = indices.f || 0;
    ScormProcessSetValue("cmi.core.lesson_location", [h, v, f].join());

    /* record interaction data */
    submitScorm();

    /* course is complete when last slide is reached
    prompt the user if exit before course complete */
    reachedEnd = Reveal.isLastSlide();

    if (reachedEnd) {
        ScormProcessSetValue("cmi.core.lesson_status", "completed");
        ScormProcessSetValue("cmi.core.exit", "");
    }
    else if (confirm("Would you like to save your progress to resume later?")) {
        ScormProcessSetValue("cmi.core.exit", "suspend");
    }
    else {
        ScormProcessSetValue("cmi.core.exit", "");
    }
    ScormProcessFinish();
}
function ZeroPad(intNum, intNumDigits) {
    var strTemp;
    var intLen;
    var i;

    strTemp = new String(intNum);
    intLen = strTemp.length;

    if (intLen > intNumDigits) {
        strTemp = strTemp.substr(0, intNumDigits);
    }
    else {
        for (i = intLen; i < intNumDigits; i++) {
            strTemp = "0" + strTemp;
        }
    }
    return strTemp;
}
function ConvertMilliSecondsToSCORMTime(intTotalMilliseconds, blnIncludeFraction) {
    var intHours;
    var intMinutes;
    var intSeconds;
    var intMilliseconds;
    var intHundredths;
    var strCMITimeSpan;

    if (blnIncludeFraction == null || blnIncludeFraction == undefined) {
        blnIncludeFraction = true;
    }

    /* extract time parts */
    intMilliseconds = intTotalMilliseconds % 1000;
    intSeconds = ((intTotalMilliseconds - intMilliseconds) / 1000) % 60;
    intMinutes = ((intTotalMilliseconds - intMilliseconds - (intSeconds * 1000)) / 60000) % 60;
    intHours = (intTotalMilliseconds - intMilliseconds - (intSeconds * 1000) - (intMinutes * 60000)) / 3600000;

    /*
    deal with exceptional case when content used a huge amount of time and interpreted CMITimstamp 
    to allow a number of intMinutes and seconds greater than 60 i.e. 9999:99:99.99 instead of 9999:60:60:99
    note - this case is permissable under SCORM, but will be exceptionally rare
    */

    if (intHours == 10000) {
        intHours = 9999;
        intMinutes = (intTotalMilliseconds - (intHours * 3600000)) / 60000;
        if (intMinutes == 100) { intMinutes = 99; }
        intMinutes = Math.floor(intMinutes);

        intSeconds = (intTotalMilliseconds - (intHours * 3600000) - (intMinutes * 60000)) / 1000;
        if (intSeconds == 100) { intSeconds = 99; }
        intSeconds = Math.floor(intSeconds);

        intMilliseconds = (intTotalMilliseconds - (intHours * 3600000) - (intMinutes * 60000) - (intSeconds * 1000));
    }

    /* drop the extra precision from the milliseconds */
    intHundredths = Math.floor(intMilliseconds / 10);

    /* put in padding 0's and concatinate to get the proper format */
    strCMITimeSpan = ZeroPad(intHours, 4) + ":" + ZeroPad(intMinutes, 2) + ":" + ZeroPad(intSeconds, 2);

    if (blnIncludeFraction) { strCMITimeSpan += "." + intHundredths; }

    /* check for case where total milliseconds is greater than max supported by strCMITimeSpan */
    if (intHours > 9999) {
        strCMITimeSpan = "9999:99:99";

        if (blnIncludeFraction) {
            strCMITimeSpan += ".99";
        }
    }
    return strCMITimeSpan;
}
function FormatChoiceResponse(value) {
    var newValue = new String(value);
    newValue = newValue.replace(/^_/, "");
    newValue = newValue.replace(/(?=.)(^\[)?((\\n)+)?(\])?(\\n)/, "");
    return newValue;
}
function scormMC() {
    var graded = document.querySelector('.slides').getAttribute('data-graded');
    var surveys = document.getElementsByClassName("survey");
    for (let survey of surveys) {
        let totalWeight = Number(survey.parentElement.getAttribute('data-points')) || 0;
        let allAnswers = survey.getElementsByTagName("li");
        var selectedAnswers = []; var correctAnswers = []; var correctResponses = []; var correct = 0;
        for (var answer of allAnswers) {
            if (answer.classList.contains("selected")) {
                selectedAnswers.push(FormatChoiceResponse(answer.querySelector('p').innerHTML));
            } else { continue; }
        }
        var earned;
        if (selectedAnswers.length == 0) {
            if (graded) {
                earned = 0;
            } else { continue; }
        }
        for (let answer of allAnswers) {
            answer.style.pointerEvents = "none";
            const tooltips = answer.getElementsByClassName("tooltip");
            for (let tooltip of tooltips) {
                tooltip.style.display = "inline";
            }
            var answer_div = answer.querySelector(".answer");
            if (answer_div.classList.contains("right")) {
                correctAnswers.push(FormatChoiceResponse(answer.querySelector('p').innerHTML));
                if (answer.classList.contains("selected")) {
                    correctResponses.push(answer);
                    answer.style.backgroundColor = "rgb(151, 255, 122)";
                    correct++;
                } else { answer.style.backgroundColor = "rgb(250, 121, 121)"; }
            } else {
                if (!answer.classList.contains("selected")) {
                    answer.style.backgroundColor = "rgb(250, 121, 121)";
                } else {
                    correctResponses.push(answer);
                    answer.style.backgroundColor = "rgb(151, 255, 122)";
                    correct++;
                }
            }
        }

        earned = (selectedAnswers.length == allAnswers.length) ? 0 : Math.round((correctResponses.length / allAnswers.length) * totalWeight);
        totalEarned += earned;
        var result = correct.toString() + "/" + allAnswers.length.toString();
        questionArray.push(new Question("choice", selectedAnswers, correctAnswers, earned, result));
        // console.log("pushing choice, earned: " + earned + ", result: " + result);
        // console.log(" selected: " + selectedAnswers);
        // console.log(" correctAnswers: " + correctAnswers);
        // console.log(" correctResponses: " + JSON.stringify(correctResponses));
    }
}
function scormBlank() {
    var texts = document.getElementsByClassName("blankText");
    for (let text of texts) {
        var inputs = text.getElementsByClassName("blankInput");
        var selects = text.getElementsByClassName("blankSelect");
        var totalPoints = Number(text.closest("section").getAttribute('data-points'));
        var weight = (totalPoints / (inputs.length + selects.length));
        for (let input of inputs) {
            let result = null;
            let correctAnswer = input.getAttribute("answer").toLowerCase().trim();
            let selectedAnswer = input.value.toLowerCase().trim();
            if (selectedAnswer == "") {
                continue;
            }
            if (selectedAnswer == correctAnswer) {
                input.style.backgroundColor = "rgb(151, 255, 122)";
                result = "correct";
                totalEarned += weight;
            } else {
                input.style.backgroundColor = "rgb(250, 121, 121)";
                result = "wrong";
            }
            questionArray.push(new Question("fill-in", selectedAnswer, correctAnswer, weight, result));
            // console.log("pushing new blank fill in, weight: " + weight + ", " + " result: " + result);
            // console.log(" selected: " + selectedAnswer.toString());
            // console.log(" correct: " + correctAnswer.toString());
        }
        for (var select of selects) {
            let result = null;
            let correctAnswer = "";
            for (var o of select.options) {
                if (o.getAttribute("answer") == "true") {
                    correctAnswer = o.value;
                }
            }
            let selectedAnswer = select.options[select.selectedIndex].value;
            if (selectedAnswer == " ") {
                continue;
            }
            if (selectedAnswer == correctAnswer) {
                select.style.backgroundColor = "rgb(151, 255, 122)";
                result = "correct";
                totalEarned += weight;
            } else {
                select.style.backgroundColor = "rgb(250, 121, 121)";
                result = "wrong";
            }
            questionArray.push(new Question("choice", selectedAnswer, correctAnswer, weight, result));
            // console.log("pushing new blank choice, weight: " + weight + ", result: " + result);
            // console.log(" selected: " + selectedAnswer.toString());
            // console.log(" correct: " + correctAnswer.toString());
        }
    }
}
function scormFree() {
    var inputs = document.getElementsByClassName("freetextInput");
    for (let input of inputs) {
        var result = null;
        var totalWeight = Number(input.closest('section').getAttribute('data-points'));
        var weight = totalWeight / inputs.length;
        var correctAnswer = input.getAttribute("answer").toLowerCase().trim();
        var selectedAnswer = input.value.toLowerCase().trim();
        if (selectedAnswer.startsWith("(") || selectedAnswer == "") {
            continue;
        }
        input.setAttribute("size", input.value.length);
        input.disabled = true;
        this.disabled = true;

        if (selectedAnswer == correctAnswer) {
            input.style.backgroundColor = "rgb(151, 255, 122)";
            result = "correct";
            totalEarned += weight;
        } else {
            input.style.backgroundColor = "rgb(255, 122, 122)";
            result = "wrong";
        }
        questionArray.push(new Question("fill-in", selectedAnswer, correctAnswer, weight, result));
        // console.log("pushing new freetext, weight: " + weight + ", result: " + result);
        // console.log(" selected: " + selectedAnswer.toString());
        // console.log(" correct: " + correctAnswer.toString());
    }
}
function getAns(drop) {
    var dropID = drop.id.replace("drop", "");
    var img = drop.querySelector('img');
    var ans;
    if (img == null) {                                      // if matching has no image to match
        if (drop.querySelector('.draggable') == null) {     // check if answered
            ans = null;
        } else {
            ans = FormatChoiceResponse(drop.innerText);
        }
    } else {                                        // else if image matching
        var imgDiv = drop.querySelector('div');
        if (imgDiv == null) {
            ans = null;
        }
        else {                                // else add img src name to answer
            ans = img.src.replace(/^.*[\\\/]/, '') + ":" + FormatChoiceResponse(imgDiv.innerHTML);
            img.id = "dropImg" + dropID;
        }
    }
    return ans;
}
function scormMatch() {
    var matchings = document.getElementsByClassName("matching");
    for (let match of matchings) {
        match.querySelector('.retryButton').disabled = true;
        var selectedAnswers = []; var correctAnswers = [];
        var weight = Number(match.parentElement.getAttribute('data-points'));
        // totalPossible += weight;
        var dropzones = match.getElementsByClassName("dropzone");
        var ans;
        for (let drop of dropzones) {
            ans = getAns(drop);
            if (ans == null) {
                continue;
            }
            selectedAnswers.push(ans);
        }
        if (selectedAnswers.length == 0) {
            continue;
        }
        for (let drop of dropzones) {
            var first = drop.getElementsByClassName("draggable")[0];
            var dropID = drop.id.replace("drop", "");
            var dropImgID = "dropImg" + dropID;
            var img = drop.querySelector('img');
            if (first.id.replace("drag", "") == dropID) {
                drop.style.backgroundColor = "rgb(151, 255, 122)";
                correctAnswers.push(getAns(drop));

            } else {
                drop.style.backgroundColor = "rgb(255, 122, 122)";
                var dragText = FormatChoiceResponse(document.getElementById("drag" + dropID).innerText);
                var dropText = (img == null) ? drop.childNodes[0].nodeValue.toString() : document.getElementById(dropImgID).src.replace(/^.*[\\\/]/, '');
                var formattedAns = FormatChoiceResponse(dropText + dragText);
                correctAnswers.push(formattedAns);
            }
        }
        var result = "correct";
        for (let sel of selectedAnswers) {
            if (!correctAnswers.includes(sel)) {
                result = "wrong";
            }
        }
        if (result == "correct") { totalEarned += weight; }

        questionArray.push(new Question("matching", selectedAnswers, correctAnswers, weight, result));
        // console.log("pushing new matching, weight: " + weight + ", result: " + result);
        // console.log("pushing " + match.id + ", selected: " + selectedAnswers);
        // console.log("pushing " + match.id + ", correct: " + correctAnswers);
    }
}
function submitScorm() {
    scormMC();
    scormBlank();
    scormFree();
    scormMatch();

    var graded = (document.querySelector('.slides').getAttribute('data-graded') == "true") ? true : false;
    for (let question of questionArray) {
        var nextIndex = ScormProcessGetValue("cmi.interactions._count");
        ScormProcessSetValue("cmi.interactions." + nextIndex + ".id", nextIndex);
        ScormProcessSetValue("cmi.interactions." + nextIndex + ".type", question.type);
        ScormProcessSetValue("cmi.interactions." + nextIndex + ".result", question.result);
        ScormProcessSetValue("cmi.interactions." + nextIndex + ".student_response", question.selectedAnswer);
        ScormProcessSetValue("cmi.interactions." + nextIndex + ".correct_responses.0.pattern", question.correctAnswer);
        console.log("to scorm " + nextIndex + ", " + question.type + ", " + question.result);
        console.log(" selected: " + question.selectedAnswer);
        console.log(" correct: " + question.correctAnswer);
        if (graded) {
            ScormProcessSetValue("cmi.interactions." + nextIndex + ".weighting", question.weight);
            console.log(" weight: " + question.weight);
        }
    }
    if (graded) {
        var score = Math.round((totalEarned / totalPossible) * 100);
        ScormProcessSetValue("cmi.core.score.raw", score);
        ScormProcessSetValue("cmi.core.score.min", "0");
        ScormProcessSetValue("cmi.core.score.max", "100");
        console.log("earned/possible = score " + totalEarned + "/" + totalPossible + " = " + score);

        var submitMessage = document.createElement('p');
        submitMessage.style.color = "#009933";
        submitMessage.innerHTML = "Thank you. Your responses have been submitted. You may now close this window.";
        document.getElementById("submitSlide").appendChild(submitMessage);
    }
}