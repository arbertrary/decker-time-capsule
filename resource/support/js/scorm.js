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

/* *************************************
     API Functions
************************************** */
var startTimeStamp, reachedEnd, bookmark, h, v, f = null,
    processedUnload = false;

function doStart() {
    // Format MC questions
    scormMC();
    /* record the time that the learner started the SCO to report the total time 
       initialize communication with LMS   */
    startTimeStamp = new Date();
    ScormProcessInitialize();

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
function scormMC() {
    var questions = document.getElementsByClassName("scorm-survey");
    let question_num = 1;

    // Insert slide before first question slide that explains test based on grading scheme
    for (let question of questions) {
        question.setAttribute("id", question_num.toString());
        question_num += 1;

        const answerDivs = question.getElementsByTagName("li");
        let defBorder = answerDivs[0].style.border;

        // color the chosen answer(s)
        let answer_num = 0;
        for (let ad of answerDivs) {
            ad.addEventListener("click", function () {
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
            answer_num += 1;
        }
    }
}
function FormatChoiceResponse(value) {
    var newValue = new String(value);
    newValue = newValue.replace(/\W/g, "_");
    newValue = newValue.replace(/^_/, "");
    return newValue;
}
function Question(id, type, selectedAnswers, correctAnswers, possiblePoints, points) {
    this.id = id;
    this.type = type;
    this.selectedAnswers = selectedAnswers;
    this.correctAnswers = correctAnswers;
    this.possiblePoints = possiblePoints;
    this.points = points;
}
function gradeScormMC() {
    document.getElementById("submitButton").style.pointerEvents = "none";
    const questions = document.getElementsByClassName("scorm-survey");
    var questionArray = [];
    var gradingScheme = document.getElementById("slideContent").getAttribute("data-grading-scheme");
    var totalPossible = 0; var totalEarned = 0;

    for (let question of questions) {
        var points = 0;
        var chosen, answerText;
        var correct = true;
        var selectedAnswers = []; var correctAnswers = [];
        var questionID = question.id;
        const allAnswers = question.getElementsByClassName("answer");
        var possiblePoints;

        // disable answer, get learner response and correct response
        for (let answer of allAnswers) {                                        // for each answer  
            answer.parentElement.style.pointerEvents = "none";                  // deactivate answer button  
            answerText = FormatChoiceResponse(answer.firstElementChild.innerHTML);
            var p = document.createElement('p');
            p.style.color = "#009933";
            var t = document.createTextNode('');
            p.appendChild(t);
            question.parentElement.insertBefore(p, question.parentElement.childNodes[0]);
            chosen = answer.parentElement.classList.contains("selected");

            // tally points for answers
            if (answer.classList.contains("lft")) {             // if correct answer
                correctAnswers.push(answerText);
                if (chosen) {                                   // if correct and chosen
                    selectedAnswers.push(answerText);
                    if (gradingScheme == "BV1" || gradingScheme == "BV2" || gradingScheme == "BV3") {
                        points++;
                    }
                } else {                                        // if correct and not chosen
                    if (gradingScheme == "BV1" || gradingScheme == "BV3") {
                        points--;
                    }
                }
            } else {                                            // if incorrect answer
                if (chosen) {                                   // if incorrect and chosen
                    selectedAnswers.push(answerText);
                    if (gradingScheme == "BV1" || gradingScheme == "BV3") {
                        points--;
                    }
                } else {                                        // if incorrect and not chosen
                    if (gradingScheme == "BV1" || gradingScheme == "BV2") {
                        points++;
                    }
                }
            }
        }
        // provide feedback to learner
        // possible earned points varies by grading scheme
        if (gradingScheme == "single" || gradingScheme == "BV4") {
            possiblePoints = 1;
            if (selectedAnswers.length == correctAnswers.length) {
                for (var i = 0; i < selectedAnswers.length; i++) {
                    if (selectedAnswers[i] !== correctAnswers[i]) { correct = false; }
                }
            } else { correct = false; }

            if (correct) {
                t.nodeValue = "Correct";
                points = 1;
            } else {
                t.nodeValue = "Incorrect";
                p.style.color = "#cc0000";
            }
        } else if (gradingScheme == "BV1" || gradingScheme == "BV2") {
            possiblePoints = allAnswers.length;
            if (points < 0) { points = 0; }
            t.nodeValue = "You received " + points + " out of " + possiblePoints + " possible points.";
        } else {
            possiblePoints = correctAnswers.length;
            if (points < 0) { points = 0; }
            t.nodeValue = "You received " + points + " out of " + possiblePoints + " possible points.";
        }
        totalEarned += points;
        totalPossible += possiblePoints;
        questionArray.push(new Question(questionID, "choice", selectedAnswers, correctAnswers, possiblePoints, points));
    }
    var submitMessage = document.createElement('p');
    submitMessage.style.color = "#009933";
    submitMessage.innerHTML = "Your answers have been submitted. Your score is " +
        totalEarned + " out of " + totalPossible +
        " possible points. You may now close the window.";
    document.getElementById("submitSlide").appendChild(submitMessage);
    // Submit quiz to LMS
    RecordTest(questionArray, totalEarned, totalPossible);
}
function RecordTest(questions, totalEarned, totalPossible) {
    for (let question of questions) {
        var nextIndex = ScormProcessGetValue("cmi.interactions._count");
        ScormProcessSetValue("cmi.interactions." + nextIndex + ".id", question.id);
        ScormProcessSetValue("cmi.interactions." + nextIndex + ".type", question.type);
        ScormProcessSetValue("cmi.interactions." + nextIndex + ".student_response", question.selectedAnswers);
        ScormProcessSetValue("cmi.interactions." + nextIndex + ".weighting", question.points);
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