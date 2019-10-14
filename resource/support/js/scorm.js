const Reveal = require('../vendor/reveal/js/reveal');
const fs = require('fs');

/* *************************************
    SCORM 1.2 API Discovery Algorithm
************************************** */

var findAPITries = 0;
 
function findAPI(win) {
   // Check to see if the window (win) contains the API
   // if the window (win) does not contain the API and
   // the window (win) has a parent window and the parent window
   // is not the same as the window (win)
   while ( (win.API == null) && (win.parent != null) && (win.parent != win) ) {
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
   if ( (theAPI == null) && (window.opener != null) && (typeof(window.opener) != "undefined") ) {
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
    if (initialized == false || finishCalled == true){return;}
    
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
    
    if (initialized == false || finishCalled == true){return;}
    
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
    
    if (initialized == false || finishCalled == true){return;}
    
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

var startTimeStamp = null,
    processedUnload = false,
    bookmark = null,
    indexh = null, indexv = null, indexf = null;

//Create function handlers for the loading and unloading of the page
function doStart() {
    /* record the time that the learner started the SCO to report the total time */
    startTimeStamp = new Date();
    
    /* initialize communication with the LMS */
    ScormProcessInitialize();
    
    /* set the lesson status to incomplete at first launch (if the course is not already completed) */
    var completionStatus = ScormProcessGetValue("cmi.core.lesson_status");
    if (completionStatus == "not attempted"){
            ScormProcessSetValue("cmi.core.lesson_status", "incomplete"); }
    
    /* see if the user stored a bookmark previously (don't check for errors
    because cmi.core.lesson_location may not be initialized
    returns string (h,v,f) */
    bookmark = ScormProcessGetValue("cmi.core.lesson_location");

    /* if there is a stored bookmark, sprompt the user to resume from the previous location */
    if (bookmark && confirm("Would you like to resume from where you previously left off?")) {
            /* find horiz, vert, frag indices */
            var indexStr = bookmark.split(",");
            indexh = parseInt(indexStr[0], 10) || 0;
            indexv = parseInt(indexStr[1], 10) || 0;
            indexf = parseInt(indexStr[2], 10) || 0; }
    else {
            indexh = 0;
            indexv = 0;
            indexf = 0; }
    /* save the current location as the bookmark; */
    ScormProcessSetValue("cmi.core.lesson_location", [indexh, indexv, indexf].join());

    /* the course is considered complete when the last page is reached */
    if (Reveal.isLastSlide() && Reveal.isLastVerticalSlide()){
            ScormProcessSetValue("cmi.core.lesson_status", "completed"); }
    Reveal.slide(indexh, indexv, indexf); 
}
function ZeroPad(intNum, intNumDigits) {
    var strTemp;
    var intLen;
    var i;
    
    strTemp = new String(intNum);
    intLen = strTemp.length;
    
    if (intLen > intNumDigits){
            strTemp = strTemp.substr(0,intNumDigits);
    }
    else{
            for (i=intLen; i<intNumDigits; i++){
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

    if (blnIncludeFraction == null || blnIncludeFraction == undefined){
            blnIncludeFraction = true; }

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

    if (blnIncludeFraction){ strCMITimeSpan += "." + intHundredths; }

    /* check for case where total milliseconds is greater than max supported by strCMITimeSpan */
    if (intHours > 9999) {
            strCMITimeSpan = "9999:99:99";

            if (blnIncludeFraction){
                    strCMITimeSpan += ".99";
            }
    }
    return strCMITimeSpan;
}
function doUnload(pressedExit) {
    /* don't call this function twice */
    if (processedUnload == true){return;}
    processedUnload = true;

    /* record the session time */
    var endTimeStamp = new Date();
    var totalMilliseconds = (endTimeStamp.getTime() - startTimeStamp.getTime());
    var scormTime = ConvertMilliSecondsToSCORMTime(totalMilliseconds, false);
    ScormProcessSetValue("cmi.core.session_time", scormTime);

    /* save the current location as the bookmark; */
    var indices = Reveal.getCurrentSlide().getIndices();
    indexh = indices.h;
    indexv = indices.v;
    indexf = indices.f;
    ScormProcessSetValue("cmi.core.lesson_location", [indexh, indexv, indexf].join());

    /* course is complete when last slide is reached
    prompt the user if exit before course complete */
    if (Reveal.isLastSlide() && Reveal.isLastVerticalSlide()){
            ScormProcessSetValue("cmi.core.lesson_status", "completed");
            ScormProcessSetValue("cmi.core.exit", ""); }
    else if (confirm("Would you like to save your progress to resume later?")) {
            ScormProcessSetValue("cmi.core.exit", "suspend"); }
    else {
            ScormProcessSetValue("cmi.core.exit", ""); }
    ScormProcessFinish();
} 

//Used to record the results of a test; passes in score as a percentage
function RecordTest(score){
    ScormProcessSetValue("cmi.core.score.raw", score);
    ScormProcessSetValue("cmi.core.score.min", "0");
    ScormProcessSetValue("cmi.core.score.max", "100");
    
    //if we get a test result, set the lesson status to passed/failed instead of completed
    //consider 70% to be passing
    if (score >= 70){
        ScormProcessSetValue("cmi.core.lesson_status", "passed");
    }
    else{
        ScormProcessSetValue("cmi.core.lesson_status", "failed");
    }
}