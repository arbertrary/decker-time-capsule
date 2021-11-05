// speech recog

var SpeechRecognition = undefined;
var SpeechGrammarList = undefined;
var SpeechRecognitionEvent = undefined;

if(window.chrome) {
  SpeechRecognition = SpeechRecognition || webkitSpeechRecognition
  SpeechGrammarList = SpeechGrammarList || webkitSpeechGrammarList
  SpeechRecognitionEvent = SpeechRecognitionEvent || webkitSpeechRecognitionEvent
}

// reference to Reveal deck
let Reveal;

// configuration parameters
let config;
let useGreenScreen;
let gsBackground;
let gsKey;
let gsSimilarity;
let gsSmoothness;
let gsSpill;

// GUI elements
let playPanel, playButton, player;
let recordPanel, recordIndicator, microphoneIndicator, captionIndicator, voiceIndicator, desktopIndicator;
let recordButton, pauseButton, stopButton;
let muteMicButton, captionToggleButton;
let voiceGainSlider, desktopGainSlider;
let cameraPanel, cameraVideo, cameraCanvas;
let transcriptionRow, transcriptionArea, transcriptionButton, downloadTranscriptionButton;

// recording stuff
let blobs;
let recorder;
let stream;
let voiceStream, desktopStream, cameraStream;
let voiceGain, desktopGain;
let volumeMeter;
let micSelect, camSelect;
let micIndicator, camIndicator;
let screenCaptureSize, cameraCaptureSize;

// playback stuff
let explainVideoUrl, explainTimesUrl, explainTimes;

let uiState;

function transition(name) {
  return (_) => uiState.transition(name);
}

// The state of this plugins UI. All possible legal states sre encoded in
// `uiStates`. Each state has set of possible transition actions together with
// the target states defined. For each state change, the DOM elements in
// `signal` have their `data-uistate` attribute set to the name of the current
// state.
class UIState {
  constructor(signal, states) {
    this.signal = signal;
    this.uiStates = states;
    this.state = this.uiStates.INIT;
  }

  is(name) {
    return name == this.state.name;
  }

  in(...names) {
    return names.reduce((a, name) => a || this.is(name), false);
  }

  name() {
    return this.state.name;
  }

  // Performs a named transition. In case anything goes wrong, the state
  // remains unchanged and an error message is logged.
  async transition(name) {
    console.log(`[] state: ${this.state.name}, transition: ${name}`);
    let transition = this.state.transition[name];
    if (!transition) {
      console.log("[] no transition named: " + name);
      return;
    }

    // A transition action may cancel a transition by returning false.
    try {
      if (transition.action && !(await transition.action())) {
        console.log("[] transition aborted by action: " + name);
        return;
      }
    } catch (e) {
      console.log("[] transition aborted by exception in action: " + e);
      return;
    }

    // The state exit action is meant ot clean up after a state is left.
    try {
      this.state.exit && this.state.exit();
    } catch (e) {
      console.log("[] transition aborted by exception in exit: " + e);
      return;
    }

    if (this.uiStates[transition.next]) {
      // Switch to the new state and signal all elements.
      this.state = this.uiStates[transition.next];
      for (let element of this.signal) {
        element.setAttribute("data-uistate", this.state.name);
      }
    } else {
      console.log("[] no state named: " + transition.next);
    }
    console.log(`[] current state: ${this.state.name}`);
  }
}

// is the user generating a PDF?
const printMode = /print-pdf/gi.test(window.location.search);

// GUI helper (uses named parameters)
function createElement({ type, id, classes, title, parent, onclick = null }) {
  let e = document.createElement(type);
  if (id) e.id = id;
  if (classes) e.className = classes;
  if (title) e.title = title;
  if (title) e.setAttribute("aria-label", title);
  if (parent) parent.appendChild(e);
  if (onclick) e.addEventListener("click", onclick);
  return e;
}

// Derives path and basename for explain data files from the document
// location.
function deckUrlBase() {
  let path = location.pathname;
  return path.substring(0, path.lastIndexOf("-"));
}

// Derives the explain video url from the document location.
function deckVideoUrl() {
  return deckUrlBase() + "-recording.mp4";
}

// Derives the explain times url from the document location.
function deckTimesUrl() {
  return deckUrlBase() + "-times.json";
}

//Derives the ttv url from the document location.
function deckTranscriptUrl() {
  return deckUrlBase() + "-recording.ttv";
}

// Derives the basenam for explain data downloads from the document location.
function videoFilenameBase() {
  const pathname = window.location.pathname;
  let filename = pathname.substring(pathname.lastIndexOf("/") + 1);
  filename = filename.substring(0, filename.lastIndexOf("-"));
  return filename;
}

// Navigates Reveal to the indexed slide in the explainTimes array.
function goToSlide(index) {
  if (explainTimes[index]) {
    let slideId = explainTimes[index].slideId;
    var indices = Reveal.getIndices(document.getElementById(slideId));
    Reveal.slide(indices.h, indices.v);
  }
}

function goToSlideId(slideId) {
  var indices = Reveal.getIndices(document.getElementById(slideId));
  Reveal.slide(indices.h, indices.v);
}

// Jumps the video tp the in-time timestamp stored at index.
function jumpToTime(index) {
  if (explainTimes[index]) {
    player.currentTime(explainTimes[index].timeIn);
  }
}

// Looks up the index of the current Reveal slide in the explainTimes array.
function currentRevealSlideIndex() {
  let slideId = Reveal.getCurrentSlide().id;
  return explainTimes.findIndex((i) => i.slideId === slideId);
}

// Looks up the index of the current slide in the video.
function currentVideoSlideIndex() {
  let time = player.currentTime();
  return explainTimes.findIndex((i) => i.timeIn <= time && time <= i.timeOut);
}

// Jumps the video to the in-time of the next slide.
function next() {
  jumpToTime(currentVideoSlideIndex() + 1);
}

// Jumps the video to the in-time of the previous slide.
function prev() {
  jumpToTime(currentVideoSlideIndex() - 1);
}

// Stops the video and navigates Reveal to the current slide.
function stop() {
  player.pause();
  goToSlide(currentVideoSlideIndex());
  return true;
}

// Starts the video at the current Reveal slide.
function play() {
  let revealIndex = currentRevealSlideIndex();
  let videoIndex = Math.max(0, currentVideoSlideIndex());
  // Only jump the video if not on the correct slide. Otherwise just continue
  // playing.
  if (videoIndex != revealIndex) {
    jumpToTime(revealIndex);
  }

  player.play();

  // we have to focus the player, otherwise keyboard events are sent to the slide
  // we have to delay this a bit, since first the playButton gets the focus due
  // to the mouse click.
  setTimeout(() => {
    player.focus();
  }, 100);

  return true;
}

// Manages recording time stamps of Reval slide changes. For each slide shown
// the time interval is stored. Slides can appear many times in a video.
// Navigation can be random.
class Timing {
  constructor() {
    this.timeIntervals = [];
    this.pauseDuration = 0;
  }

  // Establishes starting time of the recording and records the first slide.
  start() {
    this.startTime = Date.now();
    this.previousSlide = null;
    this.record();
  }

  // Accumulates amount of time pausing.
  pause() {
    this.pauseStart = Date.now();
    this.pauseSlideId = Reveal.getCurrentSlide().id;
  }

  // Resumes passing time. Restore presentation to slide at which pause was
  // initiated.
  resume() {
    this.pauseDuration += Date.now() - this.pauseStart;
    goToSlideId(this.pauseSlideId);
  }

  // Calculates the video stamp stamp for right now.
  timeStamp() {
    return String((Date.now() - this.startTime - this.pauseDuration) / 1000);
  }

  // Records a time stamp.
  record() {
    let time = this.timeStamp();

    if (this.previousSlide) this.previousSlide.timeOut = time;

    let slideId = Reveal.getCurrentSlide().id;
    this.previousSlide = {
      slideId: slideId,
      timeIn: time,
    };

    this.timeIntervals.push(this.previousSlide);
  }

  // Finishes time stamp recording ans returns a Blob containing the data
  // encoded in JSON.
  finish() {
    if (this.previousSlide) this.previousSlide.timeOut = this.timeStamp();
    let json = JSON.stringify(this.timeIntervals, null, 4);
    return new Blob([json], {
      type: "application/json",
    });
  }
}

async function captureScreen() {
  const recWidth = config && config.recWidth ? config.recWidth : undefined;
  const recHeight = config && config.recHeight ? config.recHeight : undefined;

  // get display stream
  console.log("get display stream (" + recWidth + "x" + recHeight + ")");
  desktopStream = await navigator.mediaDevices.getDisplayMedia({
    video: {
      frameRate: 30,
      width: recWidth,
      height: recHeight,
      cursor: "always",
      resizeMode: "crop-and-scale",
    },
    audio: true,
  });

  let video = desktopStream.getVideoTracks()[0].getSettings();
  console.log("display stream size: ", video.width, video.height);
  screenCaptureSize.textContent = `${video.width}x${video.height}`;

  if (desktopStream.getAudioTracks().length > 0) {
    let label = desktopStream.getAudioTracks()[0].label;
    desktopIndicator.title = label;
    desktopGainSlider.disabled = false;
  } else {
    desktopIndicator.removeAttribute("title");
    desktopGainSlider.disabled = true;
  }

  // if merged stream exists already (i.e., we are updating a stream),
  // then merge with existing streams
  if (stream) mergeStreams();
}

async function captureMicrophone() {
  console.log("get voice stream");
  console.log("mic id: " + micSelect.value);

  voiceStream = await navigator.mediaDevices.getUserMedia({
    video: false,
    audio: {
      deviceId: micSelect.value ? { exact: micSelect.value } : undefined,
      echoCancellation: false,
      noiseSuppression: true,
    },
  });

  // if mic capture succeeded...
  if (voiceStream.getAudioTracks().length > 0) {
    // ...update GUI
    const selectedMicrophone = voiceStream.getAudioTracks()[0].label;
    voiceIndicator.title = selectedMicrophone;
    micIndicator.title = selectedMicrophone;
    voiceGainSlider.disabled = false;
    micSelect.selectedIndex = -1;
    for (let i = 0; i < micSelect.options.length; i++) {
      if (micSelect.options[i].text == selectedMicrophone) {
        micSelect.selectedIndex = i;
        break;
      }
    }
    // ...remember selected mic
    if (micSelect.selectedIndex != -1) {
      // store label, since ID changes after reboot
      localStorage.setItem("decker-microphone", selectedMicrophone);
    }
  }
  // if mic capture failed...
  else {
    voiceIndicator.removeAttribute("title");
    micIndicator.removeAttribute("title");
    voiceGainSlider.disabled = true;
    micSelect.value = null;
  }

  // if merged stream exists already (i.e., we are updating a stream),
  // then merge with existing streams
  if (stream) mergeStreams();
}

async function captureCamera() {
  const camWidth = config && config.camWidth ? config.camWidth : undefined;
  const camHeight = config && config.camHeight ? config.camHeight : undefined;

  console.log("get camera stream (" + camWidth + "x" + camHeight + ")");
  console.log("cam id: " + camSelect.value);

  // get camera stream
  cameraStream = await navigator.mediaDevices.getUserMedia({
    video: {
      deviceId: camSelect.value ? { exact: camSelect.value } : undefined,
      width: camWidth,
      height: camHeight,
      frameRate: { max: 30 },
    },
    audio: false,
  });

  // if camera capture succeeded...
  if (cameraStream.getVideoTracks().length > 0) {
    // ...update GUI
    const selectedCamera = cameraStream.getVideoTracks()[0].label;
    const cameraSettings = cameraStream.getVideoTracks()[0].getSettings();
    cameraCaptureSize.textContent = `${cameraSettings.width}x${cameraSettings.height}`;
    camIndicator.title = selectedCamera;
    camSelect.selectedIndex = -1;
    for (let i = 0; i < camSelect.options.length; i++) {
      if (camSelect.options[i].text == selectedCamera) {
        camSelect.selectedIndex = i;
        break;
      }
    }
    // ...remember selected camera
    if (camSelect.selectedIndex != -1) {
      // store label, since ID changes after reboot
      localStorage.setItem("decker-camera", selectedCamera);
    }

    // ...connect camera to video element
    if (cameraPanel.classList.contains("visible")) {
      cameraVideo.pause();
      cameraVideo.srcObject = cameraStream;
      cameraVideo.play();
    } else {
      // cameraVideo.srcObject = cameraStream;
    }
  }
  // if camera capture failed...
  else {
    camIndicator.removeAttribute("title");
  }

  // if merged stream exists already (i.e., we are updating a stream),
  // then merge with existing streams
  if (stream) mergeStreams();
}

function mergeStreams() {
  const tracks = [
    ...desktopStream.getVideoTracks(),
    ...mergeAudioStreams(desktopStream, voiceStream),
  ];
  stream = new MediaStream(tracks);

  // inform user when tracks get lost
  stream.getTracks().forEach((track) => {
    track.onended = () => {
      alert("VideoRecording: Track " + track.label + " has ended.");
      uiState.transition("cancel");
    };
  });
}

let module_should_caption = false;
let module_speechRecognition = undefined;
let module_transcript = undefined;
let module_restart_on_end = undefined;
let module_gracefulStop = false;
let module_transcription_start_time = undefined;

function toggleCaptioning() {
  module_should_caption = !module_should_caption;
  updateCaptionButton();
  updateCaptionIndicatior();
}

function updateCaptionIndicatior() {
  if(module_should_caption) {
    captionIndicator.dataset.state = "captioning";
  } else {
    captionIndicator.dataset.state = "";
  }
}

function updateCaptionButton() {
  captionToggleButton.classList.remove("captioning");
  if(module_should_caption) {
    captionToggleButton.classList.add("captioning");
  }
}

/**
 * Instantiates the speech recognition module and sets its parameters.
 */
function setupSpeechRecognition() {
  if(SpeechRecognition) {
    let speechRecognition = new SpeechRecognition();
    if(!module_transcript) module_transcript = [];
    speechRecognition.continuous = true;
    speechRecognition.interimResults = true;
    speechRecognition.onstart = onTranscriptionStart;
    speechRecognition.onresult = onTranscriptResult;
    speechRecognition.onerror = onTranscriptError;
    speechRecognition.onend = onTranscriptEnd;
    module_speechRecognition = speechRecognition;
  }
}

/**
 * Adds text to the transcript and resets the timings.
 * @param text 
 */
function addToTranscript(text) {
  if(text) {
    var curTime = recorder.timing.timeStamp();
    module_transcript.push({ "startTime": module_transcription_start_time , "endTime": curTime, "text": text });
    module_transcription_start_time = curTime;
  }
}

/**
 * SpeechRecognition callback.
 */
function onTranscriptionStart() {
  console.log("started a new transcription session");
}

/**
 * Checks if the result is final and adds it to the transcript.
 * @param {*} event 
 */
function onTranscriptResult(event) {
  for(var i = event.resultIndex; i < event.results.length; i++) {
    if(event.results[i][0].confidence > 0.4) {
      if(event.results[i].isFinal) {
        addToTranscript(event.results[i][0].transcript);
      }
    }
  }
}

/**
 * Logs the error and forces a retry on the recognition. Is followed by an
 * onend event.
 * @param {*} event 
 */
function onTranscriptError(event) {
  console.error("[SPEECH RECOGNITION]: ", event.message ? event.message : event.error);
  if((event.error == "no-speech") || (event.error == "audio-capture") || (event.error == "network") || (event.error == "bad-grammar")) {
    // Was part of thttps://github.com/MidCamp/live-captioning but doesn't matter for us.
    // Leaving this here.
  }
}

/**
 * If the transcription ended before we ended the recording we want to restart
 * the speech recognition.
 */
function onTranscriptEnd() {
  if(module_gracefulStop) { //If we WANT it to end while still recording.
    module_gracefulStop = false;
    return;
  }
  if(module_restart_on_end || uiState.is("RECORDING")) {
    module_restart_on_end = false;
    module_speechRecognition.start();
  }
}

/**
 * Writes the transcription to the transcription area.
 */
function writeTranscription() {
  console.log(module_transcript);
  transcriptionArea.value = formatTranscriptTimeStamped(module_transcript, "webVTT");
}

//Modified from https://github.com/MidCamp/live-captioning
function formatTimeString(timeString) {
  let time = parseFloat(timeString);

  let seconds = Math.floor(time);
  let milliseconds = Math.floor((time - seconds) * 1000);
  console.log(milliseconds);
  let minutes = Math.floor(seconds/60);
  let hours = Math.floor(minutes/60);
  let days = Math.floor(hours/24);
  let millisecondsSeparator = ".";

  hours = hours-(days*24);
  minutes = minutes-(days*24*60)-(hours*60);
  seconds = seconds-(days*24*60*60)-(hours*60*60)-(minutes*60);

  return ((hours < 10) ? '0' : '') + hours + 
          ((minutes < 10) ? ':0' : ':') + minutes  + 
          ((seconds < 10) ? ':0' : ':') + seconds + 
          millisecondsSeparator + String(milliseconds).padStart(3,'0').substring(0, 3);
}

//Taken from https://github.com/MidCamp/live-captioning
function formatTranscriptTimeStamped(transcript, format){
  if(transcript) {
    var output = "";
    if (format === "webVTT") {
      output += "WEBVTT\n\n";
    }
    for (var i = 0; i < transcript.length; ++i) {
      output += i+1 + "\n"; // This is not neccessary and might make editing the result more difficult.
      output += formatTimeString(transcript[i].startTime) + " --> " + formatTimeString(transcript[i].endTime) + "\n";
      output += transcript[i].text + "\n\n";
    }
    return output;
  } else {
    return "No transcript available";
  }
}

/**
 * Load data from the localStorage.
 * 
 * @param {*} key 
 * @returns 
 */
function loadFromLocalStorage(key) {
  var savedJSON;

  if (localStorage) {
    try {
      savedJSON = JSON.parse(localStorage.getItem(key));
    } catch(e) {}
  }

  if (typeof savedJSON !== 'undefined') {
    return savedJSON;
  }
}

/**
* Save the data to localStorage.
*
* @param key  - local storage key to save the data under
* @param data - the data to save
*/
function saveToLocalStorage(key, data) {
  if (data && localStorage) {
    if (typeof data === "object"){
      localStorage.setItem(key, JSON.stringify(data));
    } else  {
      localStorage.setItem(key, data);
    }
  }
}


async function setupRecorder() {
  try {
    stream = null;

    // capture video/audio stream of desktop signal
    await captureScreen();

    // capture audio stream of microphone
    await captureMicrophone();

    // merge desktop and microphone streams into one stream to be recorded
    mergeStreams();

    setupSpeechRecognition();

    // setup shaders for greenscreen (has to be done before captureCamera())
    if (useGreenScreen) {
      setupGreenScreen();
    }

    // capture video stream of webcam
    await captureCamera();

    recordButton.disabled = undefined;
    pauseButton.disabled = true;
    stopButton.disabled = true;

    return true;
  } catch (e) {
    console.error(e);
    alert(
      `Recording setup failed.\n${e.message}\nRecording only works on Chrome. Also, the deck must be accessed via a URL that starts with either of \n\n- http://localhost\n- https://`
    );
  }
}

function download(blob, name) {
  let url = URL.createObjectURL(blob);
  let anchor = document.createElement("a");
  anchor.href = url;
  anchor.download = name;

  document.body.appendChild(anchor);
  anchor.click();
  document.body.removeChild(anchor);
}

function startRecording() {
  // clear blobs array
  blobs = [];

  // setup recorder
  recorder = new MediaRecorder(stream, {
    mimeType: 'video/webm; codecs=h264"',
  });

  recorder.ondataavailable = (e) => blobs.push(e.data);

  let recordSlideChange = () => recorder.timing.record();

  recorder.onstart = () => {
    console.log("[] recorder started");
    Reveal.slide(0);
    captionToggleButton.disabled = true;
    recorder.timing = new Timing();
    recorder.timing.start();
    module_transcript = [];
    if(module_should_caption) {
      module_transcription_start_time = recorder.timing.timeStamp();
    }
    if(module_speechRecognition && module_should_caption) {
      module_speechRecognition.start();
    }
    Reveal.addEventListener("slidechanged", recordSlideChange);

    updateRecordIndicator();
  };

  function upload(...files) {
    console.log("[] about to upload: ", files);
    let formData = new FormData();
    for (let file of files) {
      formData.append(file.filename, file.data);
    }
    return fetch("/upload", { method: "POST", body: formData })
      .then((r) => r.ok)
      .catch((e) => {
        console.log("[] cannot upload form data to: " + "/upload" + ", " + e);
        return false;
      });
  }

  recorder.onstop = async () => {
    console.log("[] recorder stopped");
    captionToggleButton.disabled = false;
    let vblob = new Blob(blobs, { type: "video/webm" });
    let tblob = recorder.timing.finish();
    if(module_should_caption && module_speechRecognition) {
      module_gracefulStop = true;
      module_speechRecognition.stop();
    }
    writeTranscription();
    let text = transcriptionArea.value;
    var vttblob = new Blob([text], { type: "vtt" });

    try {
      let exists = await resourceExists(deckTimesUrl());
      if (!exists || confirm("Really overwrite existing recording?")) {
        await upload(
          { data: vblob, filename: deckUrlBase() + "-recording.webm" },
          { data: tblob, filename: deckTimesUrl() },
          { data: vttblob, filename: deckUrlBase() + "-recording.vtt" }
        );
      }
    } catch (e) {
      console.err(
        `[] FAILED to upload ${tblob.size} bytes to ${deckTimesUrl()}`
      );
      console.err(
        `[] FAILED to upload ${vblob.size} bytes to ${
          deckUrlBase() + "-recording.webm"
        }`
      );
    } finally {
      download(vblob, videoFilenameBase() + "-recording.webm");
      download(tblob, videoFilenameBase() + "-times.json");
      download(vttblob, videoFilenameBase() + "-recording.vtt");
    }

    Reveal.removeEventListener("slidechanged", recordSlideChange);
    recorder = null;
    stream = null;

    saveToLocalStorage("transcript", module_transcript);
    updateRecordIndicator();
  };

  recorder.onpause = () => {
    recorder.timing.pause();
    if(module_should_caption && module_speechRecognition) {
      module_gracefulStop = true;
      module_speechRecognition.stop();
    }
    updateRecordIndicator();
  };

  recorder.onresume = () => {
    recorder.timing.resume();
    if(module_should_caption && module_speechRecognition) {
      module_speechRecognition.start();
    }
    updateRecordIndicator();
  };

  recorder.onerror = (evt) => {
    alert("VideoRecording Error: " + evt.name);
    uiState.transition("cancel");
  };
  recorder.start();
  recordButton.disabled = true;
  pauseButton.disabled = undefined;
  stopButton.disabled = undefined;
  micSelect.disabled = true;
  camSelect.disabled = true;
  return true;
}

function pauseRecording() {
  recorder.pause();
  if(module_speechRecognition) {
    module_speechRecognition.stop();
  }
  recordButton.disabled = true;
  pauseButton.disabled = undefined;
  stopButton.disabled = undefined;
  return true;
}

function resumeRecording() {
  recorder.resume();
  if(module_speechRecognition) {
    module_speechRecognition.start();
  }
  recordButton.disabled = true;
  pauseButton.disabled = undefined;
  stopButton.disabled = undefined;
  return true;
}

function stopRecording() {
  recorder.stop();
  if(module_speechRecognition) {
    module_speechRecognition.stop();
  }
  stream.getTracks().forEach((s) => s.stop());
  recordButton.disabled = undefined;
  pauseButton.disabled = true;
  stopButton.disabled = true;
  micSelect.disabled = undefined;
  camSelect.disabled = undefined;
  return true;
}

function createAudioMeter(audioContext, meter) {
  let analyser = audioContext.createAnalyser();
  analyser.fftSize = 512;
  analyser.buffer = new Float32Array(analyser.frequencyBinCount);
  analyser.fade = 0.9;
  analyser.vol = 0;
  analyser.db = 0;

  analyser.draw = function () {
    // get data from analyser node
    analyser.getFloatTimeDomainData(analyser.buffer);
    // compute maximum
    let vol = analyser.buffer.reduce((previous, current) => {
      return Math.max(previous, current);
    });
    // fade out with previous frame
    analyser.vol = Math.max(vol, analyser.vol * analyser.fade);
    // convert to dB, assign to meter
    analyser.db = 20 * Math.log10(Math.max(0.001, analyser.vol));
    meter.value = analyser.db;
    // draw again next frame
    window.requestAnimationFrame(analyser.draw);
  };

  return analyser;
}

// adapted from https://paul.kinlan.me/screen-recorderrecording-microphone-and-the-desktop-audio-at-the-same-time/
const mergeAudioStreams = (desktopStream, voiceStream) => {
  const context = new AudioContext();
  let hasDesktop = false;
  let hasVoice = false;

  // create audio meter
  let audioMeter = createAudioMeter(context, volumeMeter);

  if (desktopStream && desktopStream.getAudioTracks().length > 0) {
    hasDesktop = true;
    // connect gain to slider
    desktopGain = context.createGain();
    desktopGain.gain.value = desktopGainSlider.value;
    desktopGainSlider.gain = desktopGain;
    // connect source->gain->meter
    const source1 = context.createMediaStreamSource(desktopStream);
    source1.connect(desktopGain).connect(audioMeter);
  }

  if (voiceStream && voiceStream.getAudioTracks().length > 0) {
    hasVoice = true;
    // connect gain to slider
    voiceGain = context.createGain();
    voiceGain.gain.value = voiceGainSlider.value;
    voiceGainSlider.gain = voiceGain;
    // connect source->gain->meter
    const source2 = context.createMediaStreamSource(voiceStream);
    source2.connect(voiceGain).connect(audioMeter);
  }

  // connect source(s)->gain(s)->meter->destination
  const destination = context.createMediaStreamDestination();
  audioMeter.connect(destination);
  audioMeter.draw();

  return hasDesktop || hasVoice ? destination.stream.getAudioTracks() : [];
};

function createPlayerGUI() {
  playPanel = createElement({
    type: "div",
    id: "explain-panel",
    parent: document.body,
  });

  playButton = createElement({
    type: "button",
    id: "explain-play",
    classes: "fas fa-play decker-button explain",
    title: "Play video recording",
    onclick: transition("play"),
  });

  if(Reveal.hasPlugin("decker-plugins")) {
    let manager = Reveal.getPlugin("decker-plugins");
    manager.placeButton(playButton, "TOP_RIGHT");
  } else {
    document.body.appendChild(playButton);
  }

  let video = createElement({
    type: "video",
    id: "explain-video",
    classes: "video-js",
    parent: playPanel,
  });

  // setup video-js
  player = videojs("explain-video", {
    width: "100%",
    height: "100%",
    controls: true,
    autoplay: false,
    preload: "metadata",
    playbackRates: [0.5, 0.75, 1, 1.25, 1.5, 2],
    controlBar: {
      playToggle: true,
      volumePanel: true,
      currentTimeDisplay: true,
      timeDivider: false,
      durationDisplay: false,
      remainingTimeDisplay: true,
      playbackRateMenuButton: true,
      fullscreenToggle: false,
      pictureInPictureToggle: false,
    },
    userActions: {
      // disable going to fullscreen by double click
      doubleClick: false,
      // our keyboard shortcuts
      hotkeys: function (event) {
        event.stopPropagation();

        switch (event.code) {
          // space or k: play/pause
          case "Space":
          case "KeyK":
            if (this.paused()) this.play();
            else this.pause();
            break;

          // left/right: skip slides
          case "ArrowLeft":
            prev();
            break;
          case "ArrowRight":
            next();
            break;

          // up/down: increase/decrease volume by 5%
          case "ArrowUp":
            this.volume(Math.min(1.0, this.volume() + 0.05));
            break;
          case "ArrowDown":
            this.volume(Math.max(0.0, this.volume() - 0.05));
            break;

          // c: toggle captions
          case "KeyC":
            let tracks = player.textTracks();
            for (let i = 0; i < tracks.length; i++) {
              if (tracks[i].kind === "captions") {
                tracks[i].mode =
                  tracks[i].mode === "showing" ? "disabled" : "showing";
              }
            }
            break;

          // j/l: jump backward/forward by 10sec
          case "KeyJ":
            player.currentTime(player.currentTime() - 10);
            break;
          case "KeyL":
            player.currentTime(player.currentTime() + 10);
            break;

          // m: mute/unmute
          case "KeyM":
            this.muted(!this.muted());
            break;

          // esc: stop and hide video
          case "Escape":
            uiState.transition("stop");
            break;
        }
      },
    },
  });

  // use double tap on left/right part of player to jump backward/forward by 10sec
  let lastTap = null;
  player.on("touchstart", (evt) => {
    const now = Date.now();
    if (lastTap && now - lastTap < 300) {
      // we have a double tap
      const tapX = evt.touches[0].clientX;
      const playerWidth = player.el().clientWidth;
      if (tapX > 0.66 * playerWidth) {
        // right border: skip forward
        player.currentTime(player.currentTime() + 10);
      } else if (tapX < 0.33 * playerWidth) {
        // left border: skip backward
        player.currentTime(player.currentTime() - 10);
      } else {
        // center: play/pause
        if (player.paused()) player.play();
        else player.pause();
      }
    }
    lastTap = now;
  });

  player.on("ended", transition("stop"));

  player.on("error", (_) => {
    console.error(
      'ExplainPlugin: Could not open video "' + explainVideoUrl + '"'
    );
    uiState.transition("stop");
  });

  // get videojs button class
  let Button = videojs.getComponent("Button");

  // register and add close button
  let closeButton = videojs.extend(Button, {
    constructor: function () {
      Button.apply(this, arguments);
      this.addClass("vjs-icon-cancel");
      this.controlText("Close video");
    },
    handleClick: transition("stop"),
  });
  videojs.registerComponent("closeButton", closeButton);
  player.getChild("controlBar").addChild("closeButton", {}, 0);

  // register and add prev button
  let prevButton = videojs.extend(Button, {
    constructor: function () {
      Button.apply(this, arguments);
      this.addClass("vjs-icon-previous-item");
      this.controlText("Jump to previous slide");
    },
    handleClick: function () {
      prev();
    },
  });
  videojs.registerComponent("prevButton", prevButton);
  player.getChild("controlBar").addChild("prevButton", {}, 1);

  // register and add next button
  let nextButton = videojs.extend(Button, {
    constructor: function () {
      Button.apply(this, arguments);
      this.addClass("vjs-icon-next-item");
      this.controlText("Jump to next slide");
    },
    handleClick: function () {
      next();
    },
  });
  videojs.registerComponent("nextButton", nextButton);
  player.getChild("controlBar").addChild("nextButton", {}, 3);
}

async function createRecordingGUI() {
  recordPanel = createElement({
    type: "div",
    id: "record-panel",
    parent: document.body,
  });

  let row;

  row = createElement({
    type: "div",
    classes: "controls-row",
    parent: recordPanel,
  });

  captionIndicator = createElement({
    type: "i",
    id: "caption-indicator",
    classes: "indicator",
    parent: row,
  });

  recordIndicator = createElement({
    type: "i",
    id: "record-indicator",
    classes: "indicator",
    parent: row,
  });

  microphoneIndicator = createElement({
    type: "i",
    id: "microphone-indicator",
    classes: "indicator",
    parent: row,
  });

  row = createElement({
    type: "div",
    classes: "controls-row",
    parent: recordPanel,
  });

  volumeMeter = createElement({
    type: "meter",
    id: "audio-meter",
    parent: row,
  });
  volumeMeter.value = -60;
  volumeMeter.min = -60;
  volumeMeter.low = -20;
  volumeMeter.high = -9;
  volumeMeter.max = 0;

  row = createElement({
    type: "div",
    classes: "controls-row",
    parent: recordPanel,
  });

  voiceIndicator = createElement({
    type: "i",
    classes: "indicator fas fa-microphone",
    parent: row,
  });

  voiceGainSlider = createElement({
    type: "input",
    id: "voice-gain-slider",
    classes: "gain-slider",
    title: "Microphone Audio Gain",
    parent: row,
  });

  row = createElement({
    type: "div",
    classes: "controls-row",
    parent: recordPanel,
  });

  desktopIndicator = createElement({
    type: "i",
    classes: "indicator fas fa-tv",
    parent: row,
  });

  desktopGainSlider = createElement({
    type: "input",
    id: "desktop-gain-slider",
    classes: "gain-slider",
    title: "Desktop Audio Gain",
    parent: row,
  });
  setupGainSlider(desktopGain, desktopGainSlider);

  // mic selector
  row = createElement({
    type: "div",
    classes: "controls-row",
    parent: recordPanel,
  });

  micIndicator = createElement({
    type: "i",
    classes: "indicator fas fa-microphone",
    title: "Select microphone",
    parent: row,
  });

  micSelect = createElement({
    type: "select",
    id: "mic-select",
    classes: "input-select",
    title: "Select microphone",
    parent: row,
  });
  micSelect.onchange = captureMicrophone;

  // camera selector
  row = createElement({
    type: "div",
    classes: "controls-row",
    parent: recordPanel,
  });

  camIndicator = createElement({
    type: "i",
    classes: "indicator fas fa-camera",
    title: "Select camera",
    parent: row,
  });

  camSelect = createElement({
    type: "select",
    id: "cam-select",
    classes: "input-select",
    title: "Select camera",
    parent: row,
  });
  camSelect.onchange = captureCamera;

  row = createElement({
    type: "div",
    classes: "controls-row",
    parent: recordPanel,
  });

  createElement({
    type: "i",
    classes: "indicator fas fa-camera",
    title: "Camera capture size",
    parent: row,
  });

  cameraCaptureSize = createElement({
    type: "span",
    classes: "capture-size",
    parent: row,
  });

  createElement({
    type: "i",
    classes: "indicator fas fa-tv",
    title: "Screen capture size",
    parent: row,
  });

  screenCaptureSize = createElement({
    type: "span",
    classes: "capture-size",
    parent: row,
  });

  // collect list of cameras and microphones
  try {
    const devices = await navigator.mediaDevices.enumerateDevices();
    devices.forEach((device) => {
      switch (device.kind) {
        case "audioinput": {
          const option = document.createElement("option");
          option.value = device.deviceId;
          option.text = device.label || `microphone ${micSelect.length + 1}`;
          micSelect.add(option);
          break;
        }
        case "videoinput": {
          const option = document.createElement("option");
          option.value = device.deviceId;
          option.text = device.label || `camera ${camSelect.length + 1}`;
          camSelect.add(option);
          break;
        }
      }
    });

    // select previously chosen camera
    camSelect.selectedIndex = -1;
    const selectedCamera = localStorage.getItem("decker-camera");
    if (selectedCamera) {
      for (let i = 0; i < camSelect.options.length; i++) {
        if (camSelect.options[i].text == selectedCamera) {
          camSelect.selectedIndex = i;
          break;
        }
      }
    }
    // select previously chosen microphone
    micSelect.selectedIndex = -1;
    const selectedMicrophone = localStorage.getItem("decker-microphone");
    if (selectedMicrophone) {
      for (let i = 0; i < micSelect.options.length; i++) {
        if (micSelect.options[i].text == selectedMicrophone) {
          micSelect.selectedIndex = i;
          break;
        }
      }
    }
  } catch (e) {
    console.log("cannot list microphones and cameras:" + e);
  }

  let toggleRow = createElement({
    type: "div",
    classes: "controls-row",
    parent: recordPanel,
  })

  muteMicButton = createElement({
    type: "button",
    classes: "explain mute-button fas fa-microphone",
    title: "Mute microphone (M)",
    parent: toggleRow,
    onclick: toggleMicrophone,
  })

  captionToggleButton = createElement({
    type: "button",
    classes: "explain caption-button fas fa-closed-captioning",
    title: "Create Captions while recording",
    parent: toggleRow,
    onclick: toggleCaptioning,
  })

  row = createElement({
    type: "div",
    classes: "controls-row",
    parent: recordPanel,
  });

  recordButton = createElement({
    type: "button",
    classes: "explain record-button fas fa-play-circle",
    title: "Start recording",
    parent: row,
    onclick: transition("record"),
  });

  pauseButton = createElement({
    type: "button",
    classes: "explain pause-button fas fa-pause-circle",
    title: "Pause/resume recording",
    parent: row,
    onclick: transition("pause"),
  });

  stopButton = createElement({
    type: "button",
    classes: "explain stop-button fas fa-stop-circle",
    title: "Stop recording",
    parent: row,
    onclick: transition("stop"),
  });

  transcriptionRow = createElement({
    type: "div",
    classes: "controls-row",
    parent: recordPanel,
  });

  transcriptionArea = createElement({
    type: "textarea",
    classes: "explain transcriptarea",
    parent: transcriptionRow,
  });
  transcriptionArea.disabled = true;
  module_transcript = loadFromLocalStorage("transcript");
  writeTranscription();

  transcriptionButton = createElement({
    type: "button",
    classes: "explain transcribe-button fas fa-sync",
    title: "Reload transcription",
    parent: transcriptionRow,
    onclick: () => writeTranscription(),
  })

  downloadTranscriptionButton = createElement({
    type: "button",
    classes: "explain download-button fas fa-download",
    title: "Download transcription",
    parent:transcriptionRow,
    onclick: () => {
      let text = transcriptionArea.value;
      var blob = new Blob([text], { type: "vtt" });
      var a = document.createElement('a');
      a.download = "transcription.vtt";
      a.href = URL.createObjectURL(blob);
      a.dataset.downloadurl = ["vtt", a.download, a.href].join(':');
      a.style.display = "none";
      document.body.appendChild(a);
      a.click();
      document.body.removeChild(a);
      setTimeout(function() { URL.revokeObjectURL(a.href); }, 1500);
    }
  })

  Reveal.addKeyBinding(
    { keyCode: 77, key: "M", description: "Toggle/Mute Microphone" },
    toggleMicrophone
  )

  updateCaptionButton();
  updateCaptionIndicatior();

  setupGainSlider(voiceGain, voiceGainSlider);

}

function setupGainSlider(gain, slider) {
  slider.type = "range";
  slider.min = 0;
  slider.max = 2;
  slider.step = 0.1;
  slider.storage = "decker-" + slider.id;
  slider.value = localStorage.getItem(slider.storage)
    ? localStorage.getItem(slider.storage)
    : 1.0;

  slider.output = createElement({
    type: "output",
    parent: slider.parentElement,
  });

  slider.oninput = function () {
    if (this.gain) this.gain.gain.value = this.value;
    this.output.innerHTML = this.value;
    localStorage.setItem(this.storage, this.value);
    if(this.value === "0") {
      toggleMicButton(false);
      if(module_should_caption && module_speechRecognition && uiState.is("RECORDING")) {
        module_gracefulStop = true;
        module_speechRecognition.stop();
      }
    } else {
      toggleMicButton(true);
      if(module_should_caption && module_speechRecognition && uiState.is("RECORDING")) {
        module_speechRecognition.start();
      }
    }
  };
  slider.oninput(); // call once to set output
}

function toggleMicButton(enabled) {
  if(!muteMicButton) return; //Because this gets called once before it is initialized
  if(!microphoneIndicator) return;
  muteMicButton.classList.remove("off");
  muteMicButton.classList.remove("fa-microphone");
  muteMicButton.classList.remove("fa-microphone-slash");
  microphoneIndicator.dataset.state = "";
  if(!enabled) {
    muteMicButton.classList.add("off");
    muteMicButton.classList.add("fa-microphone-slash");
    microphoneIndicator.dataset.state = "mute";
  } else {
    muteMicButton.classList.add("fa-microphone")
  }
}

function updateRecordIndicator() {
  recordIndicator.dataset.state = recorder ? recorder.state : "";
}

function createCameraGUI() {
  cameraVideo = createElement({
    type: "video",
    id: "camera-video",
    classes: "camera-panel",
    parent: document.body,
  });
  cameraVideo.muted = true; // don't want audio in this stream

  cameraCanvas = createElement({
    type: "canvas",
    id: "camera-canvas",
    classes: "camera-panel",
    parent: document.body,
  });
  if (gsBackground) {
    cameraCanvas.style.backgroundImage = `url('${gsBackground}')`;
    cameraCanvas.style.backgroundSize = "cover";
    cameraCanvas.setAttribute("data-has-background", true);
  } else {
    cameraCanvas.setAttribute("data-has-background", false);
  }

  // camera panel is set to either cameraVideo or cameraCanvas
  if (useGreenScreen) {
    cameraVideo.style.display = "none";
    cameraPanel = cameraCanvas;
  } else {
    cameraPanel = cameraVideo;
  }

  // initialize translation and scaling
  cameraPanel.dragging = false;
  cameraPanel.dx = 0.0;
  cameraPanel.dy = 0.0;
  cameraPanel.scale = 1.0;
  cameraPanel.transform = "";

  // start dragging
  cameraPanel.onmousedown = (e) => {
    if (!cameraPanel.classList.contains("fullscreen")) {
      e.preventDefault();
      cameraPanel.dragging = true;
      cameraPanel.style.cursor = "move";
      cameraPanel.lastX = e.screenX;
      cameraPanel.lastY = e.screenY;

      // translate on mouse move
      cameraPanel.onmousemove = (e) => {
        if (cameraPanel.dragging) {
          const x = e.screenX;
          const y = e.screenY;
          cameraPanel.dx += x - cameraPanel.lastX;
          cameraPanel.dy += y - cameraPanel.lastY;
          cameraPanel.lastX = x;
          cameraPanel.lastY = y;
          cameraPanel.style.transform = `translate(${cameraPanel.dx}px, ${cameraPanel.dy}px) scale(${cameraPanel.scale})`;
        }
      };
    }
  };

  // stop dragging
  cameraPanel.onmouseup = cameraPanel.onmouseleave = (e) => {
    cameraPanel.style.cursor = "";
    cameraPanel.dragging = false;
    cameraPanel.onmousemove = null;
  };

  // use mouse wheel to scale video
  cameraPanel.onmousewheel = (e) => {
    if (!cameraPanel.classList.contains("fullscreen")) {
      cameraPanel.scale += e.deltaY * -0.01;
      cameraPanel.scale = Math.max(0.1, Math.min(10.0, cameraPanel.scale));
      cameraPanel.style.transform = `translate(${cameraPanel.dx}px, ${cameraPanel.dy}px) scale(${cameraPanel.scale})`;
    }
  };

  // use double click on video to toggle fullscreen
  cameraPanel.ondblclick = (e) => {
    if (cameraPanel.classList.toggle("fullscreen")) {
      cameraPanel.transform = cameraPanel.style.transform;
      cameraPanel.style.transform = "";
    } else {
      cameraPanel.style.transform = cameraPanel.transform;
    }
  };
}

// adapted from https://jameshfisher.com/2020/08/11/production-ready-green-screen-in-the-browser/
function setupGreenScreen() {
  const gl = cameraCanvas.getContext("webgl", { premultipliedAlpha: false });

  const vsource = String.raw`attribute vec2 c; 
    void main(void) { 
      gl_Position=vec4(c, 0.0, 1.0); 
    }`;

  const fsource = String.raw`precision mediump float;
    uniform sampler2D tex;
    uniform float texWidth;
    uniform float texHeight;
    uniform vec3  keyColor;
    uniform float similarity;
    uniform float smoothness;
    uniform float spill;

    // From https://github.com/obsproject/obs-studio/blob/master/plugins/obs-filters/data/chroma_key_filter_v2.effect
    vec2 rgb2uv(vec3 rgb) {
      return vec2(
        0.501961 - 0.100644*rgb.r - 0.338572*rgb.g + 0.439216*rgb.b,
        0.501961 + 0.439216*rgb.r - 0.398942*rgb.g - 0.040274*rgb.b
      );
    }

    vec4 ProcessChromaKey(vec2 texCoord) {
      vec4 rgba = texture2D(tex, texCoord);
      float chromaDist = distance(rgb2uv(rgba.rgb), rgb2uv(keyColor));
      float baseMask = chromaDist - similarity;
      float fullMask = pow(clamp(baseMask / smoothness, 0., 1.), 1.5);
      rgba.a = fullMask;
      float spillVal = pow(clamp(baseMask / spill, 0., 1.), 1.5);
      float desat = clamp(rgba.r * 0.2126 + rgba.g * 0.7152 + rgba.b * 0.0722, 0., 1.);
      rgba.rgb = mix(vec3(desat, desat, desat), rgba.rgb, spillVal);
      return rgba;
    }

    void main(void) {
      vec2 texCoord = vec2(gl_FragCoord.x/texWidth, 1.0 - (gl_FragCoord.y/texHeight));
      gl_FragColor = ProcessChromaKey(texCoord);
    }`;

  const vs = gl.createShader(gl.VERTEX_SHADER);
  gl.shaderSource(vs, vsource);
  gl.compileShader(vs);

  const fs = gl.createShader(gl.FRAGMENT_SHADER);
  gl.shaderSource(fs, fsource);
  gl.compileShader(fs);

  const prog = gl.createProgram();
  gl.attachShader(prog, vs);
  gl.attachShader(prog, fs);
  gl.linkProgram(prog);
  gl.useProgram(prog);

  const vb = gl.createBuffer();
  gl.bindBuffer(gl.ARRAY_BUFFER, vb);
  gl.bufferData(
    gl.ARRAY_BUFFER,
    new Float32Array([-1, 1, -1, -1, 1, -1, 1, 1]),
    gl.STATIC_DRAW
  );

  const coordLoc = gl.getAttribLocation(prog, "c");
  gl.vertexAttribPointer(coordLoc, 2, gl.FLOAT, false, 0, 0);
  gl.enableVertexAttribArray(coordLoc);

  gl.activeTexture(gl.TEXTURE0);
  const tex = gl.createTexture();
  gl.bindTexture(gl.TEXTURE_2D, tex);
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);

  const texLoc = gl.getUniformLocation(prog, "tex");
  const texWidthLoc = gl.getUniformLocation(prog, "texWidth");
  const texHeightLoc = gl.getUniformLocation(prog, "texHeight");
  const keyColorLoc = gl.getUniformLocation(prog, "keyColor");
  const similarityLoc = gl.getUniformLocation(prog, "similarity");
  const smoothnessLoc = gl.getUniformLocation(prog, "smoothness");
  const spillLoc = gl.getUniformLocation(prog, "spill");

  function processFrame(now, metadata) {
    if (cameraCanvas.width != metadata.width) {
      cameraCanvas.width = metadata.width;
      cameraCanvas.height = metadata.height;
    }
    gl.viewport(0, 0, metadata.width, metadata.height);
    gl.texImage2D(
      gl.TEXTURE_2D,
      0,
      gl.RGB,
      gl.RGB,
      gl.UNSIGNED_BYTE,
      cameraVideo
    );

    gl.uniform1i(texLoc, 0);
    gl.uniform1f(texWidthLoc, metadata.width);
    gl.uniform1f(texHeightLoc, metadata.height);
    gl.uniform3f(
      keyColorLoc,
      gsKey.r / 255.0,
      gsKey.g / 255.0,
      gsKey.b / 255.0
    );
    gl.uniform1f(similarityLoc, gsSimilarity);
    gl.uniform1f(smoothnessLoc, gsSmoothness);
    gl.uniform1f(spillLoc, gsSpill);

    gl.drawArrays(gl.TRIANGLE_FAN, 0, 4);
    cameraVideo.requestVideoFrameCallback(processFrame);
    // console.log('.');
  }

  cameraVideo.requestVideoFrameCallback(processFrame);
}

function toggleCamera() {
  if (uiState.in("RECORDER_READY", "RECORDER_PAUSED", "RECORDING")) {
    if (cameraPanel.classList.toggle("visible")) {
      if (cameraVideo.srcObject !== cameraStream) {
        cameraVideo.srcObject = cameraStream;
      }
      cameraVideo.play();
    } else {
      cameraVideo.pause();
    }
  } else {
    cameraPanel.classList.remove("visible");
  }
}

let voiceGainBak = 1.0;
function toggleMicrophone() {
  if (voiceGainSlider.value == 0) {
    voiceGainSlider.value = voiceGainBak;
  } else {
    voiceGainBak = voiceGainSlider.value;
    voiceGainSlider.value = 0;
  }
  voiceGainSlider.oninput();
}

async function resourceExists(url) {
  return fetch(url, { method: "HEAD" })
    .then((r) => r.ok)
    .catch((_) => {
      return false;
    });
}

async function fetchResourceJSON(url) {
  return fetch(url)
    .then((r) => r.json())
    .catch((e) => {
      console.log("[] cannot fetch: " + url + ", " + e);
      return null;
    });
}

async function uploadBlob(url, blob) {
  return fetch(url, { method: "PUT", body: blob })
    .then((r) => r.ok)
    .catch((e) => {
      console.log(
        "[] cannot upload " + blob.size + " bytes to: " + url + ", " + e
      );
      return false;
    });
}

async function guardedUploadBlob(url, blob) {
  if (
    (await resourceExists(url)) &&
    !confirm("Really overwrite existing recording?")
  ) {
    return false;
  } else {
    return uploadBlob(url, blob);
  }
}

let zPushCount = 0.0;
let lastZPush = null;

// key event to stop recording if pushed 3 times in quick succession
function maybeStopRecording(evt) {
  // only react on key z/Z
  if (evt.keyCode != 90) return;

  // do not react to alt/ctrl/cmd modifier
  if (evt.altKey || evt.ctrlKey || evt.metaKey) return;

  let now = Date.now();
  if (lastZPush && now - lastZPush < 500) {
    lastZPush = now;
    zPushCount++;
  } else {
    lastZPush = now;
    zPushCount = 1;
  }

  if (zPushCount == 3) {
    switch (uiState.name()) {
      case "RECORDING":
      case "RECORDER_PAUSED":
        uiState.transition("stop");
        break;
    }
  }
}

// key event to toggle recording states
function toggleRecording(evt) {
  // only react on key r/R
  if (evt.keyCode != 82) return;

  // do not react to alt/ctrl/cmd modifier
  if (evt.altKey || evt.ctrlKey || evt.metaKey) return;

  switch (uiState.name()) {
    case "INIT":
    case "PLAYER_READY":
      uiState.transition("setupRecorder");
      break;
    case "RECORDER_READY":
      uiState.transition("record");
      break;
    case "RECORDING":
    case "RECORDER_PAUSED":
      uiState.transition("pause");
      break;
  }
}

async function setupPlayer() {
  const config = Decker.meta.explain;
  explainVideoUrl = config && config.video ? config.video : deckVideoUrl();
  explainTimesUrl = config && config.times ? config.times : deckTimesUrl();
  let videoExists = false,
    timesExists = false;

  try {
    // if in electron app and user specified base url for videos:
    // if times exist locally, we assume the video exists on remote server
    if (window.electronApp && config && config.electronVideoUrl) {
      explainVideoUrl =
        config.electronVideoUrl + videoFilenameBase() + "-recording.mp4";
      videoExists = true;
      timesExists = await resourceExists(explainTimesUrl);
    }
    // in browser: check if video and times exist
    else {
      videoExists = await resourceExists(explainVideoUrl);
      timesExists = await resourceExists(explainTimesUrl);
    }

    if (videoExists && timesExists) {
      explainTimes = await fetchResourceJSON(explainTimesUrl);
      player.src({ type: "video/mp4", src: explainVideoUrl });

      let captionsUrl = explainVideoUrl.replace(".mp4", ".vtt");
      let captionsExist = await resourceExists(captionsUrl);
      if (captionsExist) {
        let captionsOptions = {
          kind: "captions",
          srclang: document.documentElement.lang,
          src: captionsUrl,
        };
        player.addRemoteTextTrack(captionsOptions, false);
      }

      return true;
    } else {
      return false;
    }
  } catch (_) {
    return false;
  }
}

function setupCallbacks() {
  // Reveal ignores key events when modifiers are pressed. We therefore use a "normal" keydown callback.
  // We still add a dummy callback prevent other plugins
  // to use "our" keys and to add our keys to the help menu.
  Reveal.addKeyBinding(
    { keyCode: 82, key: "R", description: "Toggle Recording" },
    toggleRecording
  );
  Reveal.addKeyBinding(
    { keyCode: 90, key: "Z", description: "Stop Recording (Triple Click)" },
    maybeStopRecording
  );
  Reveal.addKeyBinding(
    { keyCode: 86, key: "V", description: "Toggle Camera" },
    toggleCamera
  );

  // Intercept page leave when we are recording
  window.addEventListener("beforeunload", (evt) => {
    if (uiState.in("RECORDER_PAUSED", "RECORDING")) {
      evt.preventDefault();
      evt.returnValue = "We are recording!";
      return evt.returnValue;
    }
  });
}

// export the plugin
const Plugin = {
  id: "explain",

  init: async function (deck) {
    Reveal = deck;

    // don't do anything when exporting to PDF
    if (printMode) return;

    // get config options
    config = Reveal.getConfig().explain || {};
    useGreenScreen = config.useGreenScreen || false;
    gsBackground = config.greenScreenBackground || undefined;
    gsKey = config.greenScreenKey || { r: 0, g: 255, b: 0 };
    gsSimilarity = config.greenScreenSimilarity || 0.4;
    gsSmoothness = config.greenScreenSmoothness || 0.08;
    gsSpill = config.greenScreenSpill || 0.1;

    // setup GUI
    createRecordingGUI();
    createPlayerGUI();
    createCameraGUI();

    setupCallbacks();

    uiState = new UIState([playPanel, recordPanel, playButton, cameraPanel], {
      INIT: {
        name: "INIT",
        transition: {
          setupPlayer: { action: setupPlayer, next: "PLAYER_READY" },
          setupRecorder: { action: setupRecorder, next: "RECORDER_READY" },
        },
      },
      PLAYER_READY: {
        name: "PLAYER_READY",
        transition: {
          play: { action: play, next: "PLAYING" },
          setupRecorder: { action: setupRecorder, next: "RECORDER_READY" },
        },
      },
      PLAYING: {
        name: "PLAYING",
        transition: {
          stop: { action: stop, next: "PLAYER_READY" },
        },
      },
      RECORDER_READY: {
        name: "RECORDER_READY",
        transition: {
          cancel: { action: null, next: "INIT" },
          record: { action: startRecording, next: "RECORDING" },
        },
      },
      RECORDING: {
        name: "RECORDING",
        transition: {
          cancel: { action: stopRecording, next: "INIT" },
          stop: { action: stopRecording, next: "INIT" },
          pause: { action: pauseRecording, next: "RECORDER_PAUSED" },
        },
      },
      RECORDER_PAUSED: {
        name: "RECORDER_PAUSED",
        transition: {
          cancel: { action: stopRecording, next: "INIT" },
          stop: { action: stopRecording, next: "INIT" },
          pause: { action: resumeRecording, next: "RECORDING" },
        },
      },
    });
    // Try to connect to an existing video.
    uiState.transition("setupPlayer");
    addReloadInhibitor(
      () => !uiState.in("RECORDER_READY", "RECORDER_PAUSED", "RECORDING")
    );
  },

  playVideo: play,
  stopVideo: stop,
  isVideoPlaying: () => {
    return uiState.is("PLAYING");
  },
};

export default Plugin;
