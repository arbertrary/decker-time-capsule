"use strict";

var RevealQuiz = (() => {
    return {
        init: () => { 
            return new Promise(resolve => {
                quizMI();
                quizMC();
                quizIC();
                quizFT();
                resolve();
            });
        }
    }
})();

function quizMC() {
    for (let question of document.querySelectorAll(".qmc,.quiz-mc,.quiz-multiple-choice")) {
        for (let answer of question.getElementsByTagName("li")) {
            // remove tooltip if empty to avoid grey dot
            const tip = answer.querySelector('.tooltip');
            if (tip.childElementCount === 0) { tip.remove(); }
            answer.addEventListener("click", function () {
                const correct = this.classList.contains("correct");
                // toggle answer on click
                this.classList.forEach(c => {
                    c.match(/show-/g) ? this.classList.remove(c) : this.classList.add(correct ? "show-right" : "show-wrong");
                });
            });
        }
    }
}

function quizFT() {
    for (let question of document.querySelectorAll(".qft,.quiz-ft,.quiz-free-text")) {
        const solutions = question.querySelector(".solutionList");
        const input = question.querySelector("input");

        // Listen for enter, delete, backspace in input field
        var buffer = [];
        input.addEventListener("keydown", e => {
            buffer.push(e.key.toLowerCase());
            if (buffer[buffer.length - 1] === buffer[buffer.length - 2]) { return; };
            if (e.code === "Enter") { checkInput() };
            if (e.code === "Backspace" || e.code === "Delete") { resetQuestion() };
        });

        // Check value of input field against solutions
        function checkInput() {
            const checked = checkAnswer(solutions, input.value.toLowerCase().trim());
            input.classList.remove("show-right", "show-wrong");
            input.classList.add(checked.correct ? "show-right" : "show-wrong");

            // Display the tooltip/solution box for any expected answer, correct or incorrect
            input.addEventListener("mouseover", () => {
                if (checked.predef) { solutions.classList.add("solved"); }
            });
            input.addEventListener("mouseout", () => {
                solutions.classList.remove("solved");
            });
        }

        // Add click listeners to solution, reset buttons
        const plain = question.classList.contains('plain') ? true : false;
        const solutionButton = question.querySelector('.solutionButton');
        const resetButton = question.querySelector('.resetButton');
        solutionButton.addEventListener('click', showSolution);
        resetButton.addEventListener('click', resetQuestion);
        question.querySelector('.resetButton').classList.add(plain ? 'disabled' : 'hidden');

        const optList = solutions.getElementsByTagName('li');
        const solutionDiv = question.querySelector('.solutionDiv');

        // Populate solutionDiv to reserve space - hide if fancy style
        for (let c of optList) {
            if (c.classList.contains('correct')) { solutionDiv.appendChild(c.cloneNode(true)); }
        }
        if (!plain) { solutionDiv.classList.add('hidden'); }

        // Handle click of solution button
        function showSolution() {
            if (plain) {
                solutionDiv.classList.add('solved');
                this.classList.add('disabled');
                resetButton.classList.remove('disabled');
            } else {
                solutions.classList.add('solved');
                for (let c of optList) {
                    if (c.classList.contains('correct')) { c.classList.add("solved"); }
                }
                // Hide tooltip box after 3 seconds
                setTimeout(() => {
                    solutions.classList.remove("solved");
                    Array.from(solutions.getElementsByTagName("li")).map(x => {
                        x.classList.remove("solved");
                    });
                }, 3000);
            }
        }

        // Return to original state
        function resetQuestion() {
            for (let c of optList) { c.classList.remove('solved'); }
            solutionDiv.classList.remove('solved');
            input.classList.remove("show-right", "show-wrong");
            input.value = "";
            solutionButton.classList.remove('disabled');
            resetButton.classList.add('disabled');
        }
    }
}

function quizIC() {
    const icQuestions = document.querySelectorAll(".qic,.quiz-ic,.quiz-insert-optList");

    for (let question of icQuestions) {
        const selects = question.getElementsByTagName("select");
        const tipDiv = question.querySelector(".tooltip-div");

        for (let sel of selects) {
            const solutionList = sel.nextElementSibling;

            // Listen for selections - color appropriately
            sel.addEventListener('mouseenter', () => {
                const ind = sel.selectedIndex;
                if (ind) {
                    sel.classList.add('solved');
                    const answers = solutionList.getElementsByTagName('li');
                    const tip = answers.item(sel.selectedIndex - 1).querySelector('.tooltip');
                    const cln = tip.cloneNode(true);
                    tipDiv.appendChild(cln);
                    if (tipDiv.firstElementChild.innerHTML !== "") { 
                        tipDiv.classList.add('solved'); 
                    }
                }
            });
            sel.addEventListener('mouseleave', () => {
                tipDiv.innerHTML = "";
                tipDiv.classList.remove('solved');
                sel.classList.remove('solved');
            })
            sel.addEventListener('change', () => {
                const ind = sel.selectedIndex;
                const answer = sel.options[ind].innerText.toLowerCase().trim();
                const checked = checkAnswer(solutionList, answer);
                sel.classList.remove("show-right", "show-wrong");
                sel.classList.add(checked.correct ? "show-right" : "show-wrong");
            })
        }
    }
}

function quizMI() {
    const miQuestions = document.querySelectorAll(".qmi,.quiz-mi,.quiz-match-items");
    for (let question of miQuestions) {
        shuffleMatchItems(question);
        question.classList.contains('plain') ? buildPlainMatch(question) : buildDragDrop(question);
    }
}

/********************
 * Helper Functions
 ********************/

/**
 * @param {string} answer - The input answer
 * @param {Element} solutionList 
 * 
 * Iterate over solutionList, check if answer is equivalent to at least one correct solution.
 * Returns two booleans - 
 *   correct: whether the given answer is correct
 *   predef: whether the given answer is equivalent to one of the predefined possible answers
 * Predefined answers can be correct or wrong.
 * Tooltip will also show for expected wrong answers!
 */
function checkAnswer(solutionList, answer) {
    const solutions = solutionList.getElementsByTagName("li");

    for (let s of solutions) {
        const is_right = s.classList.contains("correct");
        // Get only the solution text and not the tooltip div
        const solution = s.innerHTML.replace(/(<div)(.|[\r\n])*(<\/div>)/, "").toLowerCase().trim();
        if (answer == solution) {
            s.classList.add("solved");
            return { correct: (is_right ? true : false), predef: true };
        }
    }
    return { correct: false, predef: false };
}

/**
 * Shuffle matchItems so the correct pairings aren't always directly below each other
 * @param {Element} question 
 */
function shuffleMatchItems(question) {

    // Fisher-Yates Shuffle
    const shuffleArray = array => {
        for (var i = array.length - 1; i > 0; i--) {
            var j = Math.floor(Math.random() * (i + 1));
            [array[i], array[j]] = [array[j], array[i]];
        }
        return array;
    }

    const matchItems = question.querySelector(".matchItems");
    const elementsArray = Array.prototype.slice.call(matchItems.getElementsByClassName('matchItem'));

    elementsArray.map(element => { matchItems.removeChild(element) })
    shuffleArray(elementsArray);
    elementsArray.map(element => { matchItems.appendChild(element) });

    const mqArray = Array.prototype.slice.call(matchItems.getElementsByClassName('matchQuestion'));
    mqArray.map(element => { matchItems.removeChild(element) });
    shuffleArray(mqArray);
    mqArray.map(element => { matchItems.appendChild(element) });

}

/**
 * Construct drag and drop listeners for Matching questions
 * @param {Element} question 
 */
function buildDragDrop(question) {
    const dropzones = question.getElementsByClassName("bucket");
    const draggables = question.getElementsByClassName("matchItem");
    const matchItems = question.querySelector(".matchItems");

    matchItems.addEventListener("drop", drop);
    matchItems.addEventListener("dragover", e => e.preventDefault());

    for (var i = 0; i < dropzones.length; i++) {
        dropzones[i].addEventListener("drop", drop);
        dropzones[i].addEventListener("dragover", e => e.preventDefault());

        for (let child of dropzones[i].children) {
            if (!child.classList.contains("matchItem") && child.tagName !== 'P') {
                child.classList.add("draggableChild");
            }
        }
    }

    for (var i = 0; i < draggables.length; i++) {
        draggables[i].addEventListener("dragstart", drag);
    }
    // do not remove draggable from children because ghost image disappears

    const answerButton = question.querySelector(".solutionButton");
    matchingAnswerButton(question, answerButton);
}

/**
 * Correct matching questions on button click
 * @param {Element} question 
 */
function matchingAnswerButton(question, button) {
    button.addEventListener('click', () => {
        const buckets = question.getElementsByClassName("bucket");
        const remainingItems = question.querySelector(".matchItems").children;
        const bucketsDiv = question.querySelector(".buckets");
        const assignedItems = bucketsDiv.getElementsByClassName("matchItem");

        if (assignedItems.length == 0) {
            alert("You haven't assigned any items!");
            return;
        }

        // color remaining items that have not been dragged
        for (let rem of remainingItems) {
            const matchId = rem.getAttribute("data-bucketid");
            rem.classList.remove("show-right", "show-wrong");
            rem.classList.add(matchId == null ? "show-right" : "show-wrong");
        }

        // color remaining items that have been dragged
        for (let bucket of buckets) {
            const droppedItems = bucket.getElementsByClassName("matchItem");
            const bucketId = bucket.getAttribute("data-bucketid");
            for (let matchItem of droppedItems) {
                matchItem.classList.remove("show-right", "show-wrong");

                const matchId = matchItem.getAttribute("data-bucketid");
                matchItem.classList.add(matchId == bucketId ? "show-right" : "show-wrong");
            }
        }
    })
}
function buildPlainMatch(question) {
    const matchItems = question.querySelector('.matchItems');
    const solutionButton = question.querySelector('.solutionButton');

    for (let qmi of matchItems.querySelectorAll(".matchQuestion")) {
        qmi.querySelector('.option').addEventListener('click', function (ev) { 
            showList(this.parentElement.nextElementSibling);
            ev.stopPropagation();
        });
        
        const choices = qmi.querySelectorAll('.option');
        for (let i = 2; i < choices.length; i++) {          // exclude ... in optList and options
            choices[i].addEventListener('click', makeSelection);
        }
    }
    // hide optionList and any other open lists
    function showList(opt) {  
        for (let sh of document.getElementsByClassName('shown')) { sh.classList.remove('shown') };
        opt.classList.add('shown');
        document.addEventListener('click', hideList);
    }
    function makeSelection() {
        let ol = this.parentElement.previousElementSibling;
        this.classList.remove('correct', 'incorrect', 'correct-notSelected');      // allow multiple attempts to solve
        this.parentElement.previousElementSibling.classList.remove('correct', 'incorrect');
        this.classList.toggle('selected');
        if (this.classList.contains('selected')) {
            let cl = this.cloneNode(true);
            ol.appendChild(cl);
            cl.addEventListener('click', function (ev) { 
                showList(this.parentElement.nextElementSibling);
                ev.stopPropagation();
            });
        } else {
            for (let child of ol.children) {
                if (child.innerText === this.innerText) { ol.removeChild(child); }
            }
        }
    }
    function hideList(event) {
        let parentCL = event.target.parentElement.classList;
        if (!parentCL.contains('shown') && !event.target.classList.contains('shown')) {
            question.getElementsByClassName('shown')[0].classList.remove('shown');
            document.removeEventListener('click', hideList);
        }
    }
    solutionButton.addEventListener('click', () => {
        const matches = matchItems.querySelectorAll('.matchQuestion');
        for (let mq of matches) {
            let list = mq.querySelector('.optList');
            let correct = list.previousElementSibling.getAttribute('data-bucketId');
            let allCorrect = []; let allSelected = [];

            for (let l of list.children) {
                allSelected.push(l.textContent);
                l.classList.add(l.getAttribute('data-bucketId') === correct ? 'correct' : 'incorrect');
            }
            allSelected.shift();                            // remove blank response

            let opts = list.nextElementSibling;
            for (let o of opts.children) {
                if (o.getAttribute('data-bucketId') === correct) {
                    allCorrect.push(o.textContent);
                    o.classList.add(o.classList.contains('selected') ? 'correct' : 'correct-notSelected');
                } else if (o.classList.contains('selected')) {
                    o.classList.add('incorrect');
                }
            };

            allSelected.length !== allCorrect.length ? list.classList.add('incorrect') : list.classList.add(JSON.stringify(allSelected.sort()) === JSON.stringify(allCorrect) ? 'correct' : 'incorrect');
        }
    });
}
var elements = [];
function drag(event) {
    // drag the enclosing div and not just the child
    let tar = event.target.classList.contains('.matchItem') ? event.target : event.target.closest('.matchItem');
    
    var index = elements.indexOf(tar);
    if (index == -1) {
        // not already existing in the array, add it now
        elements.push(tar);
        index = elements.length - 1;
    }

    event.dataTransfer.setData('index', index);
    event.dataTransfer.setDragImage(tar, tar.clientWidth / 10, tar.clientHeight / 2);
}
function drop(event) {
    event.preventDefault();
    const element = elements[event.dataTransfer.getData('index')];

    // account for drop inside matchItem, mathjax, img or if bucket header is formatted
    const list = event.target.classList;
    const tag = event.target.tagName;
    switch(true) {
      case list.contains('bucket'):
      case list.contains('matchItems'):
        event.target.appendChild(element);
        break;
      case list.contains('matchItem'):
        event.target.parentElement.appendChild(element);
        break;  
      case (tag === "SPAN"):
      case (tag === "svg"):
      case (tag === "P"):
      case (tag === "IMG"):
      case (tag === "MJX-CONTAINER"):
      case (event.target.parentElement.tagName === "P"):
        let location = event.target.closest('.bucket') ? '.bucket' : '.matchItems';
        event.target.closest(location).appendChild(element);
    }
}

Reveal.registerPlugin( 'quiz-wue', RevealQuiz );