
function initHandout() {
    currentDate();
    addSourceCodeLabels();
    addBootstrapTableClasses();
}

function currentDate() {
    var date = document.querySelector(".date");
    if (!date) return;
    var dateString = date.textContent.trim();
  
    var today = new Date().toISOString().substr(0, 10);
  
    if (dateString === "today") {
      date.textContent = today;
    }
}

function addSourceCodeLabels() {
    $("div.sourceCode[label]").each(function () {
        $("<div/>")
            .addClass("language-label")
            .text($(this).attr("label"))
            .prependTo($(this).children('pre'));
    });
}

function addBootstrapTableClasses() {
    $("table").addClass(
        "table table-sm table-responsive"
    );
}