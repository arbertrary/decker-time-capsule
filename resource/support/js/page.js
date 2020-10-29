
function initPage() {
    addSourceCodeLabels();
    addBootstrapTableClasses();
    reloadWindow();
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

/**
 *  Reload on change machinery
 */
function reloadWindow() {
    if (location.hostname == "localhost" || location.hostname == "0.0.0.0") {
      var socket = new WebSocket("ws://" + location.host + "/reload");
      socket.onmessage = (event) => {
        if (event.data.startsWith("reload!")) 
          window.location.reload();
      };
    };
  }