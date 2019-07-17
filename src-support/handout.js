import $ from "jquery";
require("@fortawesome/fontawesome-free/css/all.css");
require("@fortawesome/fontawesome-free/js/all");
require("bootstrap/dist/css/bootstrap.css");
require("./handout.scss");

document.addEventListener("load", () => {
  $("table").addClass(
    "table table-striped table-bordered table-hover table-condensed table-responsive"
  );
});

window.addEventListener('load', e => {
    const videoTags = document.getElementsByTagName("video");
    for(let video of videoTags) {
        video.addEventListener("loadeddata", evt => {
            let canvas = document.createElement("canvas");
            canvas.className = "video-placeholder";
            canvas.width = 890;
            canvas.height = 500;
            const ctx = canvas.getContext("2d");
            ctx.drawImage(video, 0, 0, 890, 500);
            let videoCaption = document.createElement("p");
            videoCaption.className = "video-caption";
            videoCaption.innerHTML = video.src.match(/([^/]+)$/)[0];
            evt.target.parentNode.insertBefore(canvas, evt.target);
            evt.target.parentNode.insertBefore(videoCaption, evt.target);
        });
    video.load();
    };
});
