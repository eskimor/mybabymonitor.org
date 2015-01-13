

if(navigator.mozGetUserMedia) {
    navigator.mozGetUserMedia({video: true, audio: true, toString : function() {
      return "video,audio";
    }
                              }, onSuccess, onError);
    
}
else {
    alert("No mozGetUserMedia!");
}


function onSuccess(stream) {
    var video = document.querySelector('video');
    video.autoplay = true;
    video.src = window.URL.createObjectURL(stream);

}

function onError() {
    alert("Hey - no video ey!");
}
