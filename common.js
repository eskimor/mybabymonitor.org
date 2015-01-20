var RTCPeerConnection = window.mozRTCPeerConnection || window.webkitRTCPeerConnection;
var RTCIceCandidate = window.mozRTCIceCandidate || window.RTCIceCandidate;
var RTCSessionDescription = window.mozRTCSessionDescription || window.RTCSessionDescription;
navigator.getUserMedia = navigator.getUserMedia || navigator.mozGetUserMedia || navigator.webkitGetUserMedia;

function gotLocalIceCandidate(connection) {
    return function(event) {
        logEvent("Got ICE candidate: " + JSON.stringify(event.candidate));
        // Send it to server:
        connection.send(JSON.stringify(
            {
                'ice' : event.candidate
            }
        ))
        logEvent("Sent local ice candidate");
    }           
}
function logEvent(t) {
    $('body').append("<p>" + t +"</p>");
}

function logError(e) {
    $('body').append("<p class='error'>" + e +"</p>");
}

