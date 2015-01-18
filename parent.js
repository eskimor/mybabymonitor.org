var RTCPeerConnection = window.mozRTCPeerConnection || window.webkitRTCPeerConnection;
var RTCIceCandidate = window.mozRTCIceCandidate || window.RTCIceCandidate;
var RTCSessionDescription = window.mozRTCSessionDescription || window.RTCSessionDescription;
navigator.getUserMedia = navigator.getUserMedia || navigator.mozGetUserMedia || navigator.webkitGetUserMedia;
window.AudioContext = window.AudioContext || window.webkitAudioContext;



function logEvent(t) {
    $('body').append("<p>" + t +"</p>");
}


$(document).ready(
    function(){
       //  Set up volume:
        var ctx = new window.AudioContext();
        
        
        logEvent("Document loaded.");
        var servers = null;
        peerConnection = new RTCPeerConnection(servers);
        peerConnection.onicecandidate = gotIceCandidate;
        peerConnection.onaddstream = gotStream;
        function retrieveIceCandidate() {
            $.get('@{BabyIceCandidateR}'
                  , function (data, status) {
                      logEvent("Received ice (" + data + "): " + status);
                      var pdata = JSON.parse(data);
                      if(pdata) {
                          peerConnection.addIceCandidate(new RTCIceCandidate(pdata));
                          logEvent("Added ICE candidate.")
                          retrieveIceCandidate();
                      }
                  }
                 )
        }
        retrieveIceCandidate();
        $.get('@{BabyOfferDescriptionR}'
              , function (data, status) {
                  logEvent("Received description (" + data + "): " + status);
                  peerConnection.setRemoteDescription(new RTCSessionDescription(JSON.parse(data)));
                  peerConnection.createAnswer(
                      gotDescription
                      , function(error) {
                          logEvent("Error happend when creating answer: " + error);
                      }
                  );
              }
             )
        function gotStream(event){
            logEvent("Received remote stream! JUHU! (" + event.stream.id + ")");
            var source = ctx.createMediaStreamSource(event.stream);
            var gainNode = ctx.createGain();
            gainNode.gain.value = 10;
            source.connect(gainNode);
            gainNode.connect(ctx.destination);
            $("#remoteVideo").attr('src', URL.createObjectURL(ctx.destination));
        }
        function gotIceCandidate(event){
            logEvent("Got ICE candidate: \n" + JSON.stringify(event.candidate) + "typeof(event): " + typeof(event));
            // Send it to baby:
            $.post('@{ParentIceCandidateR}'
                   , {
                       "data" : JSON.stringify(event.candidate)
                   }
                   , function(data, status) {
                       logEvent("Posted ICE candidate ( " + data + "): " + status);
                   }
                  )
        }
        function gotDescription(description) {
            logEvent("Got local description: " + JSON.stringify(description));
            peerConnection.setLocalDescription(description);
            $.post('@{ParentOfferDescriptionR}'
                   , {
                       "data" : JSON.stringify(description)
                   }
                   , function(data, status) {
                       logEvent("Sent description (" + JSON.stringify(description) + "): " + status);
                   }
                  )
        }
    }
)

