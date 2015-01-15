var RTCPeerConnection = window.mozRTCPeerConnection || window.webkitRTCPeerConnection;
var RTCIceCandidate = window.mozRTCIceCandidate || window.RTCIceCandidate;
var RTCSessionDescription = window.mozRTCSessionDescription || window.RTCSessionDescription;
navigator.getUserMedia = navigator.getUserMedia || navigator.mozGetUserMedia || navigator.webkitGetUserMedia;


function logEvent(t) {
    $('body').append("<p>" + t +"</p>");
}


$(document).ready(
    function(){
        logEvent("Document loaded.");
        navigator.getUserMedia({audio:true, video:true}, gotStream,
                               function(error) {
                                   logEvent("getUserMedia error: " + error);
                               }
                              );
        function gotStream(stream){
            logEvent("Received local stream.");
            var servers = null;
            peerConnection = new RTCPeerConnection(servers);
            peerConnection.onicecandidate = gotLocalIceCandidate;
            logEvent("Adding stream ...");
            peerConnection.addStream(stream);
            logEvent("Creating offer ...");
            peerConnection.createOffer(
                function(description) {
                    logEvent("Got description: " + JSON.stringify(description));
                    peerConnection.setLocalDescription(description);
                    $.post('@{BabyOfferDescriptionR}'
                           , {
                               "data" : JSON.stringify(description)
                           }
                           , function (data, status) {
                               logEvent("Posted (" + data + "): " + status);
                               //                               gotStream(stream); // Await new clients
                           }
                          )
                    
                }
                ,
                function(error) {
                    logEvent("Error creating offer: " + error);
                }
            )
            function retrieveIceCandidate() {
                $.get('@{ParentIceCandidateR}'
                      , function(data, status) {
                          logEvent("Got parent ice candidate (" + data + "): " + status);
                          var pdata = JSON.parse(data);
                          if(pdata) {
                              peerConnection.addIceCandidate(new RTCIceCandidate(pdata));
                              retrieveIceCandidate();
                          }
                      }
                     )
            }
            retrieveIceCandidate();
            $.get('@{ParentOfferDescriptionR}'
                  , function(data, status) {
                      logEvent("Got parent description (" + data + "): " + status);
                      peerConnection.setRemoteDescription(new RTCSessionDescription(JSON.parse(data)));
                  }
                 )

        }
        function gotLocalIceCandidate(event){
            logEvent("Got ICE candidate: " + JSON.stringify(event.candidate));
            // Send it to server:
            $.post('@{BabyIceCandidateR}'
                   , {
                       "data" : JSON.stringify(event.candidate)
                   }
                   , function(data, status) {
                       logEvent("Posted ICE candidate ( " + data + "): " + status);
                   }
                  )
        }           
    }
)

