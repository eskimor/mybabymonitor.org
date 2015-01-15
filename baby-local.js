var RTCPeerConnection = window.mozRTCPeerConnection || window.webkitRTCPeerConnection;
var RTCIceCandidate = window.mozRTCIceCandidate || window.RTCIceCandidate;
var RTCSessionDescription = window.mozRTCSessionDescription || window.RTCSessionDescription;
navigator.getUserMedia = navigator.getUserMedia || navigator.mozGetUserMedia || navigator.webkitGetUserMedia;

var localStream, localPeerConnection, remotePeerConnection;

var localVideo = document.getElementById("localVideo");
var remoteVideo = document.getElementById("remoteVideo");

var startButton = document.getElementById("startButton");
var callButton = document.getElementById("callButton");
var hangupButton = document.getElementById("hangupButton");
startButton.disabled = false;
callButton.disabled = true;
hangupButton.disabled = true;
startButton.onclick = start;
callButton.onclick = call;
hangupButton.onclick = hangup;

function serParse(val) {
    JSON.parse(JSON.stringify(val));
}

function trace(text) {
  console.log((performance.now() / 1000).toFixed(3) + ": " + text);
}

function gotStream(stream){
  trace("Received local stream");
  //localVideo.src = URL.createObjectURL(stream);
    $('#localVideo').attr('src', URL.createObjectURL(stream));
  localStream = stream;
  callButton.disabled = false;
}

function start() {
  trace("Requesting local stream");
  startButton.disabled = true;
  navigator.getUserMedia({audio:true, video:true}, gotStream,
    function(error) {
      trace("getUserMedia error: ", error);
    });
}

function call() {
  callButton.disabled = true;
  hangupButton.disabled = false;
  trace("Starting call");

  if (localStream.getVideoTracks().length > 0) {
    trace('Using video device: ' + localStream.getVideoTracks()[0].label);
  }
  if (localStream.getAudioTracks().length > 0) {
    trace('Using audio device: ' + localStream.getAudioTracks()[0].label);
  }

  var servers = null;

  localPeerConnection = new RTCPeerConnection(servers);
  trace("Created local peer connection object localPeerConnection");
  localPeerConnection.onicecandidate = gotLocalIceCandidate;

  remotePeerConnection = new RTCPeerConnection(servers);
  trace("Created remote peer connection object remotePeerConnection");
  remotePeerConnection.onicecandidate = gotRemoteIceCandidate;
  remotePeerConnection.onaddstream = gotRemoteStream;

  localPeerConnection.addStream(localStream);
  trace("Added localStream to localPeerConnection");
  localPeerConnection.createOffer(gotLocalDescription,handleError);
}


function gotLocalDescription(description){
    localPeerConnection.setLocalDescription(description);
    trace("Offer from localPeerConnection: \n" + description.sdp);
    remotePeerConnection.setRemoteDescription(new RTCSessionDescription(JSON.parse(JSON.stringify(description))));
    remotePeerConnection.createAnswer(gotRemoteDescription,handleError);
}

function gotRemoteDescription(description){
  remotePeerConnection.setLocalDescription(description);
    trace("Answer from remotePeerConnection: \n" + serParse(description.sdp));
    localPeerConnection.setRemoteDescription(new RTCSessionDescription(JSON.parse(JSON.stringify(description))));
}

function hangup() {
  trace("Ending call");
  localPeerConnection.close();
  remotePeerConnection.close();
  localPeerConnection = null;
  remotePeerConnection = null;
  hangupButton.disabled = true;
  callButton.disabled = false;
}

function gotRemoteStream(event){
  remoteVideo.src = URL.createObjectURL(event.stream);
  trace("Received remote stream");
}

function gotLocalIceCandidate(event){
  if (event.candidate) {
      remotePeerConnection.addIceCandidate(new RTCIceCandidate(JSON.parse(JSON.stringify(event.candidate))));
    trace("Local ICE candidate: \n" + event.candidate.candidate);
  }
}

function gotRemoteIceCandidate(event){
  if (event.candidate) {
      localPeerConnection.addIceCandidate(new RTCIceCandidate(JSON.parse(JSON.stringify(event.candidate))));
      trace("Remote ICE candidate: \n " + event.candidate.candidate);
  }
}

function handleError(){}

/*
var PeerConnection = window.mozRTCPeerConnection || window.webkitRTCPeerConnection;
var IceCandidate = window.mozRTCIceCandidate || window.RTCIceCandidate;
var SessionDescription = window.mozRTCSessionDescription || window.RTCSessionDescription;
navigator.getUserMedia = navigator.getUserMedia || navigator.mozGetUserMedia || navigator.webkitGetUserMedia;

var pc = new PeerConnection();
pc.onicecandidate = function (e) {
    alert(e);
}

// Helper functions
function endCall() {
  var videos = document.getElementsByTagName("video");
  for (var i = 0; i < videos.length; i++) {
    videos[i].pause();
  }

  pc.close();
}

function error(err) { endCall(); }

var pcL = new PeerConnection();



if(navigator.getUserMedia) {
    navigator.getUserMedia({video: true, audio: true, toString : function() {
      return "video,audio";
    }
                              }, onSuccess, onError);
    
}
else {
    alert("No getUserMedia!");
}


function onSuccess(stream) {
    var video = document.querySelector('video');
    video.autoplay = true;
    video.src = window.URL.createObjectURL(stream);
    pcL.addStream(stream);
    pcL.createOffer(function(offer) {
        pcL.setLocalDescription(new SessionDescription(offer), function() {
            
            pc.setRemoteDescription(new SessionDescription(offer), function() {
                pc.createAnswer(function(answer) {
                    pc.setLocalDescription(new SessionDescription(answer), function() {
                        pcL.setRemoteDescription(new SessionDescription(answer), function() { }, error);
                    }, error);
                }, error);
            }, error);
        }, error);
    }, error);

}

function onError() {
    alert("Hey - no video ey!");
}
*/
