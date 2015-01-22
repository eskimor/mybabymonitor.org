$(document).ready(
    function(){
        logEvent("Document loaded.");
        navigator.getUserMedia({audio:true, video:true}, gotStream,
                               function(error) {
                                   logError("getUserMedia error: " + error);
                               }
                              );
        function gotStream(stream){
            logEvent("Received local stream.");
            openSocket(stream);
        }
    }
)


function openSocket(stream) {
    var connection = new WebSocket(webSockUrl('@{BabyOpenChannelR "baby"}'), []);
    var peerConnection = null;
    connection.onerror = function (e) {
        logErrorRetry('Websocket error: ' + e , stream);
    }
    connection.onmessage = function (e) {
        var message = JSON.parse(e.data);
        if(message.error) {
            logErrorRetry('Server error: ' + message.error, stream);
            return;
        }
        if(message.startStreaming) {
            logEvent("Parent connected");
            peerConnection = startStreaming(stream, connection);
            // Ok we got one client, let's wait for another one:
            openSocket(stream);
        }
        if(message.description) {
            logEvent("Got parent description (" + message.description + "): " + status);
            peerConnection.setRemoteDescription(new RTCSessionDescription(message.description));
        }
        if(message.ice) {
            logEvent("Got parent ice candidate (" + message.ice + "): " + status);
            peerConnection.addIceCandidate(new RTCIceCandidate(message.ice));
        }
        if(message.gotStream) {
            logEvent("Parent is connected, closing socket ...");
            connection.close();
        }
    }
}


function startStreaming(stream, connection) { // Alright then.
    var servers = null;
    var peerConnection = new RTCPeerConnection(servers);
    peerConnection.onicecandidate = gotLocalIceCandidate(connection);
    logEvent("Adding stream ...");
    peerConnection.addStream(stream);
    logEvent("Creating offer ...");
    peerConnection.createOffer(
        function(description) {
            logEvent("Got description: " + JSON.stringify(description));
            peerConnection.setLocalDescription(description);
            connection.send(JSON.stringify(
                {
                    'description' : description
                }
            ))
            logEvent('Sent description.');
            
        }
        ,
        function(error) {
            logErrorRetry("Error creating offer: " + error, stream);
            throw("Something went wrong, see log!")
        }
    )
    return peerConnection;
}


function retrySocket(stream) {
    setTimeout(function () {openSocket(stream);}, 10000);
}

function logErrorRetry(e, stream) {
    logError(e + ", retrying in 10 seconds ...");
    retrySocket(stream);
}
