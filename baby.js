$(document).ready(
    function(){
        logEvent("Document loaded.");
        navigator.getUserMedia(
            {audio:true, video:true}
            , gotStream
            , logErrorF("getUserMedia error: ")
        );
    }
)


function openSocket(stream) {
    var connection = null;
    try {
        connection = new WebSocket(webSockUrl('@{BabyOpenChannelR "baby"}'), []);
    }
    catch (e) {
        logErrorRetry("Error when creating WebSocket: " + e, stream);
        return;
    }
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
            peerConnection.setRemoteDescription(
                new RTCSessionDescription(message.description)
                , logEventF("Successfully set remote description.")
                , logErrorF("Error while setting remote description: ")
            );
        }
        if(message.ice) {
            logEvent("Got parent ice candidate (" + message.ice + "): " + status);
            peerConnection.addIceCandidate(
                new RTCIceCandidate(message.ice)
                , logEventF("Successfully added ice candidate.")
                , logErrorF("Error while setting ice candidate: ")
            );
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
            peerConnection.setLocalDescription(
                description
                , gotLocalDescription(connection, description)
                , logErrorF("Error while setting local description: ")
            );
        }
        , logErrorF("Error creating offer: ")
    );
    return peerConnection;
}

function gotLocalDescription(connection, description) {
    return function () {
        connection.send(JSON.stringify(
            {
                'description' : description
            }
        ))
        logEvent('Sent description.');
    }
}

function gotStream(stream){
    logEvent("Received local stream.");
    openSocket(stream);
}

function retrySocket(stream) {
    setTimeout(function () {openSocket(stream);}, 10000);
}

function logErrorRetry(e, stream) {
    logError(e + ", retrying in 10 seconds ...");
    retrySocket(stream);
}
