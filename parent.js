
$(document).ready(connectSocket);

function connectSocket() {
    logEvent('Connecting ...');
    var connection = new WebSocket(webSockUrl('@{BabyConnectChannelR "baby"}'), [])
    connection.onopen = function (e) {
        try {
            startStreaming(connection);
        }
        catch(e) {
            logErrorRetry("Sending startStreaming failed (readyState: " + connection.readyState + "): " + e);
        }
    }
    connection.onclose = function (e) {
        logEvent("Connection closed: " + e);
    }
    var servers = null;
    var peerConnection = new RTCPeerConnection(servers);
    peerConnection.onicecandidate = gotLocalIceCandidate(connection);
    peerConnection.onaddstream = gotStream(connection);
    connection.onerror = function (e) {
        logErrorRetry('Websocket error: ' + e);
    }
    connection.onmessage = function (e) {
        logEvent("Got message: " + e.data);
        var message = JSON.parse (e.data);
        if(message.error) {
            connection.close();
            logErrorRetry('Server error: ' + message.error);
            return;
        }
        if(message.description) {
            logEvent("Got baby description (" + message.description + "): " + status);
            peerConnection.setRemoteDescription(
                new RTCSessionDescription(message.description)
                , createAnswer(peerConnection, connection)
                , logErrorF("Error while setting remote description: ")
            );
        }
        if(message.ice) {
            logEvent("Got baby ice candidate (" + message.ice + "): " + status);
            peerConnection.addIceCandidate(
                new RTCIceCandidate(message.ice)
                , logEventF("Successfully added ice candidate!")
                , logErrorF("Adding ice candidate failed: ")
            );
        }
    /*    else if(message.ice == null) {
            logEvent("Got null ice candidate, closing connection!");
            connection.send(JSON.stringify(
                {
                    'gotStream' : 'true'
                }
            ))
            connection.close();
        }
        */

    }
}

function gotStream(connection){
    return function(event) {
        logEvent("Received remote stream! JUHU! (" + event.stream.id + ")");
        //  Set up volume:
        var ctx = new window.AudioContext();
        var source = ctx.createMediaStreamSource(event.stream);
        var gainNode = ctx.createGain();
        gainNode.gain.value = 10;
        source.connect(gainNode);
        gainNode.connect(ctx.destination);
        
        $("#remoteVideo").attr('src', URL.createObjectURL(event.stream));
        $("#remoteAudio").attr('src', URL.createObjectURL(ctx.destination));
    }
}

function gotDescription(connection, peerConnection) {
    return function(description) {
        logEvent("Got local description: " + JSON.stringify(description));
        function sendDescription() {
            connection.send(JSON.stringify(
                {
                    'description' : description
                }
            ))
            logEvent('Sent description ...');
        }
        peerConnection.setLocalDescription(
            description
            , sendDescription
            , logErrorF("Error while setting local description: ")
        );
    }
}

function retryConnect() {
    setTimeout(connectSocket, 10000);
}

function logErrorRetry(e) {
    logError(e + ", retrying in 10 seconds ...");
    retryConnect();
}

function startStreaming(connection) {
    logEvent("Sending startStreaming ...");
    connection.send(JSON.stringify(
        {
            'startStreaming' : true
        }
    ))
}


function createAnswer(peerConnection, connection) {
    return function () {
        peerConnection.createAnswer(
            gotDescription(connection, peerConnection)
            , logErrorF('Error happend when creating answer: ')
        );
    }
}
