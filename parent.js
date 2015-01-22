
$(document).ready(connectSocket);

function connectSocket() {
    logEvent('Connecting ...');
    var connection = new WebSocket(webSockUrl('@{BabyConnectChannelR "baby"}'), [])
    var servers = null;
    var peerConnection = new RTCPeerConnection(servers);
    peerConnection.onicecandidate = gotLocalIceCandidate(connection);
    peerConnection.onaddstream = gotStream(connection);
    connection.onopen = function (e) {connection.send(JSON.stringify(
        {
            'startStreaming' : true
        }
    ))}
    connection.onerror = function (e) {
        logErrorRetry('Websocket error: ' + e);
        retryConnect();
    }
    connection.onmessage = function (e) {
        var message = JSON.parse (e.data);
        logEvent("Got message: " + e.data);
        if(message.error) {
            logErrorRetry('Server error: ' + message.error);
            retryConnect();
            return;
        }
        if(message.description) {
            logEvent("Got baby description (" + message.description + "): " + status);
            peerConnection.setRemoteDescription(new RTCSessionDescription(message.description));
            peerConnection.createAnswer(
                gotDescription(connection, peerConnection)
                , function(e) {
                    logErrorRetry('Error happend when creating answer: ' + e);
                    retryConnect();
                }
            )
        }
        if(message.ice) {
            logEvent("Got baby ice candidate (" + message.ice + "): " + status);
            peerConnection.addIceCandidate(new RTCIceCandidate(message.ice));
        }
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
        $("#remoteVideo").attr('src', URL.createObjectURL(ctx.destination));
        connection.send(JSON.stringify(
            {
                'gotStream' : 'true'
            }
        ))
        connection.close();
    }
}

function gotDescription(connection, peerConnection) {
    return function(description) {
        logEvent("Got local description: " + JSON.stringify(description));
        peerConnection.setLocalDescription(description);
        connection.send(JSON.stringify(
            {
                'description' : description
            }
        ))
        logEvent('Sent description ...');
    }
}

function retryConnect() {
    setTimeout(connectSocket, 10000);
}

function logErrorRetry(e) {
    logError(e + ", retrying in 10 seconds ...");
    retryConnect();
}
