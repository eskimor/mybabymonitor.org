module Server (connect, outgoing, Outgoing(..), Incoming(..)) where
    
import WebSocket
import Json.Decode as JD
import Json.Decode ((:=))
import Json.Encode as JE     
import Signal
import Signal ((<~), Signal)


type Outgoing = IamHere
              | BabyStarted String
              | ConnectBaby String

type Incoming = BabyCount Int
               | AvailableBabies (List String)


-- Format:
--   { "BabyCount" : 5} , ...


outgoing : Signal.Channel Outgoing
outgoing = Signal.channel IamHere


encodedOutgoing : Signal String
encodedOutgoing = JE.encode 0 <~ (encoder <~ Signal.subscribe outgoing)
           
connect : Signal (Result String Incoming)
connect = JD.decodeString decoder <~ WebSocket.connect "ws://mybabymonitor.org" encodedOutgoing



decoder : JD.Decoder Incoming
decoder = JD.oneOf [
           JD.object1 BabyCount ( "BabyCount" := JD.int )
          , JD.object1 AvailableBabies ( "AvailableBabies" := JD.list JD.string )
          ]


encoder : Outgoing -> JE.Value
encoder outgoing = case outgoing of
                     IamHere -> JE.object [("IamHere", JE.string "")]
                     BabyStarted name -> JE.object [("BabyStarted", JE.string name)]
                     ConnectBaby name -> JE.object [("ConnectBaby", JE.string name)]