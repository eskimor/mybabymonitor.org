{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Main where


import Yesod
import Text.Julius
import Text.Hamlet
import qualified Data.Text as T
import qualified Data.Conduit.Text as CT
import qualified Data.Conduit as C
import Control.Concurrent.MVar
import Data.Conduit (($$), (=$))
import Control.Applicative ((<$>),(<*>))

data BabyPhone = BabyPhone {
      babyIceCandidate
    , parentIceCandidate
    , babyDescription
    , parentDescription :: MVar T.Text
    }



mkYesod "BabyPhone" [parseRoutes|
                      / HomeR GET
                      /baby BabyR GET
                      /parent ParentR GET
                      /babyIceCandidate BabyIceCandidateR POST GET
                      /babyOfferDescription BabyOfferDescriptionR POST GET
                      /parentIceCandidate ParentIceCandidateR POST GET
                      /parentOfferDescription ParentOfferDescriptionR POST GET
|]

instance Yesod BabyPhone

instance RenderMessage BabyPhone FormMessage where
    renderMessage _ _ = defaultFormMessage

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Hello World!|]


getBabyR :: Handler Html
getBabyR = defaultLayout $ do
             $(whamletFile "baby.html")
             toWidget $(juliusFile "baby.js")
             addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.js"

getParentR :: Handler Html
getParentR = defaultLayout $ do
             $(whamletFile "parent.html")
             toWidget $(juliusFile "parent.js")
             addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.11.1/jquery.js"



postBabyIceCandidateR :: Handler ()
postBabyIceCandidateR = putBodyInMVar babyIceCandidate

postBabyOfferDescriptionR :: Handler ()
postBabyOfferDescriptionR = putBodyInMVar babyDescription

getBabyIceCandidateR :: Handler T.Text
getBabyIceCandidateR = putMVarInBody babyIceCandidate

getBabyOfferDescriptionR :: Handler T.Text
getBabyOfferDescriptionR = putMVarInBody babyDescription

postParentIceCandidateR :: Handler ()
postParentIceCandidateR = putBodyInMVar parentIceCandidate

postParentOfferDescriptionR :: Handler ()
postParentOfferDescriptionR = putBodyInMVar parentDescription

getParentIceCandidateR :: Handler T.Text
getParentIceCandidateR = putMVarInBody parentIceCandidate

getParentOfferDescriptionR :: Handler T.Text
getParentOfferDescriptionR = putMVarInBody parentDescription
  

main :: IO ()
main = b >>= warp 3000 
    where
      b = BabyPhone
          <$> newEmptyMVar
          <*> newEmptyMVar
          <*> newEmptyMVar
          <*> newEmptyMVar


getStuffSink ::  Monad m => C.Sink i m i
getStuffSink = do
               val <- C.await
               case val of
                Just x -> return x
                Nothing -> getStuffSink


putBodyInMVar :: (BabyPhone -> MVar T.Text) -> Handler ()
putBodyInMVar mvar = do
  body <- runInputPost $ ireq textField "data"
  y <- getYesod
  liftIO $ putMVar (mvar y) body
  
putMVarInBody :: (BabyPhone -> MVar T.Text) -> Handler T.Text
putMVarInBody mvar = do
  y <- getYesod
  liftIO $ takeMVar (mvar y) 
