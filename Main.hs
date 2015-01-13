{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Main where


import Yesod
import Text.Julius
import Text.Hamlet

data HelloWorld = HelloWorld

mkYesod "HelloWorld" [parseRoutes|
                                   / HomeR GET
                                   /baby BabyR GET
|]

instance Yesod HelloWorld

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Hello World!|]


getBabyR :: Handler Html
getBabyR = defaultLayout $ do
             $(whamletFile "baby.html")
             toWidget $(juliusFile "baby.js")



main :: IO ()
main = warp 3000 HelloWorld
