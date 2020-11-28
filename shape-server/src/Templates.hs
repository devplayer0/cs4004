{-# LANGUAGE OverloadedStrings #-}
module Templates where

import Control.Monad (forM_)

import Data.ByteString.UTF8
import Data.Text
import Data.ByteString.Base64.URL
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes

home :: [String] -> String -> H.Html
home examples current = H.html $ do
  H.head $ do
    H.title "asd"

  H.body $ do
    H.h1 "Shape solver"
    H.p "Enter an expression in the box below and click Render"

    H.p "Examples:"
    H.ul $ forM_ examples (H.li . H.pre . H.toHtml)

    H.form H.! action "" H.! method "POST" $ do
      H.textarea H.! name "expression" H.! style "width: 800px; height: 300px;" $ (H.toHtml current)
      H.input H.! type_ "submit" H.! value "Render"

    case current of
      ""   -> H.stringComment "No expression currently :("
      expr -> H.img H.! src (H.toValue $ "/render/" ++ (unpack $ encodeBase64 (fromString expr)) ++ "/png")
