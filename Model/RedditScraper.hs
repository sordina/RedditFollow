{-# LANGUAGE NoMonomorphismRestriction #-}

module Model.RedditScraper ( pull ) where

import Text.XML.HXT.Core
import Text.XML.HXT.Curl

pull     :: String -> IO [[String]]
pull user = runX $ process ("http://www.reddit.com/user/" ++ user ++ "/.rss")

  where

    process url = readDocument  [withValidate no, withCurl []] url
              >>> deep (isElem >>> hasName "item")
              >>> listA itemInfo

    itemInfo   = catA [elemText "title", elemText "link", external]

    elemText e = deep $ isElem >>> hasName e >>> deep getText

    external   = elemText "description"
             >>> hread
             >>> deep (isElem >>> hasName "a")
             >>> getAttrValue "href"
