module Model.RedditScraper ( RedditItem(..), pull ) where

import Text.XML.HXT.Core
import Text.XML.HXT.Curl

data RedditItem = RI { title ::  String
                     , link  ::  String
                     , exts  :: [String]
                     }
                     deriving (Show, Eq, Ord)

pull     :: String -> IO [RedditItem]
pull user = runX process >>= return . map build where

    url        = "http://www.reddit.com/user/" ++ user ++ "/.rss?limit=100"

    process    = readDocument  [withValidate no, withCurl []] url
             >>> deep (isElem >>> hasName "item")
             >>> listA itemInfo

    itemInfo   = catA [elemText "title", elemText "link", external]

    elemText e = deep $ isElem >>> hasName e >>> deep getText

    external   = elemText "description"
             >>> hread
             >>> deep (isElem >>> hasName "a")
             >>> filterA (deep getText >>> arr (== "link"))
             >>> getAttrValue "href"

    build (t:l:ex) = RI t l ex
    build l        = error ("Invalid Reddit Item: " ++ show l)
