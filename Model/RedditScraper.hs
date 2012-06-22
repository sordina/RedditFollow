{-# LANGUAGE NoMonomorphismRestriction #-}

import Network.HTTP.Conduit
import Text.XML.HXT.Core
import Data.ByteString.Lazy.Char8 (unpack)
import System.IO
import Control.Monad (unless)
import Data.Text hiding (unpack)

-- import Data.Text.Lazy.Encoding (decodeASCII)

type Link        = Text
type RedditLink  = Text
type Description = Text

data Item = Item Link RedditLink (Maybe Description) deriving (Show)

main = simpleHttp url >>= runX . process . unpack >>= mapM_ print >> hFlush stdout

process str = constA str >>> hread >>> deep memberNames

memberNames = isElem >>> hasName "item" >>> (title &&& link &&& desc)
-- deep getText --  >>> hasAttrValue "class" (== "memName") >>> deep getText

title = undefined
link  = undefined
desc  = undefined

url = "http://www.reddit.com/user/sordina/.rss"
