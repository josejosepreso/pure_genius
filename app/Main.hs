module Main where

import Text.Regex.Posix
import System.Environment
import Data.List
import Network.Curl
import Text.HTML.TagSoup

getLyrics url = parseTags <$> snd <$> curlGetString url curlOpts >>= putStr . unlines . map fromTagText . filter isTagText . takeWhile notSideBar . dropWhile notLyricsDiv
  where
    notLyricsDiv t = or [ not $ isTagOpen t , fromAttrib "data-lyrics-container" t /= "true" ]
    notSideBar t = or [ not $ isTagOpen t , not $ fromAttrib "class" t =~ "RightSidebar*" :: Bool ]

search query = getResults >>= geniusLink . firstLink >>= getLyrics
  where
    getResults = parseTags <$> snd <$> curlGetString (duck ++ query) curlOpts
    firstLink = head . map (drop 2 . fromAttrib "href") . filter (\t -> t ~== TagOpen "a" [])
    geniusLink s = tail <$> dropWhile (/= '=') <$> fromAttrib "content" <$> head <$> filter (\t -> t ~== TagOpen "META" []) <$> parseTags <$> snd <$> curlGetString s curlOpts

main :: IO ()
main = getArgs >>= search . intercalate "%20"

duck :: String
duck = "https://lite.duckduckgo.com/lite?q=genius%20lyrics%20"

httpHeaders :: [String]
httpHeaders =
  [ "User-Agent: Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36"
  ]

curlOpts :: [CurlOption]
curlOpts =
  [ CurlFollowLocation True
  , CurlHttpHeaders httpHeaders
  ]
