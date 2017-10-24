module URL.Parse where

import Prelude
import URL.Core
import Data.Attoparsec.Text as Atto
import qualified Data.Text as TS
import Data.Char
import Data.List (find)
import Data.Maybe (fromJust)

import Control.Applicative


parse :: TS.Text -> Either String URL
parse text = parseOnly (uRLP <* endOfInput) text

-- * Parsers (in bottom-up order)

-- ** URL

protoP :: Parser Proto
protoP = Proto . TS.pack <$> (many1 chars <* string "://")
  where
    chars = satisfy isAlphaNum <|> char '+' <|> char '-' <|> char '.'
    -- ^ todo: Use inClass for better speed?

authenticationP :: Parser (Maybe (T, T))
authenticationP = some <|> no
  where
    some = do
      user <- Atto.takeWhile (/= ':') <* char ':'
      password <- Atto.takeWhile (/= '@') <* char '@'
      return $ Just (user, password)
    no = pure Nothing

hostP :: Parser Host
hostP = try ip4P <|> domainP

ip4P :: Parser Host
ip4P = IP4 <$> octet' <*> octet' <*> octet' <*> octet
  where
    octet = decimal
    octet' = octet <* char '.'

domainP :: Parser Host
domainP = Domain <$> Atto.takeWhile (/= ':')

portP :: Parser Port
portP = Port <$> (char ':' *> decimal)

authorityP :: Port -> Parser Authority
authorityP defaultPort = do
  authText <- Atto.takeWhile (\c -> not (c == '/' || c == '?' || c == '#'))
  either fail return $ parseOnly authorityP' authText
  where
    authorityP' :: Parser Authority
    authorityP' = Authority <$> authenticationP <*> hostP <*> (portP <|> pure defaultPort)

pathP :: Parser Path
pathP = some <|> no
  where
    some = char '/' *> relativePathP
    no = return (Path [])

relativePathP :: Parser Path
relativePathP = some <|> no
  where
    part = Atto.takeWhile (\c -> c /= '/' && c /= '?' && c /= '#')
    parts = (:) <$> part <*> many (char '/' *> part)
    some = Path <$> parts
    no = return (Path [])

paramsP :: Parser Params
paramsP = some <|> no
  where
    part = Atto.takeWhile (\c -> c /= '#' && c /= '&')
    split = uncurry paramVal . TS.break (== '=') :: TS.Text -> (TS.Text, Maybe TS.Text)
    paramVal a b = if TS.null b
      then (a, Nothing)
      else (a, Just $ TS.tail b)

    some = do
      char '?'
      parts :: [TS.Text] <- sepBy part (char '&')
      return $ Params (map split parts)
    no = pure (Params [])

fragmentP :: Parser Fragment
fragmentP = some <|> no
  where
    some = char '#' *> (Fragment <$> takeText)
    no = pure (Fragment "")


uRLP :: Parser URL
uRLP = do
  proto <- protoP
  URL proto
    <$> authorityP (defaultPort proto)
    <*> pathP
    <*> paramsP
    <*> fragmentP

-- * WebURL

webURLP :: Parser WebURL
webURLP =
  try full <|>
  try absolute <|>
  try relative <|>
  fragment
  where
    full = Full <$> uRLP
    absolute = AbsolutePath <$> (char '/' *> pathParamsFragment)
    relative = RelativePath <$> pathParamsFragment
    fragment = FragmentOnly <$> fragmentP
    pathParamsFragment = PathParamsFragment <$> relativePathP <*> paramsP <*> fragmentP

-- * Helpers

defaultPort :: Proto -> Port
defaultPort p = fromJust (lookup p defaultPortMap)

defaultPortMap :: [(Proto, Port)]
defaultPortMap =
  [ ("http", 80)
  , ("https", 443)
  ]

test = tw3
  where
    t1 = pTest protoP "h-.t+tp://"
    t2 = pTest authenticationP "a:b@"
    t3 = pTest hostP "1.2.3.4"
    t4 = pTest domainP "google.com"

    t5 = pTest portP ":123"
    t6 = pTest pathP "/a/b"
    c = pTest relativePathP "a/b"
    t7 = pTest paramsP "?a=b&c=d"
    t8 = pTest fragmentP "#eeuhtulrc.g"

    tx0 = pTest (authorityP 80) "google.com"
    tx1 = pTest uRLP "http://google.com"
    tx2 = pTest uRLP "https://google.com"
    tx3 = pTest uRLP "http://google.com/a/b/"
    tx4 = pTest uRLP "http://google.com?a=b&c=d"
    tx5 = pTest uRLP "http://google.com#aesonuhnseh"
    txc = pTest uRLP "http://google.com/a/b#aesonuhnseh"

    ty = pTest uRLP "http://domain.com/p1/p2?param1=value1&param2=value2#bla"
    ty2 = pTest uRLP "http://user:password@domain.com/p1/p2?param1=value1&param2=value2#bla"

    tw0 = pTest webURLP "#http://a:b@gmail.com/a/b?c=d&e=f#ensaoeuh"
    tw1 = pTest webURLP "/a/b?eeu&xxx=yyy#ee"
    tw2 = pTest webURLP "a/b?eeu&xxx=yyy#ee"
    tw3 = pTest webURLP "#"

    pTest p t = parseOnly (p <* endOfInput) t
