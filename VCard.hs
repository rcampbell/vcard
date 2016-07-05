import Text.ParserCombinators.Parsec
import Text.Parsec.Char (crlf)
import Text.Parsec.String (parseFromFile)
import Text.Parsec.Perm (permute, (<$$>), (<$?>), (<||>), (<|?>))
import Control.Monad (liftM)
import Data.Char (toLower, toUpper, isSpace)
import Data.List (intersperse)
import Test.HUnit

{- TODO

 - The permutation parser for contentlines ignores all isolated instances

 - Should I trim? During parsing or afterwards?

 - How should I perform version validation? 

 -}


{- | Write class -}

class Write a where
  write :: a -> String

-- | Used by Write class instances for line ending
nl :: String
nl = "\r\n"

-- | Used by Write class instances for list write support
lwrite :: Write a => [a] -> String
lwrite [] = ""
lwrite (x:xs) = write x ++ lwrite xs

-- | Used by Write class instances for Maybe write support
mwrite :: Write a => Maybe a -> String
mwrite Nothing = ""
mwrite (Just x) = write x


{- | Generic parsers -}

-- | Match the lowercase or uppercase form of 'c'
ichar :: Char -> Parser Char
ichar c = char (toLower c) <|> char (toUpper c)

-- | Match the string 's', accepting either lowercase or uppercase form of each character 
istring :: String -> Parser String
istring s = try (mapM ichar s) <?> "\"" ++ s ++ "\""

-- TODO, clean me!
components :: Parser [[String]]
components = values `sepBy` semicolon
  where values = text `sepBy` comma
        text = many (noneOf ";,\r\n")
        semicolon = char ';'
        comma = char ','

-- TODO clean me!
filter_empty_components = filter (\ss -> concat ss /= "")

-- TODO clean me!
pref :: Parser String
pref = do string "PREF"
          char '='
          char '1'
          return "PREF" -- TODO ugly..


{- | 3. vCard Format Specification -}

-- | 3.3. ABNF Format Definition

data VCard = VCard { vc_version :: Maybe VERSION
                   , vc_properties :: Properties
                   } deriving (Show)

instance Write VCard where
  write c = "BEGIN:VCARD\r\n" ++
            mwrite (vc_version c) ++
            write (vc_properties c) ++
            "END:VCARD\r\n"

vcard :: Parser VCard
vcard = do
  begin
  v <- try (lookAhead (optionMaybe version))
  p <- properties
  end
  return (VCard { vc_version = v, vc_properties = p }) -- TODO

data Properties = Properties { p_version :: Maybe VERSION
                             , p_fn :: [FN]
                             , p_n :: Maybe N
                             , p_tel :: TEL
                             , p_prodid :: Maybe PRODID
                             , p_x :: [X]
                             } deriving (Show)

instance Write Properties where
  write p = lwrite (p_fn p) ++
            mwrite (p_n p) ++
--            write (p_tel p) ++
            mwrite (p_prodid p) ++
            lwrite (p_x p)


-- TODO
-- The reason I use many1 for tel and x instead of many for
-- these * cardinality properties is because it don't work
-- properly otherwise. I guessed that it would help "fail"
-- to fst [] when there were 0, but it is actually needed
-- even when there is one instance.
properties = permute (Properties
                       <$?> (Nothing, Just `liftM` version)
                       <||> many1 fn
                       <|?> (Nothing, Just `liftM` n)
                       <|?> ([], many1 tel)
                       <|?> (Nothing, Just `liftM` prodid)
                       <|?> ([], many1 x))                        

group :: Parser String
group = do
  g <- (many1 (alphaNum <|> char '-'))
  char '.'
  return g






{- | 6. vCard Properties -}


-- | 6.1.1. BEGIN

begin :: Parser ()
begin = do
  istring "BEGIN"
  char ':'
  istring "VCARD"
  crlf
  return ()
  

-- 6.1.2. END

end :: Parser ()
end = do
  istring "END"
  char ':'
  istring "VCARD"
  crlf
  return ()


-- 6.2.1. FN 1*

data FN = FN String
  deriving (Show)

instance Write FN where
  write (FN s) = "FN:" ++ s ++ nl

fn ::Parser FN
fn = do
  istring "FN"
  char ':'
  v <- manyTill anyChar crlf
  return (FN v)
  

-- 6.2.2. N *1

data N = N [[String]]
  deriving (Show)

instance Write N where
  write (N sss) = "N:" ++ flatten ++ nl
    where flatten = join ";" (map (join ",") sss)
          join c = concat . intersperse c

n :: Parser N
n = do
  ichar 'N'
  char ':'
  v <- components
  crlf
  return (N (filter_empty_components v))


-- 6.3.1. ADR

adr = do
  istring "ADR"
  char ';'
--  p <- params
  char ':'
  v <- components
  crlf
  return (filter_empty_components v)


-- 6.4.1. TEL *

type TEL = [([String], String)]

{-
instance Write TEL where
  write [] = ""
  write ((p, t):ts) = "TEL;" ++ p 
-}
tel :: Parser ([String], String)
tel = do
  istring "TEL"
  char ';'
  p <- params
  char ':'
  v <- manyTill anyChar crlf
  return (p, v)
  where params = (try pvalue <|> try pref <|> try ptype) `sepBy` (char ';')
        pvalue = do string "VALUE"
                    char '='
                    string "text" <|> string "uri"
        ptype = do istring "TYPE"
                   char '='
                   try vlist <|> plist
        vlist = do v <- between
                     (char '"')
                     (char '"')
                     ((many (noneOf "\"")) `sepBy` (char ',')) :: Parser [String]
                   return (concat (intersperse "," v))
        plist = try (istring "text")
                <|> try (istring "voice")
                <|> try (istring "fax")
                <|> try (istring "cell")
                <|> try (istring "video")
                <|> try (istring "pager")
                <|> try (istring "textphone")
                <|> try (many (noneOf ";:\r\n"))


-- 6.4.2. EMAIL



-- 6.6.4. ORG

type ORG = [String]

org :: Parser ORG
org = do
  istring "ORG"
  char ':'
  v <- components
  crlf
  return (filter (not . null) v)
  where components = text `sepBy` semicolon
        text = many (noneOf "\r\n")
        semicolon = char ';'


-- 6.7.3. PRODID *1

data PRODID = PRODID String
  deriving (Show)

instance Write PRODID where
  write (PRODID s) = "PRODID:" ++ s ++ nl
  
prodid :: Parser PRODID
prodid = do
  istring "PRODID"
  char ':'
  v <- manyTill anyChar crlf
  return (PRODID v)
  

-- 6.7.9. VERSION 1

data VERSION = VERSION String
  deriving (Show)

instance Write VERSION where
  write (VERSION s) = "VERSION:" ++ s ++ nl

version :: Parser VERSION
version = do
  string "VERSION:"
  v <- (string "4.0" <|> string "3.0" <|> string "2.1")
  crlf
  return (VERSION v)
  

-- 6.10. Extended Properties and Parameters *

data X = X (String, String)
  deriving (Show)

instance Write X where
  write (X (n, v)) = n ++ ":" ++ v ++ nl

x :: Parser X
x = do
  try (lookAhead (istring "X-"))
  n <- many1 (noneOf ":")
  char ':'
  v <- many (noneOf "\r\n")
  crlf
  return (X (n, v))






-- 6.2.7. GENDER
{-
gender_param = "GENDER"
  
data Gender = Gender { sex :: Maybe Sex
                     , identity :: Maybe Identity
                     }
data Sex = M | F | O | N | U deriving (Show, Read)
type Identity = String

instance Show Gender where
  show (Gender {sex = s, identity = i}) = gender_param ++ ":" ++ s' ++ i'
    where s' = case s of
                 Just s -> show s
                 Nothing -> ""
          i' = case i of
                 Just i -> ";" ++ i
                 Nothing -> ""

gender :: Parser Gender
gender = do string (gender_param ++ ":")
            s <- optionMaybe sex
            i <- optionMaybe identity
            return Gender {sex = fmap (read . (:[])) s, identity = i}
  where sex = oneOf "MFONU"
        identity = do char ';'
                      many (noneOf "\r\n")                                            

tests_gender = test [ "Gender" ~: "show" ~: "GENDER:M" ~=? (show Gender {sex = Just M, identity = Nothing})
                    , "Gender" ~: "show" ~: "GENDER:F" ~=? (show Gender {sex = Just F, identity = Nothing})
                    , "Gender" ~: "show" ~: "GENDER:M;Fellow" ~=? (show Gender {sex = Just M, identity = Just "Fellow"})
                    , "Gender" ~: "show" ~: "GENDER:F;grrrl" ~=? (show Gender {sex = Just F, identity = Just "grrrl"})
                    , "Gender" ~: "show" ~: "GENDER:O;intersex" ~=? (show Gender {sex = Just O, identity = Just "intersex"})
                    , "Gender" ~: "show" ~: "GENDER:;it's complicated" ~=? (show Gender {sex = Nothing, identity = Just "it's complicated"})
                    ]
-}


-- UTILITY STUFF!!!!!!!
p1 = do
  result <- parseFromFile vcard "sample1"
  case result of
    Left err -> print err
    Right xs -> do print xs
                   putStr "\n"
                   putStrLn (write xs)
