module VCard where

import System.IO (withFile, IOMode(WriteMode), hSetEncoding, utf8, hPutStr)
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

-- TODO clean me!
pref :: Parser String
pref = do string "PREF"
          char '='
          char '1'
          return "PREF" -- TODO ugly..


{- | 3. vCard Format Specification -}

-- | 3.3. ABNF Format Definition 1*

data VCard = VCard
  { vc_version    :: Maybe VERSION
  , vc_properties :: Properties
  } deriving (Show)

instance Write VCard where
  write c = "BEGIN:VCARD" ++ nl ++
            mwrite (vc_version c) ++
            write (vc_properties c) ++
            "END:VCARD" ++ nl

vcardEntity = do
  many1 vcard

vcard :: Parser VCard
vcard = do
  begin
  v <- try (lookAhead (optionMaybe version))
  p <- properties
  end
  return (VCard { vc_version = v, vc_properties = p }) -- TODO


-- | contentline
{- TODO is this even needed?
data ContentLine = ContentLine { group :: Maybe Group
                               , property :: Property
                               , 
contentline = ""
-}

-- | group

data Group = Group String

instance Write Group where
  write (Group s) = s ++ "."

group :: Parser Group
group = do
  g <- (many1 (alphaNum <|> char '-'))
  char '.'
  return $ Group g


{- | 3.4. Property Value Escaping -}

-- | Property value parser for multiple values delimited by commas
pvalues :: Parser [String]
pvalues = do
  vs <- many value `sepBy` char ','
  return $ map concat vs
  where value = nonEscape <|> escape
        nonEscape = fmap return (noneOf ("\\,\r\n"))
        escape = do
          b <- char '\\'
          c <- oneOf "\\,;nN"
          return [b, c]

-- | Property value parser for multiple fields delimited by semicolons
pfields :: Parser [[String]]
pfields = do
  fs <- field `sepBy` char ';'
  return $ map (map concat) fs
  where field = many component `sepBy` char ','
        component = nonEscape <|> escape
        nonEscape = fmap return (noneOf "\\,;\r\n")
        escape = do
          b <- char '\\'
          c <- oneOf "\\,;nN"
          return [b, c]


{- | 5. Property Parameters -}

data Parameters = Parameters { param_value :: [VALUE]
                             , param_type  :: [TYPE]
                             } deriving (Show)

instance Write Parameters where
  write p = lwrite (param_value p) ++
            lwrite (param_type p)

params = permute (Parameters
                  <$?> ([], many1 valueParam)
                  <|?> ([], many1 typeParam))

-- TODO somehow fail on unquoted : or ; unless it's the next param or value
paramValues :: Parser [String]
paramValues = do
  vs <- many value `sepBy` char ','
  return $ map concat vs
  where value = nonEscape <|> escape
        nonEscape = fmap return (noneOf (",:;\""))
        escape = do
          s <- char '"'
          v <- many (noneOf "\"")
          e <- char '"'
          return ([s] ++ v ++ [e])
          
          
-- | 5.2. VALUE
-- TODO support all those predefined values

data VALUE = VALUE String
  deriving (Show)

instance Write VALUE where
  write (VALUE s) = ";VALUE=" ++ s

valueParam :: Parser VALUE
valueParam = do
  istring ";VALUE="
  v <- many (noneOf ",:;")
  return $ VALUE v


-- | 5.6 TYPE

data TYPE = TYPE [String]
  deriving (Show)

instance Write TYPE where
  write (TYPE ss) = ";TYPE=" ++ vs
    where vs = concat $ intersperse "," ss

typeParam :: Parser TYPE
typeParam = do
  istring ";TYPE="
  vs <- paramValues
  return $ TYPE vs


{- | 6. vCard Properties -}

data Properties = Properties
  { prop_version :: Maybe VERSION
  , prop_fn      :: [FN]
  , prop_n       :: Maybe N
  , prop_tel     :: [TEL]
  , prop_prodid  :: Maybe PRODID
  , prop_x       :: [X]
  } deriving (Show)

instance Write Properties where
  write p = lwrite (prop_fn p) ++
            mwrite (prop_n p) ++
            lwrite (prop_tel p) ++
            mwrite (prop_prodid p) ++
            lwrite (prop_x p)

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


-- | 6.1.1. BEGIN

begin :: Parser ()
begin = do
  istring "BEGIN:VCARD"
  crlf
  return ()
  

-- 6.1.2. END

end :: Parser ()
end = do
  istring "END:VCARD"
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
  fs <- pfields 
  crlf
  return (N fs)


-- 6.3.1. ADR

adr = do
  istring "ADR"
  char ';'
--  p <- params
  char ':'
  fs <- pfields
  crlf
  return fs


-- 6.4.1. TEL *

data TEL = TEL { tel_params :: Parameters
               , tel_value  :: String
               } deriving (Show)

instance Write TEL where
  write t = "TEL" ++
            write (tel_params t) ++
            ":" ++
            tel_value t ++
            nl

tel :: Parser TEL
tel = do
  istring "TEL"
  ps <- params
  char ':'
  v <- manyTill anyChar crlf
  return $ TEL { tel_params = ps, tel_value = v }


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



{- | I/O -}

-- | Reading


-- | Writing, must be UTF-8 and should have a .vcf or .vcard extension
writeVCard :: FilePath -> VCard -> IO ()
writeVCard f c = do
  withFile f WriteMode $ \h -> do
    hSetEncoding h utf8
    hPutStr h (write c)



-- UTILITY STUFF!!!!!!!
p1 = do
  result <- parseFromFile vcardEntity "../hs/sample1"
  case result of
    Left err -> print err
    Right xs -> do print xs
                   putStr "\n"
                   putStrLn (lwrite xs)
