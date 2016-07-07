module VCard where

import System.IO (withFile, IOMode(WriteMode), hSetEncoding, utf8, hPutStr)
import Text.ParserCombinators.Parsec
import Text.Parsec.Char (crlf)
import Text.Parsec.String (parseFromFile)
import Text.Parsec.Perm (permute, (<$$>), (<$?>), (<||>), (<|?>))
import Control.Monad (liftM)
-- import Network.URI (URI, parseURI) TODO do I want to even validate?
import Data.Char (toLower, toUpper, isSpace)
import Data.List (intercalate)
import Data.Time.Format (parseTimeM, formatTime, defaultTimeLocale)
import Data.Time.Clock (UTCTime)
import Test.HUnit

{- TODO

 * The permutation parser for contentlines fails on later isolated instances

 * Groups break my permutation parsing. Here's what's happening: the very first
   parser in each of the property parsers - literally the first line, now the
   group line, but usually the istring "ADR" stuff - needs to fail for the
   switch to happen. In other words, if the first parser succeeds, even if the
   second one in the property parser fails (should be the name following the
   group) then it won't actually switch to other parsers; it just fails out
   saying it expected blah and blah. THE FAIL NEEDS TO HAPPEN IN THE FIRST PARSER.
   I have no idea why. Also, consumption of input after a failure does matter;
   the first parser which can fail needs a try for this to work!

 * The folding (wrapping) logic isn't implemented, breaks

 - Date, time types, https://two-wrongs.com/haskell-time-library-tutorial

 - There is tons of duplication among similar property parsers. I'm hesitant to
   start factoring it out before I make them more fully correct, supporting each
   property's nuances... 

 - Look into moving from String to Text or ByteString. More generally, no
   consideration has yet been given to space and time optimization. 

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

-- | Used by Write class instances for writing property values
vwrite :: [String] -> String
vwrite = intercalate ","

-- | Used by Write class instances for writing property value fields
fwrite :: [[String]] -> String
fwrite sss = intercalate ";" (map (intercalate ",") sss)


{- | Generic parsers -}

-- | Match the lowercase or uppercase form of 'c'
ichar :: Char -> Parser Char
ichar c = char (toLower c) <|> char (toUpper c)

-- | Match the string 's', accepting either lowercase or uppercase form of each character 
istring :: String -> Parser String
istring s = try (mapM ichar s) <?> "\"" ++ s ++ "\""


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
  v <- try (lookAhead (optionMaybe version)) -- TODO I think optionMaybe should be called AFTER the try.. not sure about lookAhead
  p <- properties
  end
  return (VCard { vc_version = v, vc_properties = p }) -- TODO


-- | Groups

data Group = Group String
  deriving (Show)

instance Write Group where
  write (Group s) = s ++ "."

group :: Parser (Maybe Group)
group = do
  g <- optionMaybe (try group)
  return $ Group `liftM` g
    where group = manyTill (alphaNum <|> char '-') (char '.')


{- | 3.4. Property Value Escaping -}

propEscaping :: String -> Parser String
propEscaping cs = do
  nonEscape <|> escape
    where nonEscape = fmap return (noneOf cs)
          escape = do
            b <- char '\\' 
            c <- oneOf "\\,;nN"
            return [b, c]
            

-- | Property value parser for multiple values delimited by commas
propValues :: Parser [String]
propValues = do
  vs <- many value `sepBy` char ','
  return $ map concat vs
  where value = propEscaping "\\,\r\n"
  

-- | Property value parser for multiple fields delimited by semicolons
propFields :: Parser [[String]]
propFields = do
  fs <- field `sepBy` char ';'
  return $ map (map concat) fs
  where field = many component `sepBy` char ','
        component = propEscaping "\\,;\r\n"


{- | 5. Property Parameters -}

data Parameters = Parameters { param_value :: [VALUE]
                             , param_type  :: [TYPE]
                             , param_pref  :: [PREF]
                             } deriving (Show)

instance Write Parameters where
  write p = lwrite (param_value p) ++
            lwrite (param_type p) ++
            lwrite (param_pref p)

params = permute (Parameters
                  <$?> ([], many1 valueParam)
                  <|?> ([], many1 typeParam)
                  <|?> ([], many1 prefParam))

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


-- | 5.3 PREF

data PREF = PREF Integer -- an integer between 1 and 100
  deriving (Show)

instance Write PREF where
  write (PREF i) = ";PREF=" ++ show i

-- TODO improve validation, PREF=0 not allowed...
prefParam :: Parser PREF
prefParam = do
  istring ";PREF="
  v <- try (string "100")
       <|> try (count 2 digit)
       <|> try (count 1 digit)
  return $ PREF (read v)


-- | 5.6 TYPE

data TYPE = TYPE [String]
  deriving (Show)

instance Write TYPE where
  write (TYPE ss) = ";TYPE=" ++ (intercalate "," ss)

typeParam :: Parser TYPE
typeParam = do
  istring ";TYPE="
  vs <- paramValues
  return $ TYPE vs


{- | 6. vCard Properties -}

data Properties = Properties
  { prop_source   :: [SOURCE]
  , prop_kind     :: Maybe KIND
  , prop_fn       :: [FN]
  , prop_n        :: Maybe N
  , prop_nickname :: [NICKNAME]
  , prop_bday     :: Maybe BDAY
  , prop_adr      :: [ADR]
  , prop_tel      :: [TEL]
  , prop_email    :: [EMAIL]
  , prop_impp     :: [IMPP]
  , prop_org      :: [ORG]
  , prop_note     :: [NOTE]
  , prop_prodid   :: Maybe PRODID
  , prop_rev      :: Maybe REV
  , prop_url      :: [URL]
  , prop_version  :: Maybe VERSION
  , prop_x        :: [X]
  } deriving (Show)

instance Write Properties where
  write p = lwrite (prop_source p)   ++
            mwrite (prop_kind p)     ++
            lwrite (prop_fn p)       ++
            mwrite (prop_n p)        ++
            lwrite (prop_nickname p) ++
            mwrite (prop_bday p)     ++       
            lwrite (prop_adr p)      ++
            lwrite (prop_tel p)      ++    
            lwrite (prop_email p)    ++
            lwrite (prop_impp p)     ++
            lwrite (prop_org p)      ++
            lwrite (prop_note p)     ++
            mwrite (prop_prodid p)   ++
            mwrite (prop_rev p)      ++
            lwrite (prop_url p)      ++
            lwrite (prop_x p)

properties = permute (Properties
                       <$?> ([], many1 source)
                       <|?> (Nothing, Just `liftM` kind)
                       <||> many1 fn
                       <|?> (Nothing, Just `liftM` n)
                       <|?> ([], many1 nickname)
                       <|?> (Nothing, Just `liftM` bday)
                       <|?> ([], many1 adr)
                       <|?> ([], many1 tel)
                       <|?> ([], many1 email)
                       <|?> ([], many1 impp)
                       <|?> ([], many1 org)
                       <|?> ([], many1 note)
                       <|?> (Nothing, Just `liftM` prodid)
                       <|?> (Nothing, Just `liftM` rev)
                       <|?> ([], many1 url)
                       <|?> (Nothing, Just `liftM` version)
                       <|?> ([], many1 x))                


-- | 6.1.1. BEGIN 1

begin :: Parser ()
begin = do
  istring "BEGIN:VCARD"
  crlf
  return ()
  

-- 6.1.2. END 1

end :: Parser ()
end = do
  istring "END:VCARD"
  crlf
  return ()


-- | 6.1.3. SOURCE *

data SOURCE = SOURCE { source_param :: Parameters
                     , source_value :: String
                     } deriving (Show)

instance Write SOURCE where
  write r = "SOURCE" ++
            write (source_param r) ++
            ":" ++
            source_value r ++
            nl

source :: Parser SOURCE
source = do
  istring "SOURCE"
  p <- params
  char ':'
  v <- manyTill anyChar crlf
  return $ SOURCE p v


-- | 6.1.4. KIND *1

data KIND = KIND { kind_param :: Parameters
                 , kind_value :: String
                 } deriving (Show)

instance Write KIND where
  write r = "KIND" ++
            write (kind_param r) ++
            ":" ++
            kind_value r ++
            nl

kind :: Parser KIND
kind = do
  istring "KIND"
  p <- params
  char ':'
  v <- manyTill anyChar crlf
  return $ KIND p v


-- | 6.2.1. FN 1*

data FN = FN { fn_group :: Maybe Group
             , fn_param :: Parameters
             , fn_value :: String
             } deriving (Show)

instance Write FN where
  write r = mwrite (fn_group r) ++
            "FN" ++
            write (fn_param r) ++
            ":" ++
            fn_value r ++
            nl

fn ::Parser FN
fn = do
  g <- try (do g <- group
               istring "FN"
               return g)
  p <- params
  char ':'
  v <- manyTill anyChar crlf
  return $ FN g p v
  

-- | 6.2.2. N *1

data N = N [[String]]
  deriving (Show)

instance Write N where
  write (N sss) = "N:" ++ fwrite sss ++ nl

n :: Parser N
n = do
  ichar 'N'
  char ':'
  fs <- propFields 
  crlf
  return $ N fs


-- | 6.2.3. NICKNAME *

data NICKNAME = NICKNAME { nickname_param :: Parameters
                         , nickname_value :: [String]
                         } deriving (Show)

instance Write NICKNAME where
  write r = "NICKNAME" ++
            write (nickname_param r) ++
            ":" ++
            vwrite (nickname_value r) ++
            nl

nickname :: Parser NICKNAME
nickname = do
  istring "NICKNAME"
  p <- params
  char ':'
  v <- propValues
  crlf
  return $ NICKNAME p v


-- | 6.2.4. PHOTO *

-- | 6.2.5. BDAY *1

data BDAY = BDAY { bday_param :: Parameters
                 --, bday_value :: Either Date String
                 , bday_value :: String
                 } deriving (Show)

instance Write BDAY where
  write r = ""

bday :: Parser BDAY
bday = do
  istring "BDAY"
  p <- params
  char ':'
  v <- manyTill anyChar crlf
  return $ BDAY p v
                 

-- | 6.3.1. ADR *

data ADR = ADR { adr_group :: Maybe Group
               , adr_param :: Parameters
               , adr_value :: [[String]]
               } deriving (Show)

instance Write ADR where
  write a = mwrite (adr_group a) ++
            "ADR" ++
            write (adr_param a) ++
            ":" ++
            fwrite (adr_value a) ++
            nl

adr :: Parser ADR
adr = do
  g <- try (do g <- group
               istring "ADR"
               return g)
  p <- params
  char ':'
  v <- propFields
  crlf
  return $ ADR g p v


-- | 6.4.1. TEL *

data TEL = TEL { tel_group :: Maybe Group
               , tel_param :: Parameters
               , tel_value :: String
               } deriving (Show)

instance Write TEL where
  write r = mwrite (tel_group r) ++
            "TEL" ++
            write (tel_param r) ++
            ":" ++
            tel_value r ++
            nl

tel :: Parser TEL
tel = do
  g <- try (do g <- group
               istring "TEL"
               return g)
  p <- params
  char ':'
  v <- manyTill anyChar crlf
  return $ TEL g p v


-- | 6.4.2. EMAIL *

data EMAIL = EMAIL { email_param :: Parameters
                   , email_value :: String
                   } deriving (Show)

instance Write EMAIL where
  write r = "EMAIL" ++
            write (email_param r) ++
            ":" ++
            email_value r ++
            nl

email :: Parser EMAIL
email = do
  istring "EMAIL"
  p <- params
  char ':'
  v <- manyTill anyChar crlf
  return $ EMAIL p v


-- | 6.4.3. IMPP *

data IMPP = IMPP { impp_param :: Parameters
                 , impp_value :: String
                 } deriving (Show)

instance Write IMPP where
  write r = "IMPP" ++
            write (impp_param r) ++
            ":" ++
            impp_value r ++
            nl

-- TODO spit out some URI validation warning, though I don't think I want to toss them..
impp :: Parser IMPP
impp = do
  istring "IMPP"
  p <- params
  char ':'
  v <- manyTill anyChar crlf
  return $ IMPP p v


-- | 6.6.4. ORG *

data ORG = ORG [[String]]
  deriving (Show)

instance Write ORG where
  write (ORG sss) = "ORG:" ++ fwrite sss ++ nl

org :: Parser ORG
org = do
  istring "ORG"
  char ':'
  v <- propFields
  crlf
  return $ ORG v


-- | 6.7.2. NOTE *

data NOTE = NOTE { note_param :: Parameters
                 , note_value :: String
                 } deriving (Show)

instance Write NOTE where
  write n = "NOTE" ++
            write (note_param n) ++
            ":" ++
            note_value n ++
            nl

note :: Parser NOTE
note = do
  istring "NOTE"
  p <- params
  char ':'
  v <- manyTill anyChar crlf
  return $ NOTE p v

-- | 6.7.3. PRODID *1

data PRODID = PRODID String
  deriving (Show)

instance Write PRODID where
  write (PRODID s) = "PRODID:" ++ s ++ nl
  
prodid :: Parser PRODID
prodid = do
  istring "PRODID"
  char ':'
  v <- manyTill anyChar crlf
  return $ PRODID v


-- | 6.7.4. REV *1

data REV = REV String
  deriving (Show)

instance Write REV where
  write (REV s) = "REV:" ++ s ++ nl

rev :: Parser REV
rev = do
  istring "REV"
  char ':'
  v <- manyTill anyChar crlf
  return $ REV v


-- | 6.7.8. URL *

data URL = URL { url_param :: Parameters
               , url_value :: String
               } deriving (Show)

instance Write URL where
  write u = "URL" ++
            write (url_param u) ++
            ":" ++
            url_value u ++
            nl

-- TODO issue warning if v not a valid URI, don't think I want to toss it out though..
url :: Parser URL
url = do
  istring "URL"
  p <- params
  char ':'
  v <- manyTill anyChar crlf
  return $ URL p v


-- | 6.7.9. VERSION 1

data VERSION = VERSION String
  deriving (Show)

instance Write VERSION where
  write (VERSION s) = "VERSION:" ++ s ++ nl

version :: Parser VERSION
version = do
  istring "VERSION:"
  v <- (string "4.0" <|> string "3.0" <|> string "2.1")
  crlf
  return $ VERSION v
  

-- | 6.10. Extended Properties and Parameters *

data X = X { x_group :: Maybe Group
           , x_name  :: String
           , x_value :: String
           } deriving (Show)

instance Write X where
  write x = mwrite (x_group x) ++
            x_name x ++
            ":" ++
            x_value x ++
            nl

x :: Parser X
x = do
  g <- try (do g <- group
               lookAhead (istring "X-")
               return g)
  n <- manyTill anyChar (char ':')
  v <- manyTill anyChar crlf
  return $ X g n v


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
  result <- parseFromFile vcardEntity "../samples.vcf"
  case result of
    Left err -> print err
    Right xs -> do print xs
                   putStr "\n"
                   putStrLn (lwrite xs)


-- sandbox

p :: Parser (String, String)
p = permute (pair
              <$?> ("", pa)
              <|?> ("", pb))
  where pair a b = (a, b)

pa :: Parser String
pa = do
  try (do optional (string "x1234")
          many1 (char 'a'))

pb :: Parser String
pb = do
  try (do optional (string "x1234")
          many1 (char 'b'))
