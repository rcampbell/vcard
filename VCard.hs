-- {-# LANGUAGE OverloadedStrings #-}
module VCard where

import System.IO (withFile, IOMode(WriteMode), hSetEncoding, utf8, hPutStr)
import Text.ParserCombinators.Parsec
import Text.Parsec.Char (crlf)
import Text.Parsec.String (parseFromFile)
import Text.Parsec.Perm (permute, (<$$>), (<$?>), (<||>), (<|?>))
import Text.Regex (mkRegex, subRegex)
import Control.Monad (liftM)
-- import Network.URI (URI, parseURI) TODO do I want to even validate?
-- import qualified Data.Text as T
import Data.Char (toLower, toUpper, isSpace)
import Data.List (intercalate)
import Data.Time.Format (parseTimeM, formatTime, defaultTimeLocale)
import Data.Time.Clock (UTCTime)
import Test.HUnit

{- TODO

 * The folding (wrapping) logic isn't implemented, breaks

 - Parameters will need the same many (try permute) fix

 - Update params to assume ending :

 - Date, time types, https://two-wrongs.com/haskell-time-library-tutorial

 - There is tons of duplication among similar property parsers. I'm hesitant to
   start factoring it out before I make them more fully correct, supporting each
   property's nuances... 

 - Look into moving from String to String or ByteString. More generally, no
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

-- | 3.2. Line Delimiting and Folding

unfold s = subRegex (mkRegex "\r\n[ \t]") s ""


-- | 3.3. ABNF Format Definition 1*


data VCard = VCard
  { vc_contentlines :: ContentLines
  } deriving (Show)

instance Write VCard where
  write c = "BEGIN:VCARD" ++ nl ++
            write (vc_contentlines c) ++
            "END:VCARD" ++ nl

data Foo = Foo Int deriving (Eq, Ord)

vcardEntity = many1 vcard

vcard :: Parser VCard
vcard = do
  begin
  cls <- manyTill (try contentLines) end
  return $ VCard (foldl1 merge cls)
  -- TODO this is the ugliest thing I've ever written
  where merge (ContentLines
                version
                source
                kind
                fn
                n
                nickname
                bday
                adr
                tel
                email
                impp
                org
                note
                prodid
                rev
                url
                x)
              (ContentLines
                version'
                source'
                kind'
                fn'
                n'
                nickname'
                bday'
                adr'
                tel'
                email'
                impp'
                org'
                note'
                prodid'
                rev'
                url'
                x') =
          ContentLines
            (max version version')
            (source ++ source')
            (max kind kind')
            (fn ++ fn')
            (max n n')
            (nickname ++ nickname')
            (max bday bday')
            (adr ++ adr')
            (tel ++ tel')
            (email ++ email')
            (impp ++ impp')
            (org ++ org')
            (note ++ note')
            (max prodid prodid')
            (max rev rev')
            (url ++ url')
            (x ++ x')


-- | Content Lines

data Name = SOURCE | KIND | FN | N | NICKNAME | PHOTO | BDAY | ANNIVERSARY |
            GENDER | ADR | TEL | EMAIL | IMPP | LANG | TZ | GEO | TITLE |
            ROLE | LOGO | ORG | MEMBER | RELATED | CATEGORIES | NOTE | PRODID |
            REV | SOUND | UID | CLIENTPIDMAP | URL | KEY | FBURL | CALADRURI |
            CALURI | XML | VERSION | X_NAME String
  deriving (Show, Eq, Ord)

data Value = TEXT String | TEXT' [String] | TEXT'' [[String]]
  deriving (Show, Eq, Ord)

-- TODO why can't records have different type and constructor names..?
data CL = CL { cl_group :: Maybe Group
             , cl_name  :: Name
             , cl_param :: Parameters -- [Parameter]
             , cl_value :: Value
             } deriving (Show, Eq, Ord)

instance Write Value where
  write (TEXT s) = s
  write (TEXT' ss) = vwrite ss
  write (TEXT'' sss) = fwrite sss
  

instance Write CL where
  write cl = mwrite (cl_group cl) ++
             name (cl_name cl)  ++
             write (cl_param cl) ++
             ":" ++
             write (cl_value cl) ++
             nl
    where name (X_NAME s) = s
          name n = show n


-- | Groups

data Group = Group String
  deriving (Show, Eq, Ord)

instance Write Group where
  write (Group s) = s ++ "."

group :: Parser (Maybe Group)
group = do
  g <- optionMaybe (try group)
  return $ Group `liftM` g
    where group = manyTill (alphaNum <|> char '-') (char '.')

prefix :: String -> Parser (Maybe Group)
prefix name = try (do g <- group
                      istring name
                      return g)


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
                             } deriving (Show, Eq, Ord)

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
  deriving (Show, Eq, Ord)

instance Write VALUE where
  write (VALUE s) = ";VALUE=" ++ s

valueParam :: Parser VALUE
valueParam = do
  istring ";VALUE="
  v <- many (noneOf ",:;")
  return $ VALUE v


-- | 5.3 PREF

data PREF = PREF Integer -- an integer between 1 and 100
  deriving (Show, Eq, Ord)

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
  deriving (Show, Eq, Ord)

instance Write TYPE where
  write (TYPE ss) = ";TYPE=" ++ (intercalate "," ss)

typeParam :: Parser TYPE
typeParam = do
  istring ";TYPE="
  vs <- paramValues
  return $ TYPE vs


{- | 6. vCard Properties -}

data ContentLines = ContentLines
  { prop_version  :: Maybe CL
  , prop_source   :: [CL]
  , prop_kind     :: Maybe CL
  , prop_fn       :: [CL]
  , prop_n        :: Maybe CL
  , prop_nickname :: [CL]
  , prop_bday     :: Maybe CL
  , prop_adr      :: [CL]
  , prop_tel      :: [CL]
  , prop_email    :: [CL]
  , prop_impp     :: [CL]
  , prop_org      :: [CL]
  , prop_note     :: [CL]
  , prop_prodid   :: Maybe CL
  , prop_rev      :: Maybe CL
  , prop_url      :: [CL]
  , prop_x        :: [CL]
  } deriving (Show)

instance Write ContentLines where
  write p = mwrite (prop_version p)  ++ -- ensure first
            lwrite (prop_source p)   ++
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

contentLines = permute (ContentLines
                         <$?> (Nothing, Just `liftM` version) 
                         <|?> ([], many1 source)
                         <|?> (Nothing, Just `liftM` kind)
                         <|?> ([], many1 fn) -- TODO req, but changed to opt for many try
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

source :: Parser CL
source = do
  g <- prefix (show SOURCE)
  p <- params
  char ':'
  v <- manyTill anyChar crlf
  return $ CL g SOURCE p (TEXT v)


-- | 6.1.4. KIND *1

kind :: Parser CL
kind = do
  g <- prefix (show KIND)
  p <- params
  char ':'
  v <- manyTill anyChar crlf
  return $ CL g KIND p (TEXT v)


-- | 6.2.1. FN 1*

fn ::Parser CL
fn = do
  g <- prefix (show FN)
  p <- params
  char ':'
  v <- manyTill anyChar crlf
  return $ CL g FN p (TEXT v)
  

-- | 6.2.2. N *1

n :: Parser CL
n = do
  g <- prefix (show N)
  p <- params
  char ':'
  v <- propFields 
  crlf
  return $ CL g N p (TEXT'' v)


-- | 6.2.3. NICKNAME *

nickname :: Parser CL
nickname = do
  g <- prefix (show NICKNAME)
  p <- params
  char ':'
  v <- propValues
  crlf
  return $ CL g NICKNAME p (TEXT' v)


-- | 6.2.4. PHOTO *

-- | 6.2.5. BDAY *1

bday :: Parser CL -- TODO date support
bday = do
  g <- prefix (show BDAY)
  p <- params
  char ':'
  v <- manyTill anyChar crlf
  return $ CL g BDAY p (TEXT v)
                 

-- | 6.3.1. ADR *

adr :: Parser CL
adr = do
  g <- prefix (show ADR)
  p <- params
  char ':'
  v <- propFields
  crlf
  return $ CL g ADR p (TEXT'' v)


-- | 6.4.1. TEL *

tel :: Parser CL
tel = do
  g <- prefix (show TEL)
  p <- params
  char ':'
  v <- manyTill anyChar crlf
  return $ CL g TEL p (TEXT v)


-- | 6.4.2. EMAIL *

email :: Parser CL
email = do
  g <- prefix (show EMAIL)
  p <- params
  char ':'
  v <- manyTill anyChar crlf
  return $ CL g EMAIL p (TEXT v)


-- | 6.4.3. IMPP *

-- TODO spit out some URI validation warning, though I don't think I want to toss them..
impp :: Parser CL
impp = do
  g <- prefix (show IMPP)
  p <- params
  char ':'
  v <- manyTill anyChar crlf
  return $ CL g IMPP p (TEXT v)


-- | 6.6.4. ORG *

org :: Parser CL
org = do
  g <- prefix (show ORG)
  p <- params
  char ':'
  v <- propFields
  crlf
  return $ CL g ORG p (TEXT'' v)


-- | 6.7.2. NOTE *

note :: Parser CL
note = do
  g <- prefix (show NOTE)
  p <- params
  char ':'
  v <- manyTill anyChar crlf
  return $ CL g NOTE p (TEXT v)


-- | 6.7.3. PRODID *1

prodid :: Parser CL
prodid = do
  g <- prefix (show PRODID)
  p <- params
  char ':'
  v <- manyTill anyChar crlf
  return $ CL g PRODID p (TEXT v)


-- | 6.7.4. REV *1

rev :: Parser CL
rev = do
  g <- prefix (show REV)
  p <- params
  char ':'
  v <- manyTill anyChar crlf
  return $ CL g REV p (TEXT v)


-- | 6.7.8. URL *

-- TODO issue warning if v not a valid URI, don't think I want to toss it out though..
url :: Parser CL
url = do
  g <- prefix (show URL)
  p <- params
  char ':'
  v <- manyTill anyChar crlf
  return $ CL g URL p (TEXT v)


-- | 6.7.9. VERSION 1

version :: Parser CL
version = do
  istring (show VERSION)
  p <- params
  char ':'
  v <- (string "4.0" <|> string "3.0" <|> string "2.1")
  crlf
  return $ CL Nothing VERSION p (TEXT v)
  

-- | 6.10. Extended Properties and Parameters *

x :: Parser CL
x = do
  g <- try (do g <- group
               lookAhead (istring "X-")
               return g)
  n <- manyTill (alphaNum <|> char '-') (oneOf ";:") -- 1*
  p <- params
  v <- manyTill anyChar crlf
  return $ CL g (X_NAME n) p (TEXT v)


{- | I/O -}

-- | Reading

readVCard :: FilePath -> IO ()
readVCard f = do
  contents <- readFile f
  let unfolded = unfold contents in
    case (parse vcardEntity f unfolded) of
      Left err -> print err
      Right cs -> putStr (lwrite cs)


-- | Writing, must be UTF-8 and should have a .vcf or .vcard extension

writeVCard :: FilePath -> VCard -> IO ()
writeVCard f c = do
  withFile f WriteMode $ \h -> do
    hSetEncoding h utf8
    hPutStr h (write c)






