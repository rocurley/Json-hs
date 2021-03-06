
import qualified Data.Map as M
import Text.ParserCombinators.Parsec
import Data.Char
import Control.Monad

data JValue  = JString String
    | JNum Float
    | JObject (M.Map String JValue)
    | JArray [JValue]
    | JBool Bool
    | Null
deriving(Eq)

instance Show JValue where
    show (JString str) =
        '"' : show' str ++ "\""
        where
            show' "" = ""
            show' (x:xs)
                |x=='"'  = "\\\"" ++ show' xs
                |x=='\\' = "\\\\" ++ show' xs
                |x=='/'  = "\\/"  ++ show' xs
                |x=='\b' = "\\b"  ++ show' xs
                |x=='\f' = "\\f"  ++ show' xs
                |x=='\n' = "\\n"  ++ show' xs
                |x=='\r' = "\\r"  ++ show' xs
                |x=='\t' = "\\t"  ++ show' xs
                |otherwise = x : show' xs
    show (JNum num) = show num --Check this out later
    show (JArray lst) = show lst
    show (JObject obj)
        |M.null obj = "{}"
        |otherwise = (('{' :) . tail .
            M.foldrWithKey (\ x k acc -> ","++ show (JString x) ++ " : " ++ show k ++ acc) "}") obj
    show (JBool b) = if b then "true" else "false"
    show Null  = "null"

instance Read JValue where
    readsPrec p str = either (const []) (\ x-> [(x,"")]) (parse jsonValue "JSON parse error" str)

jsonChar :: Parser Char
jsonChar =
    (char '\\' >> (
    ((return . jsonSpecialChar . toLower) =<< oneOf ('"':'\\':"/bfnrt")) <|>
    ((return . unicodeLookup)   =<< (oneOf "uU" >> replicateM 4 hexDigit))))
    <|> noneOf "\""
    where jsonSpecialChar '"'= '"'
         jsonSpecialChar '\\'= '\\'
         jsonSpecialChar 'b'= '\b'
         jsonSpecialChar 'f'= '\f'
         jsonSpecialChar 'n'= '\n'
         jsonSpecialChar 'r'= '\r'
         jsonSpecialChar 't'= '\t'
         unicodeLookup x =toEnum (read ("0x"++x) ::Int) ::Char
                 
jsonString :: Parser String
jsonString = do
    char '"'
    out <- many jsonChar
    char '"'
    return out
                
parseDelimited :: Parser a -> Parser b -> Parser [b]
parseDelimited dParser vParser=
    try $ do
        x<-vParser
        xs<-many (dParser >> vParser)
        return (x:xs)
    <|> return []
    
stringNoCase :: String -> Parser String
stringNoCase str = sequence [char (toLower x) <|> char (toUpper x) |x <- str]

jsonSpecial :: Parser JValue
jsonSpecial =
    (stringNoCase "true"  >> return (JBool True) ) <|>
    (stringNoCase "false" >> return (JBool False)) <|>
    (stringNoCase "null"  >> return Null )
    
jsonNum :: Parser JValue
jsonNum =
    do
        sign  <- (char '-' >> return (-1))   <|> return 1
        iPart <- (char '0' >> return "0") <|> many digit
        dPart <- (char '.' >> many digit) <|> return "0"
        expn  <- (do
            char 'e' <|> char 'E'
            sign <- (char '-' >> return (-1)) <|> (char '+' >> return 1) <|> return 1
            digits <- many1 digit
            return ((sign * read digits) :: Float)
            )
           <|> return 0
        return $ JNum $ sign * read (iPart ++ "." ++ dPart) * 10 ** expn

jsonValue :: Parser JValue
jsonValue = do
    spaces
    out<-((return . JString) =<< jsonString) <|>
        jsonArray  <|>
        jsonObject <|>
        jsonSpecial <|>
        jsonNum
    spaces
    return out

jsonArray :: Parser JValue
jsonArray = do 
    char '['
    out <- parseDelimited (char ',') jsonValue
    char ']'
    return $ JArray out

jsonKV :: Parser (String,JValue)
jsonKV = do
    spaces
    k <- jsonString
    spaces
    char ':'
    v <- jsonValue
    return (k,v)

jsonObject :: Parser JValue
jsonObject = do 
    char '{'
    out <- parseDelimited (char ',') jsonKV
    char '}'
    return $ JObject $ M.fromList out