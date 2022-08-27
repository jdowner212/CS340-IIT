module MP4 where

import Data.Char


data State s a = State { run :: s -> Maybe (a, s) }


instance Functor (State s) where
  fmap f st = State $ \s -> case run st s of
                              Nothing -> Nothing
                              Just (x, s') -> Just (f x, s')


instance Applicative (State s) where
  pure x = State $ \s -> Just (x, s)
  stf <*> stx = State $ \s -> case run stf s of
                                Nothing -> Nothing
                                Just (f, s') -> run (f <$> stx) s'


instance Monad (State s) where
  st >>= f = State $ \s -> case run st s of
                             Nothing -> Nothing
                             Just (x, s') -> run (f x) s'


class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a

  many :: f a -> f [a]
  some :: f a -> f [a]

  many x = some x <|> pure []
  some x = pure (:) <*> x <*> many x


instance Alternative (State s) where
  empty = State $ \_ -> Nothing
  p <|> q = State $ \s -> case run p s of
                            Nothing -> run q s
                            r -> r


type Parser a = State String a


-----------------------------------------------------------------
--------------------------- Utilities ---------------------------
-----------------------------------------------------------------

sat :: (Char -> Bool) -> Parser Char
sat p = do x <- char
           if p x then return x else State (\_ -> Nothing)

char :: Parser Char
char = State $ \input -> case input of "" -> Nothing
                                       (x:xs) -> Just (x, xs)

char_input :: Char -> Parser Char
char_input c = sat (==c)

string :: Parser String
string = onePlus char

string_input :: String -> Parser String
string_input "" = return ""
string_input (x:xs) = do char_input x
                         string_input xs
                         return $ x:xs

space :: Parser ()
space = do many $ sat isSpace
           return ()

token :: Parser a -> Parser a
token p = do space
             x <- p
             space
             return x

symbol :: String -> Parser String
symbol s = token (string_input s)

pOr :: Parser a -> Parser a -> Parser a
p `pOr` q = State $ \s -> case run p s of 
                               Nothing -> run q s
                               r -> r

onePlus :: Parser a -> Parser [a]
onePlus p = do x <- p 
               xs <- onePlus p `pOr` return []
               return $ x:xs

includes :: Char -> [Char] -> Bool
includes l [] = False
includes l (a:z) | l == a = True
                 | otherwise = includes l z

-----------------------------------------------------------------
------------------------ Helper Functions -----------------------
-----------------------------------------------------------------

isLetter' :: Char -> Bool
isLetter' l = includes l $ ['a'..'z'] ++ ['A'..'Z']

tok_letter :: Parser [Char]
tok_letter = token (onePlus (sat isLetter'))

-----------------------------------------------------------------

isLowerLetter :: Char -> Bool
isLowerLetter l = includes l $ ['a'..'z']

tok_lowerLetters :: Parser [Char]
tok_lowerLetters = token (onePlus (sat isLowerLetter))

-----------------------------------------------------------------

is_alphaNum :: Char -> Bool
is_alphaNum l = includes l $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

alphaNums :: Parser [Char]
alphaNums = onePlus (sat is_alphaNum)

tok_alphaNums:: Parser [Char]
tok_alphaNums = token alphaNums

-----------------------------------------------------------------

is_alphaNumDash :: Char -> Bool
is_alphaNumDash l = includes l $ ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'] ++ ['-']

alphaNumDashes :: Parser [Char]
alphaNumDashes = onePlus (sat is_alphaNumDash)

-----------------------------------------------------------------

return_line :: Parser String                   
return_line = do space
                 string_input "return"
                 space
                 r <- alphaNumDashes
                 symbol ";"
                 return r

-----------------------------------------------------------------

func_name :: Parser String
func_name = do l <- (sat isLowerLetter)
               ls <- alphaNums `pOr` return []
               return (l:ls)

typeName :: Parser String
typeName = do xs <- string_input "int" <|> string_input "char"
              return xs

varName :: Parser String
varName = do first <- tok_lowerLetters
             rest <- tok_alphaNums <|> string_input ""
             return (first ++ rest)

-----------------------------------------------------------------

param :: Parser String
param = do typeName
           space
           p <- varName
           return p

empty_tup :: Parser [String]
empty_tup = do symbol "("
               space
               symbol ")"
               return []

param_tup :: Parser [String]
param_tup = do symbol "("
               space
               p <- param
               ps <- many (do symbol ","
                              space
                              param)
               symbol ")"
               return (p:ps)

param_curly :: Parser [String]
param_curly = do typeName
                 space
                 p <- varName
                 ps <- many (do symbol ","
                                space
                                varName)
                 symbol ";"
                 return (p:ps)

params_curly :: Parser [String]
params_curly = do ps <- many (do p <- param_curly
                                 return p)
                  return (concat ps)

-----------------------------------------------------------------

parseVarsVals :: Parser [String]
parseVarsVals = do varName
                   space
                   symbol "="
                   space
                   v <- alphaNumDashes
                   symbol ";"
                   space
                   return [v]

-----------------------------------------------------------------



{-
  Parses a limited C function in order to obtain:

  1. The name of the function
  2. A list of the names of the parameters of the function
  3. A list of the names of the local variables declared within the function
  4. The variable name or integer value returned by the function (as a string),
     or the empty string if there is no return statement.

  See the writeup for examples.
-}


funcDef :: Parser (String,[String],[String],String)
funcDef = do
              t1 <- typeName
              space
              f <- func_name
              p1 <- param_tup `pOr` empty_tup
              space
              symbol "{"
              space
              typed_vars <- params_curly `pOr` return []
              many (do parseVarsVals) `pOr` return []
              space
              ret <- return_line `pOr` return []
              symbol "}"
              return (f,p1, typed_vars, ret)











