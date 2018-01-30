import Data.Char
import Data.Tuple
import Text.Printf
import Prelude

data Stack a = NilStack
             | StackElem a (Stack a)
     deriving (Show)

type Opr = Char
type ParenL = Char
type ParenR = Char
type Opt = Char
data Token =  Opr
            | ParenL
            | ParenR
            | Opt
    deriving Show



main = do  
    putStrLn "Enter your Expression"  
    expression <- getLine  
    print (convertAndCalculate expression)

isEmpty :: (Stack a) -> Bool
isEmpty NilStack = True
isEmpty (StackElem _ _) = False

size :: (Stack a) -> Int
size NilStack = 0
size (StackElem _ rst) = 1 + (size rst)

peek :: (Stack a) -> a
peek NilStack = error "Attempt to peek empty stack."
peek (StackElem item _) = item

push :: a -> (Stack a) -> (Stack a)
push = StackElem 

pop :: (Stack a) -> (Stack a) 
pop NilStack = error "Attempt to pop empty stack."
pop (StackElem _ rst) = rst

st1 = push 3 (push 5 (push 8 (push 1 NilStack)))

isParenL :: (ParenL) -> Bool
isParenL x 
    |x == '('   = True
    |otherwise = False

isParenR :: (ParenR) -> Bool
isParenR x 
    |x == ')'   = True
    |otherwise = False

isOpt :: (Opt) -> Bool
isOpt x 
    |x == '+'   = True
    |x == '-'   = True
    |x == '/'   = True
    |x == '*'   = True
    |otherwise = False

isPlus :: Char -> Bool
isPlus x 
    |x == '+'   = True
    |otherwise = False

isMinus :: Char -> Bool
isMinus x 
    |x == '-'   = True
    |otherwise = False

isMult :: Char -> Bool
isMult x 
    |x == '*'   = True
    |otherwise = False

isDivide :: Char -> Bool
isDivide x 
    |x == '/'   = True
    |otherwise = False
    
isOpr :: (Opr) -> Bool
isOpr x
     |isParenL x == True = False 
     |isParenR x == True = False 
     |isOpt x == True = False 
     |otherwise = True

infixToPostFix :: [Char] -> [Char]
infixToPostFix x = handleInput x [] NilStack

--Invoke with "Expression" [] push $ NilStack
handleInput :: [Char] -> [Char] -> (Stack Char) ->[Char]
handleInput [] solution NilStack = solution
handleInput [] solution (StackElem y z) = popToEmpty solution (StackElem y z)
handleInput (x:xs) lst NilStack
     | isParenL x == True = handleInput xs lst (handleParenL NilStack x) 
     | isParenR x == True = handleInput xs (lst ++  (fst(handleParenR [] NilStack))) (snd(handleParenR [] NilStack))
     | isOpt x == True = handleInput xs ( lst  ++ ['$'] ++ handleOpt [] NilStack) (push x NilStack) 
     | isOpr x == True = handleInput xs ( lst ++ [x]) NilStack
handleInput (x:xs) lst (StackElem y z)
     | isParenL x == True = handleInput xs  lst (handleParenL (StackElem y z) x) 
     | isParenR x == True = handleInput xs ( lst ++ ( fst ( handleParenR [] ( StackElem y z )))) (snd(handleParenR [] (StackElem y z)))
     | isOpt x == True = handleInput xs (lst ++ ['$'] ++ (fst(handleOpt' [] (StackElem y z)))) (push x (snd(handleOpt' [] (StackElem y z)))) 
     | isOpr x == True = handleInput xs ( lst ++ [x]) (StackElem y z)

handleParenL :: (Stack Char) -> Char -> (Stack Char)
handleParenL NilStack x = push x NilStack 
handleParenL (StackElem item NilStack) x = push x (StackElem item NilStack)
handleParenL (StackElem y z) x
     |(size (StackElem y z) > 1) == True = push x (StackElem y z) 

handleOpt :: [Char] -> (Stack Char) -> [Char] -- Create handleOpt' that returns a tuple of [Char] and (Stack Char)
handleOpt lst NilStack = lst

handleOpt' :: [Char] -> (Stack Char) -> ( [Char], (Stack Char))
handleOpt' lst NilStack = (lst, NilStack)
handleOpt' lst (StackElem y z) 
     |isParenL(peek(StackElem y z)) == True = (lst, (StackElem y z))
     |otherwise = (lst ++ [peek(StackElem y z)],pop (StackElem y z))

popToEmpty :: [Char] -> (Stack Char) -> [Char] 
popToEmpty lst NilStack = lst
popToEmpty lst (StackElem y z) = popToEmpty (lst ++ [peek(StackElem y z)]) (pop(StackElem y z)) 

handleParenR :: [Char] -> (Stack Char) -> ([Char], (Stack Char))
handleParenR lst NilStack = (lst, NilStack)
handleParenR lst (StackElem y z) 
     |isParenL(peek(StackElem y z)) == True = ([], pop (StackElem y z))
     |otherwise = (lst ++ [peek(StackElem y z)], pop (StackElem y z))

getFirstNumber :: [Char] -> Float
getFirstNumber x = (char2float(joinChar x))

char2float :: [Char] -> Float
char2float n =  read n :: Float


cleanList :: [Char] -> [Char]
cleanList (x:xs)
     |isOpt x == True = (x:xs)
     |x == '$' = xs
     |isOpr x == True = cleanList xs

joinChar :: [Char] -> [Char]
joinChar lst = (concatUntilDelimeter lst [])

concatUntilDelimeter :: [Char] -> [Char] -> [Char]
concatUntilDelimeter (x:xs) []
     | x == '$' = []
     | isOpt x  == True = concatUntilDelimeter xs []
     | isOpr x == True = concatUntilDelimeter xs [x]
     | isParenL x == True = []
concatUntilDelimeter (x:xs) lst
     | x == '$' = lst
     | isOpt x  == True = lst
     | isOpr x == True = concatUntilDelimeter xs (lst ++ [x])
     | isParenL x == True = lst

popTop2 :: (Stack a) -> (Stack a)
popTop2 (StackElem y z) = pop(pop(StackElem y z))

calculateValue :: Float -> Float -> Char -> Float
calculateValue f1 f2 x
     |isPlus x == True = (f1 + f2)
     |isMinus x == True = (f1 - f2)     
     |isMult x == True = (f1 * f2)
     |isDivide x == True = (f1 / f2)

postFixCalulator :: [Char] -> (Stack Float) -> Float
postFixCalulator (x:xs) NilStack
     |isOpr x == True = postFixCalulator (cleanList(x:xs)) (push (getFirstNumber(x:xs)) NilStack)
postFixCalulator (x:[]) (StackElem y z)
     | isParenL x == True = (peek(StackElem y z))
postFixCalulator (x:xs) (StackElem y z)
     |isOpr x == True = postFixCalulator (cleanList(x:xs)) (push (getFirstNumber(x:xs)) (StackElem y z))
     |isOpt x == True = postFixCalulator xs (push (calculateValue(peek(pop(StackElem y z)))(peek(StackElem y z))x) (popTop2(StackElem y z)))
     |x == '$' = postFixCalulator xs (StackElem y z)
     |isParenL x == True = (peek(StackElem y z)) 
postFixCalulator [] (StackElem y z) = (peek( StackElem y z))
-- postFixCalulator [] (StackElem y NilStack) = (peek(StackElem y NilStack))

hasTrue :: [Bool] -> Bool
hasTrue (x:xs)
     |x == True = True
     |otherwise = hasTrue xs
hasTrue [] = False

checkForMoreOperators :: [Char] -> Bool
checkForMoreOperators x = isLastOperator x False

isLastOperator :: [Char] -> Bool -> Bool
isLastOperator [] False = False
isLastOperator (x:xs) False
     |isOpt x == True = isLastOperator xs True
     |otherwise = isLastOperator xs False
isLastOperator [] True = True
isLastOperator (x:xs) True
     |isOpt x == True = False
     |otherwise = isLastOperator xs True

convertAndCalculate :: [Char] -> Float
convertAndCalculate x = postFixCalulator (infixToPostFix x) NilStack