module Main where
-- libraries
import qualified Text.ParserCombinators.Parsec.Token as Token
import qualified Data.Map.Strict as Map
import Data.List (intercalate)
import Control.Monad (forM_)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import System.IO
import Control.Monad
import Data.List (foldl')
import Control.Monad(forM_)

-- Defining all the data structures

-- Boolean expressions
data BExpr = BoolConst Bool
           | Not BExpr
           | BBinary BBinOp BExpr BExpr
           | RBinary RBinOp AExpr AExpr


-- Binary boolean operators
data BBinOp = And | Or deriving (Show)

-- Relational operators
data RBinOp = Greater | Less | Eq deriving (Show)

-- Arithmetic expressions
data AExpr = Var String
           | IntConst Integer
           | Neg AExpr
           | ABinary ABinOp AExpr AExpr


-- Arithmetic operators
data ABinOp = Add
            | Subtract
            | Multiply
            | Divide
            | Expon


-- Statements
data Stmt = Seq [Stmt]
          | Assign String AExpr
          | If BExpr Stmt Stmt
          | While BExpr Stmt
          | Skip


-- define syntax for While Language
languageDef =
  emptyDef { Token.commentStart    = "/*"
           , Token.commentEnd      = "*/"
           , Token.commentLine     = "//"
           , Token.identStart      = letter
           , Token.identLetter     = alphaNum
           , Token.opLetter        = oneOf"="
           , Token.reservedNames   = [ "if"
                                     , "then"
                                     , "else"
                                     , "while"
                                     , "do"
                                     , "skip"
                                     , "true"
                                     , "false"
                                     , "not"
                                     , "and"
                                     , "or"
                                     ]
           , Token.reservedOpNames = ["+", "-", "*", "/", ":="
                                     , "<", ">", "∨", "∧", "¬", "=", "^"
                                     ]
           }

-- create a lexer
lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer -- parses an identifier
reserved   = Token.reserved   lexer -- parses a reserved name
reservedOp = Token.reservedOp lexer -- parses an operator +-*< > etc
parens     = Token.parens     lexer -- parses surrounding parenthesis: ()
integer    = Token.integer    lexer -- parses an integer int
semi       = Token.semi       lexer -- parses a semicolon  ;
whiteSpace = Token.whiteSpace lexer -- parses whitespace " "
brace      =  Token.braces    lexer -- parses braces {}

whileParser :: Parser Stmt
whileParser = whiteSpace >> statement

statement :: Parser Stmt
statement =   parens statement
          <|> brace statement
          <|> sequenceOfStmt

-- If there are a sequences of statements separated by a semicolon
-- use sepBy1 to parse at least one statement
sequenceOfStmt =
  do list <- (sepBy1 statement' semi)
     -- If there's only one statement return it
     return $ if length list == 1 then head list else Seq list

-- check which kind of statement it is
statement' :: Parser Stmt
statement' =   ifStmt
           <|> whileStmt
           <|> skipStmt
           <|> assignStmt

-- define the parsers for all the possible statements
ifStmt :: Parser Stmt
ifStmt =
  do reserved "if"
     cond  <- bExpression
     reserved "then"
     stmt1 <- statement
     reserved "else"
     stmt2 <- statement
     return $ If cond stmt1 stmt2

whileStmt :: Parser Stmt
whileStmt =
  do reserved "while"
     cond <- bExpression
     reserved "do"
     stmt <- statement
     return $ While cond stmt

assignStmt :: Parser Stmt
assignStmt =
  do var  <- identifier
     reservedOp ":="
     expr <- aExpression
     return $ Assign var expr

skipStmt :: Parser Stmt
skipStmt = reserved "skip" >> return Skip

-- expression parsers
aExpression :: Parser AExpr
aExpression = buildExpressionParser aOperators aTerm

bExpression :: Parser BExpr
bExpression = buildExpressionParser bOperators bTerm

-- define lists of operator precedence and associativity
-- Added "^" exponentiation to the top since it has a
-- higher precedence than multiplication
aOperators = [ [Prefix (reservedOp "-"   >> return (Neg             ))          ]
             , [Infix  (reservedOp "^"   >> return (ABinary Expon))   AssocLeft ],
             [Infix  (reservedOp "*"   >> return (ABinary Multiply)) AssocLeft, Infix  (reservedOp "/"   >> return (ABinary Divide  )) AssocLeft]
             , [Infix  (reservedOp "+"   >> return (ABinary Add     )) AssocLeft,
                Infix  (reservedOp "-"   >> return (ABinary Subtract)) AssocLeft]
              ]

bOperators = [ [Prefix (reservedOp "¬" >> return (Not             ))          ]
             , [Infix  (reservedOp "∧" >> return (BBinary And     )) AssocLeft,
                Infix  (reservedOp "∨" >> return (BBinary Or      )) AssocLeft]

             ]

aTerm =  parens aExpression
     <|> liftM Var identifier
     <|> liftM IntConst integer

bTerm =  parens bExpression
     <|> (reserved "true"  >> return (BoolConst True ))
     <|> (reserved "false" >> return (BoolConst False))
     <|> rExpression

-- parser for relational expressions
rExpression =
  do a1 <- aExpression
     op <- relation
     a2 <- aExpression
     return $ RBinary op a1 a2

relation =   (reservedOp ">" >> return Greater)
         <|> (reservedOp "<" >> return Less)
         <|> (reservedOp "=" >> return Eq)

parseString :: String -> Stmt
parseString str = case parse whileParser "" str of
  Left e  -> error $ show e
  Right r -> r

-- Evaluating boolean expressions
-- takes in a boolexpression and a map and outputs
--  a boolean ,, checks for all cases
evalBool:: BExpr -> Map.Map String Integer-> Bool
evalBool bexp store =
  case bexp of
    BoolConst x ->
      case x of
        True  -> True
        False -> False
    Not x -> not $! evalBool x store
    BBinary op b1 b2 ->
      case op of
        And -> evalBool b1 store && evalBool b2 store
        Or  -> evalBool b1 store || evalBool b2 store
    RBinary op a1 a2 ->
      case op of
        Greater -> evalA a1 store > evalA a2 store
        Less    -> evalA a1 store < evalA a2 store
        Eq      -> evalA a1 store == evalA a2 store

-- Evaluating arithmetic expressions
-- takes in arithmetic expression and a map and
-- outputs an integer

evalA :: AExpr -> Map.Map String Integer -> Integer
evalA expr mapstore =
    case expr of
      Var strx ->
        -- check if map has the string variable
        case (Map.member strx mapstore) of
          True -> mapstore Map.! strx
          False -> do -- if it doesn't add var: 0 to the map
            let mapstore' = Map.insert strx 0 mapstore
            mapstore' Map.! strx
      IntConst n -> n   -- return integer
      Neg e1 -> negate $! evalA e1 mapstore
      ABinary op e1 e2 ->
        case op of
          Add       ->
            evalA e1 mapstore + evalA e2 mapstore
          Subtract  ->
            evalA e1 mapstore - evalA e2 mapstore
          Multiply  ->
            evalA e1 mapstore * evalA e2 mapstore
          Divide    ->
            evalA e1 mapstore `div` evalA e2 mapstore
          Expon     ->
            evalA e1 mapstore ^ evalA e2 mapstore

--interpreter takes in statement, map and returns map
interpret :: Stmt -> Map.Map String Integer -> Map.Map String Integer
interpret var store =
  case var of
      -- assigns integer to variable
      Assign name a1    -> Map.insert name (evalA a1 store) store
      If b1 s1 s2       -> do
        if evalBool b1 store == True
          then interpret s1 store
          else interpret s2 store
      Skip              -> store
      While b1 s1       ->
        if evalBool b1 store
          then interpret (While b1 s1) (interpret s1 store)
          else store
      Seq []           -> store
      Seq (x:xs)       -> do -- recursively call interpret on rest of stmt
        interpret (Seq xs) (interpret x store)

cleanData:: (Stmt, String) -> String
cleanData (stmt, mapstore)  = do
  "⇒ "++show stmt++ ", "++ mapstore

helperstep::(Stmt, Map.Map String Integer, [String]) -> Maybe(Stmt, Map.Map String Integer,[String])
helperstep (ast, state, result) =
  case smallstep(ast, state) of
    Just(ast', state') -> do
            let smap = printMap(state')
            let resultData = cleanData(ast', smap)
            let cleanData = filter(/= '\n') resultData
            helperstep(ast', state', result ++ [cleanData])
    Nothing            ->   do
            Just(ast, state, result)


-- input: AST, s
-- returns: remaining AST, s'
smallstep:: (Stmt, Map.Map String Integer) -> Maybe(Stmt, Map.Map String Integer)
smallstep (stmt, store) =
  case stmt of
    Assign str a1 -> Just(Skip, Map.insert str (evalA a1 store) store)
    Skip          -> Nothing
    If b1 c1 c2       -> do
      if evalBool b1 store == True
        then Just(c1, store)
        else Just(c2, store)
    Seq []           ->  Nothing
    Seq (x:xs)       -> do
      case smallstep(x, store) of
        Just(x', store')  -> Just(Seq ([x'] ++ xs), store')
        Nothing           -> do
          if null xs
            then Nothing
            else Just(Seq xs, store)
    While b1 s1       ->
      if evalBool b1 store
        then Just (Skip, store)
        else Just(Skip, store)


-- pretty printing
-- Boolean expressions
instance Show BExpr where
  show (BoolConst True) = "true"
  show (BoolConst False) = "false"
  show (RBinary Eq a b) = "(" ++ show a ++ "=" ++ show b ++ ")"
  show (RBinary Less a b) = "(" ++ show a ++ "<" ++ show b ++ ")"
  show (Not a) = "¬" ++ show a
  show (BBinary Or a b) = "(" ++ show a ++ "∨" ++ show b ++ ")"
  show (BBinary And a b) = "(" ++ show a ++ "∧" ++ show b ++ ")"

--Arithmetic expressions
instance Show AExpr where
  show (IntConst n) = show n
  show (Var s) = s
  show (ABinary Add a b) = "(" ++ show a ++ "+" ++ show b ++ ")"
  show (ABinary Subtract a b) = "(" ++ show a ++ "-" ++ show b ++ ")"
  show (ABinary Multiply a b) = "(" ++ show a ++ "*" ++ show b ++ ")"
  show (ABinary Divide a b) = "(" ++ show a ++ "/" ++ show b ++ ")"
  show (ABinary Expon a b) = "(" ++ show a ++ "^" ++ show b ++ ")"

-- statements
instance Show Stmt where
  show Skip = "skip"
  show (Assign s a) = s ++ " := " ++ show a
  show (Seq a ) =
    intercalate "; " $ map show a
  show (If c a b) = "if " ++ show c ++ " then { " ++ show a ++ " } else { " ++ show b ++ " }"
  show (While b c) = "while " ++ show b ++ " do { " ++ show c ++ " }"

-- utility function for printing the map in
-- the correct format
printMap :: Map.Map String Integer ->  String
printMap newMap = do
  if newMap == Map.empty
    then "{}"
    else
      do
        let extra = Map.toList newMap
        let func = \(key, value) -> "{"++ key ++ " → " ++ show value ++ "}"
        unlines $ map func extra


main = do
    -- getContents reads everything from standard input
    contents <-  getContents
    let inputStr = (unlines (lines contents))
    --let ast = parseString inputStr
    let ast = parseString contents
    let store = Map.empty
    --print(ast)

    --let (stmt, s_map) = smallstep(ast, store)
    --let (stmt, state) = helperstep(ast, store)
    -- let output = helperstep(ast, store)
    let Just(stmt, s_map, output) = helperstep(ast, store, [])
    mapM_ putStrLn(output)
