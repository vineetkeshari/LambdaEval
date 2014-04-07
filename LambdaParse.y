-- 02/24/2014
-- [FIXED] Grammar for parsing expressions in the lambda calculus
-- Author: Vineet Keshari (EID: vk3226)

{
module LambdaParse where
import Data.Char
import Lambda
import Lexer
}

%name parser
%tokentype { Token }

%token 
    id     { TokenIdent $$ }
    '\\'   { Symbol "\\" }
    '.'    { Symbol "." }
    '('    { Symbol "(" }
    ')'    { Symbol ")" }

%%

Exp : LamExp                          { $1 }
    | NonLamExp                       { $1 }

NonLamExp  : AssociativeExp           { $1 }
           | AssociativeExp LamExp    { App $1 $2 }

AssociativeExp : AssociativeExp Prim  { App $1 $2 }
               | Prim                 { $1 }

LamExp : '\\' id '.' Exp              { Abs $2 $4 }

Prim : id                             { Var $1 }
     | '(' Exp ')'                    { $2 }

{

symbols = [".", "\\", "(", ")"]
keywords = []
parseExp str = parser (lexer symbols keywords str)

parseInput = do
  input <- getContents
  print (parseExp input)

}

