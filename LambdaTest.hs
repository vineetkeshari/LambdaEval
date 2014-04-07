import Prelude hiding (LT, GT, EQ, id)
import Lambda
import LambdaParse
import Base

justVar = "x"
lamIdent = "(\\x.x)"
lamTru = "(\\t.\\f.t)"
lamFls = "(\\t.\\f.f)"
lamTest = "(\\l.\\m.\\n.l m n)"
lamAnd = "(\\b.\\c.b c" ++ lamFls ++ ")"
lamOr = "(\\b.\\c.b" ++ lamTru ++ "c)"
lamNot = "(\\b.b" ++ lamFls ++ lamTru ++ ")"
lamPair = "(\\f.\\s.\\b.b f s)"
lamFst = "(\\p. p" ++ lamTru ++ ")"
lamSnd = "(\\p. p" ++ lamFls ++ ")"
churchZero = "(\\s.\\z.z)"
churchSucc = "(\\n.\\s.\\z.s(n s z))"
churchOne = "(" ++ churchSucc ++ churchZero ++ ")"
churchTwo = "(" ++ churchSucc ++ churchOne ++ ")"
churchSum = "(\\m.\\n.\\s.\\z.m s(n s z))"
churchTimes = "(\\m.\\n.m(" ++ churchSum ++ "n)" ++ churchZero ++ ")"
churchIsZero = "(\\m.m(\\x." ++ lamFls ++ ")" ++ lamTru ++ ")"

mixedPair = "(" ++ lamPair ++ lamTru ++ churchOne ++ ")"
lamDangle = "(\\x.x " ++ lamIdent ++ ")"
justVarEval = "(" ++ lamIdent ++ "x y)"

outOfOrder = "(x " ++ lamIdent ++ " y z " ++ lamIdent ++ " t)"
churchOutOfOrder = "(" ++ churchOne ++ churchSucc ++ churchSucc ++ churchZero ++ churchOne ++ ")"

capture = "((\\x.\\y.x y) y " ++ churchZero ++")"
nestedCapture = "((\\x.\\y.x y (\\y.y)) y " ++ churchZero ++ ")"

lambda = do
  test "1: parse" parseExp justVar
  test "2: parse" parseExp lamIdent
  test "3: parse" parseExp lamTru
  test "4: parse" parseExp lamFls
  test "5: parse" parseExp lamTest
  test "6: parse" parseExp lamAnd
  test "7: parse" parseExp lamOr
  test "8: parse" parseExp lamNot
  test "9: parse" parseExp lamPair
  test "10: parse" parseExp lamFst
  test "11: parse" parseExp lamSnd
  test "12: parse" parseExp churchZero
  test "13: parse" parseExp churchSucc
  test "14: parse" parseExp churchOne
  test "15: parse" parseExp churchTwo
  test "16: parse" parseExp churchSum
  test "17: parse" parseExp churchTimes
  test "18: parse" parseExp churchIsZero
  test "19: parse" parseExp mixedPair
  test "20: parse" parseExp lamDangle
  test "21: parse" parseExp justVarEval
  test "22: parse" parseExp outOfOrder
  test "23: parse" parseExp churchOutOfOrder
  test "24: parse" parseExp capture
  test "25: parse" parseExp nestedCapture

  test "1: evalb" evalb (parseExp (lamTru ++ " x y"))
  test "1: evals" evals' (parseExp (lamTru ++ " x y"))
  test "2: evalb" evalb (parseExp (lamAnd ++ lamFls ++ lamTru))
  test "2: evals" evals' (parseExp (lamAnd ++ lamFls ++ lamTru))
  test "3: evalb" evalb (parseExp (lamAnd ++ lamTru ++ lamTru))
  test "3: evals" evals' (parseExp (lamAnd ++ lamTru ++ lamTru))
  test "4: evalb" evalb (parseExp (lamOr ++ lamFls ++ lamTru))
  test "4: evals" evals' (parseExp (lamOr ++ lamFls ++ lamTru))
  test "5: evalb" evalb (parseExp (lamOr ++ lamFls ++ lamFls))
  test "5: evals" evals' (parseExp (lamOr ++ lamFls ++ lamFls))
  test "6: evalb" evalb (parseExp (lamNot ++ lamFls))
  test "6: evals" evals' (parseExp (lamNot ++ lamFls))
  test "7: evalb" evalb (parseExp (lamNot ++ lamTru))
  test "7: evals" evals' (parseExp (lamNot ++ lamTru))
  test "8: evalb" evalb (parseExp (lamDangle))
  test "8: evals" evals' (parseExp (lamDangle))
  test "9: evalb" evalb (parseExp (lamDangle ++ lamIdent))
  test "9: evals" evals' (parseExp (lamDangle ++ lamIdent))
  test "10: evalb" evalb (parseExp (justVar))
  test "10: evals" evals' (parseExp (justVar))
  test "11: evalb" evalb (parseExp (justVarEval))
  test "11: evals" evals' (parseExp (justVarEval))
  test "12: evalb" evalb (parseExp (churchSum ++ churchOne ++ churchTwo))
  test "12: evals" evals' (parseExp (churchSum ++ churchOne ++ churchTwo))
  test "13: evalb" evalb (parseExp (churchTimes ++ churchOne ++ churchTwo))
  test "13: evals" evals' (parseExp (churchTimes ++ churchOne ++ churchTwo))
  test "14: evalb" evalb (parseExp (churchIsZero ++ churchZero))
  test "14: evals" evals' (parseExp (churchIsZero ++ churchZero))
  test "15: evalb" evalb (parseExp (churchIsZero ++ churchOne))
  test "15: evals" evals' (parseExp (churchIsZero ++ churchOne))
  test "16: evalb" evalb (parseExp (churchIsZero ++ "(" ++ churchTimes ++ churchTwo ++ churchZero ++ ")"))
  test "16: evals" evals' (parseExp (churchIsZero ++ "(" ++ churchTimes ++ churchTwo ++ churchZero ++ ")"))
  test "17: evalb" evalb (parseExp (lamTest ++ lamTru ++ churchZero ++ churchOne))
  test "17: evals" evals' (parseExp (lamTest ++ lamTru ++ churchZero ++ churchOne))
  test "18: evalb" evalb (parseExp (lamTest ++ lamFls ++ churchZero ++ churchOne))
  test "18: evals" evals' (parseExp (lamTest ++ lamFls ++ churchZero ++ churchOne))
  test "19: evalb" evalb (parseExp (churchTwo ++ lamNot ++ lamFls))
  test "19: evals" evals' (parseExp (churchTwo ++ lamNot ++ lamFls))
  test "20: evalb" evalb (parseExp (lamFst ++ mixedPair))
  test "20: evals" evals' (parseExp (lamFst ++ mixedPair))
  test "21: evalb" evalb (parseExp (lamSnd ++ mixedPair))
  test "21: evals" evals' (parseExp (lamSnd ++ mixedPair))
  test "22: evalb" evalb (parseExp outOfOrder)
  test "22: evals" evals' (parseExp outOfOrder)
  test "23: evalb" evalb (parseExp churchOutOfOrder)
  test "23: evals" evals' (parseExp churchOutOfOrder)
  test "24: evalb" evalb (parseExp capture)
  test "24: evals" evals' (parseExp capture)
  test "25: evalb" evalb (parseExp nestedCapture)
  test "25: evals" evals' (parseExp nestedCapture)

  test "1: evalsingle" evals (parseExp (lamTru ++ " x y"))
  test "2: evalsingle" evals (parseExp (lamAnd ++ lamFls ++ lamTru))
  test "3: evalsingle" evals (parseExp (lamAnd ++ lamTru ++ lamTru))
  test "4: evalsingle" evals (parseExp (lamOr ++ lamFls ++ lamTru))
  test "5: evalsingle" evals (parseExp (lamOr ++ lamFls ++ lamFls))
  test "6: evalsingle" evals (parseExp (lamNot ++ lamFls))
  test "7: evalsingle" evals (parseExp (lamNot ++ lamTru))
  test "8: evalsingle" evals (parseExp (lamDangle))
  test "9: evalsingle" evals (parseExp (lamDangle ++ lamIdent))
  test "10: evalsingle" evals (parseExp (justVar))
  test "11: evalsingle" evals (parseExp (justVarEval))
  test "12: evalsingle" evals (parseExp (churchSum ++ churchOne ++ churchTwo))
  test "13: evalsingle" evals (parseExp (churchTimes ++ churchOne ++ churchTwo))
  test "14: evalsingle" evals (parseExp (churchIsZero ++ churchZero))
  test "15: evalsingle" evals (parseExp (churchIsZero ++ churchOne))
  test "16: evalsingle" evals (parseExp (churchIsZero ++ "(" ++ churchTimes ++ churchTwo ++ churchZero ++ ")"))
  test "17: evalsingle" evals (parseExp (lamTest ++ lamTru ++ churchZero ++ churchOne))
  test "18: evalsingle" evals (parseExp (lamTest ++ lamFls ++ churchZero ++ churchOne))
  test "19: evalsingle" evals (parseExp (churchTwo ++ lamNot ++ lamFls))
  test "20: evalsingle" evals (parseExp (lamFst ++ mixedPair))
  test "21: evalsingle" evals (parseExp (lamSnd ++ mixedPair))
  test "22: evalsingle" evals (parseExp outOfOrder)
  test "23: evalsingle" evals (parseExp churchOutOfOrder)
  test "24: evalsingle" evals (parseExp capture)
  test "25: evalsingle" evals (parseExp nestedCapture)

main = do
  tagged "Lambda" lambda
  
  
