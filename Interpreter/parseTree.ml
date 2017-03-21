type tTerm =
    | TermNull
    | TermVar of string
    | TermInteger of int
    | TermString of string
    | TermBool of bool

    | TermSet of Set.Make(String).t
    | TermArgs of string list

    | MultiStatement of tTerm * tTerm

    | Declaration of tTerm * tTerm

    | PrintOperation of tTerm
    | ForOperation of tTerm * tTerm * tTerm
    | ForLoop of tTerm * tTerm * tTerm
    | IfStatement of tTerm * tTerm 
    | IfElseStatement of tTerm * tTerm * tTerm

    | TermConcat of tTerm * tTerm
    | TermMut of tTerm * tTerm
    | TermUnion of tTerm * tTerm
    | TermIntersection of tTerm * tTerm
    | TermDifference of tTerm * tTerm
    | TermAdd of tTerm * tTerm

    | TermPlus of tTerm * tTerm
    | TermMinus of tTerm * tTerm
    | TermMult of tTerm * tTerm
    | TermDiv of tTerm * tTerm
    | TermMod of tTerm * tTerm

    | TermNot of tTerm

    | TermLt of tTerm * tTerm
    | TermLte of tTerm * tTerm
    | TermGt of tTerm * tTerm
    | TermGte of tTerm * tTerm
    | TermEq of tTerm * tTerm
    | TermNeq of tTerm * tTerm
;;
