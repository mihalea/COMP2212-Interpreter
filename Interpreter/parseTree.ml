type tTerm = 
    | TermVar of string
    | TermInteger of int
    | TermString of string
    | TermNull
    | TermPlus of tTerm * tTerm 
    | TermSet of Set.Make(String).t
    | TermArgs of string list

    | MultiStatement of tTerm * tTerm

    | Declaration of tTerm * tTerm

    | PrintOperation of tTerm 

    | ForOperation of tTerm * tTerm * tTerm

    | TermConcat of tTerm * tTerm
    | TermMut of tTerm * tTerm
    | TermUnion of tTerm * tTerm
    | TermIntersection of tTerm * tTerm
    | TermDifference of tTerm * tTerm
;;
