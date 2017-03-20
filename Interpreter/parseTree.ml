type tTerm = 
    | TermVar of string
    | TermInteger of int
    | TermString of string
    | TermNull
    | TermPlus of tTerm * tTerm 
    | TermSet of Set.Make(String).t
    | MultiStatement of tTerm * tTerm
    | IntDeclaration of tTerm * tTerm 
    | StrDeclaration of tTerm * tTerm
    | PrintOperation of tTerm 
    | ForOperation of tTerm * tTerm * tTerm
    | TermConcat of tTerm * tTerm
    | TermMut of tTerm * tTerm
;;
