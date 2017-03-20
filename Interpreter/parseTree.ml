type tTerm = 
    | TermInteger of int
    | TermString of string
    | TermPlus of tTerm * tTerm 
    | TermSet of Set.Make(String).t
    | MultiStatement of tTerm * tTerm
    | IntDeclaration of string * tTerm 
    | PrintOperation of string 
    | ForOperation of string * string * tTerm
    | TermConcat of tTerm * tTerm
;;
