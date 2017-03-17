type setVar =
| SetRaw of Set.Make(String).t
| SetIdent of string
;;

type stringVar =
| StringRaw of string
| StringIdent of string
;;

type intVar =
| IntRaw of int
| IntIdent of string
;;

type boolVar =
| BoolRaw of bool
| BoolIdent of string
;;

type generic =
| GenInt of int
| GenStr of string
| GenSet of Set.Make(String).t
| GenBln of bool
;;

type vars =
| SetVar of setVar
| StringVar of stringVar
| IntVar of intVar
| BoolVar of boolVar
;;

type binding =
| SetBinding of string * setVar
| StringBinding of string * stringVar
| IntBinding of string * intVar
| BoolBinding of string * boolVar

type setOperation =
| Set of setVar
| SetAddition of stringVar * setOperation
| SetKleene of setOperation * int 
| SetUnion of setOperation * setOperation
| SetIntersection of setOperation * setOperation
| SetCartesian of setOperation * setOperation
| SetSubtraction of setOperation * setOperation
;;

type integerOperation =
| Integer of intVar
| Plus of integerOperation * integerOperation
| Minus of integerOperation * integerOperation
| Times of integerOperation * integerOperation
| Div of integerOperation * integerOperation
| Mod of integerOperation * integerOperation
| SetLength of setOperation
;;

type stringOperation =
| String of stringVar
| StringConcat of stringOperation * stringOperation
;;

type booleanOperation =
| Boolean of boolVar
| BooleanLessThan of integerOperation * integerOperation
| BooleanGreaterThan of integerOperation * integerOperation
| BooleanLessEqualThan of integerOperation * integerOperation
| BooleanGreaterEqualThan of integerOperation * integerOperation
| BooleanEqual of integerOperation * integerOperation
| BooleanNotEqual of integerOperation * integerOperation
| SetSubset of setOperation * setOperation
| SetBelong of stringVar * setOperation
;;

type operation =
| SetOperation of setOperation
| StringOperation of stringOperation
| IntegerOperation of integerOperation
| BooleanOperation of booleanOperation
;;


type declaration =
| SetDeclaration of string * setOperation
| StringDeclaration of string * stringOperation
| IntegerDeclaration of string * integerOperation
| BooleanDeclaration of string * booleanOperation
;;

type mutation =
| SetMutation of string * setOperation
| StringMutation of string * stringOperation
| IntegerMutation of string * integerOperation
| BooleanMutation of string * booleanOperation
;;

type exec =
| Operation of operation
| Declaration of declaration
| Mutation of mutation
| Print of operation
;;

type body =
| SingleStatement of statement 
| MultiStatement of statement * body
and
statement =
| IfStatement of ifElse
| ForStatement of forEach
| ExecStatement of exec
and
ifElse =
| If of booleanOperation * body
| IfElse of booleanOperation * body * body
and
forEach =
| ForEach of string * string * body
;;


type mainTree =
| Body of body
;;
