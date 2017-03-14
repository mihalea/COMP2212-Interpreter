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

type integerOperation =
| Integer of intVar
| IntegerAddition of integerOperation * integerOperation
| IntegerSubtraction of integerOperation * integerOperation
| IntegerMultiplication of integerOperation * integerOperation
| IntegerDivision of integerOperation * integerOperation
| IntegerModulo of integerOperation * integerOperation
;;

type setOperation =
| Set of setVar
| SetLength of setOperation
| SetBelong of stringVar * setOperation
| SetAddition of stringVar * setOperation
| SetSubset of setOperation * setOperation
| SetKleene of setOperation * integerOperation
| SetUnion of setOperation * setOperation
| SetIntersection of setOperation * setOperation
| SetCartesian of setOperation * setOperation
| SetSubtraction of setOperation * setOperation
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
| SingleStatement of body
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
