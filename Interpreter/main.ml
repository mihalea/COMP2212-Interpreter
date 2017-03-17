exception UnboundError ;;
exception NotAnInt ;;
exception NotStringType ;;
exception NotBooleanType ;;
exception NotASet ;;

open ParseTree;;

module SS = Set.Make(String)

(* ENVIRONMENTS *)
type 'a context = Env of (string * generic) list

(* Function to look up the type of a string name variable in a type environment *)
let rec lookup env str = match env with
   Env [] -> raise UnboundError
  |Env ((name,thing) :: gs) ->
        (
          match (name = str) with
            true -> thing
           |false -> lookup (Env (gs)) str
	)
;;

(* Function to add an extra entry in to an environment *)
let addBinding env binding = match env with
      Env(gs) -> Env ( binding :: gs ) ;;


let splitInput line =
  let delim = Str.regexp "[{},]" in
    let split = Str.split(delim) in
      List.map String.trim (split line)
;;

let rec print_list = function
[] -> ()
| e::l -> print_string e ; print_string " " ; print_list l
;;


let rec readInput env lineCount =
  try
    let line = input_line stdin in
    if (Str.string_match (Str.regexp "^[0-9]+$") line 0) then
      addBinding env ("K",GenInt(int_of_string line))
    else
      readInput (addBinding env (("INPUT" ^ (string_of_int lineCount)), GenSet(SS.of_list (splitInput line)))) (lineCount + 1);
  with
  End_of_file -> env;
;;

let print_generic gen = match gen with
| GenStr x -> print_endline x
| GenInt x -> print_endline (string_of_int x)
| GenSet x -> SS.iter print_endline x
| GenBln x -> if x then print_endline "true" else print_endline "false" 
;;

let lookupIntVar env iv = match iv with
    | IntRaw i -> i
    | IntIdent i -> try (match (lookup env i) with GenInt x -> x | _ -> raise NotAnInt) 
            with Not_found -> failwith ("Variable "^i^" not defined")
;;

let lookupStrVar env tokens = match tokens with
    | StringRaw str -> str
    | StringIdent str -> try (match (lookup env str) with GenStr x -> x | _ -> raise NotStringType)
            with Not_found -> failwith ("Variable "^str^" not defined")
;;

let lookupBoolVar env tokens = match tokens with
    | BoolRaw bln -> bln
    | BoolIdent bln -> try (match (lookup env bln) with GenBln x -> x | _ -> raise NotBooleanType)
            with Not_found -> failwith ("Variable "^bln^" not defined")
;;

let lookupSetVar env tokens = match tokens with
    | SetRaw set -> set
    | SetIdent set -> try (match (lookup env set) with GenSet x -> x | _ -> raise NotASet)
            with Not_found -> failwith ("Variable"^set^"not found")
;;

let rec processStrOp env tokens = match tokens with
    | String str -> lookupStrVar env str
    | StringConcat (str1, str2) -> (processStrOp env str1) ^ (processStrOp env str2) 
;;

let rec processSetOp env tokens = match tokens with
    | Set set -> lookupSetVar env set
    | SetAddition (elem,set) -> SS.add (lookupStrVar env elem) (processSetOp env set)
    | SetKleene (set, num) -> SS.of_list []
    | SetUnion (set1, set2) -> SS.union (processSetOp env set1) (processSetOp env set2)
    | SetIntersection (set1, set2) -> SS.inter (processSetOp env set1) (processSetOp env set2)
    | SetCartesian (set1, set2) -> SS.of_list (SS.fold (fun (x:SS.elt) -> (SS.iter (fun (y:SS.elt) -> (x,y)) (processSetOp env set2))) (processSetOp env set1) [])
    | SetSubtraction (set1, set2) -> SS.diff (processSetOp env set1) (processSetOp env set2)
;;

let processBoolOperation env tokens = match tokens with
    | Boolean booln -> lookupBoolVar env booln
    | SetSubset (set1, set2) -> SS.subset set1 set2
    | SetBelong (elem, set) -> SS.mem elem set
    | _ -> ()
;;

let processOperation env tokens = match tokens with
    | IntegerOperation intOp -> () 
    | StringOperation strOp -> processStrOp env strOp
    | SetOperation setOp -> processSetOp env setOp
    | BooleanOperation boolOp -> processBoolOperation env boolOp
;;

let processExec env tokens = match tokens with 
    | Operation op -> processOperation env op
    | Declaration dec -> processDeclaration env dec
    | Mutation mut -> processMutation env mut
    | Print pr -> processPrint env print
;;

let processTokens env tokens = match tokens with
    | Body body -> processBody env tokens
and processBody env tokens = match tokens with
    | SingleStatement s -> processStatement env s
    | MultiStatement (s, b) -> processStatement env s; processBody env b
and processStatement env tokens = match tokens with
    | IfStatement s -> processIf env tokens
    | ForStatement s -> processFor env tokens
    | ExecStatement s -> processExec env tokens
and processIf env tokens = match tokens with
    | If (bool, body) -> if (processBoolExec env bool) then (processBody env body)
    | IfElse (bool, body, elBody) -> if (processBoolExec env bool) then (processBody env body) else (processBody env elBody)
and processFor env tokens = match tokens with
    | ForEach (elem, coll, body) ->
            (
                try
                    lookup env elem
                    failwith ("Variable already in use: " ^elem^"")
                with Not_found -> 
                    (
                        try
                            SS.iter (fun (x : SS.elt) -> processBody (addBinding env (elem, lookup env x)) body) (lookup env coll)
                        with Not_found -> failwith ("No collection named "^coll^" found.")))
;;

let run =
  let env = readInput (Env []) 0 in
    try 
      let lexbuf = Lexing.from_channel (open_in Sys.argv.(1)) in
        let token_stream = (Parser.main Lexer.token lexbuf) in
          processTokens env token_stream        
    with Lexer.Eof ->
        exit 0
;;
