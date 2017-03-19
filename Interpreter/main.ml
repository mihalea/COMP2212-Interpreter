exception Eof;;
exception Terminated;;
exception UnboundError;;
exception StuckTerm;;

open ParseTree;;

module SS = Set.Make(String)

(* ENVIRONMENTS *)
type 'a context = Env of (string * 'a) list


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


let rec isValue e = match e with
  | TermInteger(n) -> true
  | _ -> false
;;

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
      addBinding env ("K", TermInteger(int_of_string line))
    else
      readInput (addBinding env (("INPUT" ^ (string_of_int lineCount)), TermSet(SS.of_list (splitInput line)))) (lineCount + 1)
  with
  End_of_file -> env;
;;

let print_generic env var = match (lookup env var) with
  | TermInteger x -> print_endline (string_of_int x)
  | TermSet x -> SS.iter print_endline x
(* | TermString x -> print_endline x
| TermBoolean x -> if x then print_endline "true" else print_endline "false" *)
| _ -> ()
;;

let rec eval env e = match e with
  | (MultiStatement (e1, e2)) -> let env' = (eval env e1) in eval env' e2
  | (TermInteger x) -> env
  | (IntDeclaration(k, v)) -> addBinding env (k, v)
  | (PrintOperation x) -> print_generic env x; env
  (* | (TermPlus(TermInteger(n), TermInteger(m)) -> (TermInteger(n+m), env) *)
  | _ -> raise Terminated;;

(* let rec evalloop env e = try (let (e',env') = (eval env e) in (evalloop env' e')) with Terminated -> if (isValue e) then e else raise StuckTerm  ;; *)


let run =
  let env = readInput (Env []) 0 in
    try
      let lexbuf = Lexing.from_channel (open_in Sys.argv.(1)) in
        let token_stream = (Parser.start Lexer.next lexbuf) in
          eval env token_stream
    with Eof ->
        exit 0
