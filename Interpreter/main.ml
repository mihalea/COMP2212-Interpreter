exception Eof;;
exception Terminated;;
exception UnboundError;;
exception StuckTerm;;
exception Not_a_set;;
exception Erroneous_String_Concat;;

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

let rec remove_binding env str = match env with
    Env [] -> raise UnboundError
   |Env ((name, thing) :: gs) ->
           (
               match (name = str) with
               true -> Env(gs)
               |false -> let env' = (remove_binding (Env (gs)) str) in
                    match env' with
                        | Env (gs') -> Env ((name,thing) :: gs')
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

let rec print_generic env var = match var with
  | TermVar x -> print_generic env (lookup env x)
  | TermInteger x -> print_endline (string_of_int x)
  | TermSet x -> (print_string "{";
                    SS.iter (fun (elem:SS.elt) -> print_string elem;print_string ",") x;
                    print_endline "}")
  | TermString x -> print_endline x
(*| TermBoolean x -> if x then print_endline "true" else print_endline "false" *)
| _ -> print_endline "Blabla" 
;;

let rec eval env e = match e with
  | (MultiStatement (e1, e2)) ->let (e, env') = (eval env e1) in eval env' e2

  | (TermInteger x) -> (e, env)
  | (TermString x) ->(e, env) 
  | (TermVar x) -> ((lookup env x), env)
  | (TermArgs args) -> (TermSet(SS.of_list args), env)

  | (Declaration(TermVar(k), v)) when (isValue v) -> (TermNull, addBinding env (k, v))
  | (Declaration(TermVar(k), v)) -> (
      let (v', env') = (eval env v) in 
        (TermNull, addBinding env (k, v'))
    )

  | (TermMut(TermVar(ident), action)) when (isValue action) -> (
      try 
          let env' = (remove_binding env ident) in
            (TermNull, addBinding env' (ident, action))
        with UnboundError -> failwith ("Variable " ^ ident ^ " not declared.")
        )
  | (TermMut(TermVar(ident), action)) -> (
      try
          let env' = (remove_binding env ident) in
            let (e', env'') = (eval env action) in
                (TermNull, addBinding env' (ident, e'))
        with UnboundError -> failwith ("Variable " ^ ident ^ " not declared.")
    )

  | (TermConcat (TermString(t1), TermString(t2))) ->(TermString(t1^t2), env) 
  | (TermConcat (TermString(t1), e2)) -> ( 
      let (e2', env') = (eval env e2) in
        match e2' with 
              (TermString (s)) -> ((TermString(t1^s)),env')
            | _ -> raise Erroneous_String_Concat
  )
  | (TermConcat (e1, e2)) -> (
      let (e1', env') = (eval env e1) in
        let (e2', env'') = (eval env' e2) in
            match (e1', e2') with
                  (TermString(s1), TermString(s2)) -> (TermString (s1 ^ s2),env)
                | _ -> raise Erroneous_String_Concat
  )

  | (PrintOperation x) when (isValue x) -> print_generic env x;(TermNull, env)
  | (PrintOperation x) -> let (e', env') =  (eval env x) in print_generic env' e';(TermNull, env')

  | (ForOperation (TermVar(elem), TermVar(iter), body)) -> (
    try
      lookup env elem;
      failwith("Variable already in use " ^ elem)
    with UnboundError -> (
        try
            let env' = env in (
                match (lookup env' iter) with 
                      TermSet set -> (
                          let rec iterate set_iter = 
                            if (SS.is_empty set_iter) then
                                (TermNull, env)
                            else 
                                let chosen = SS.choose set_iter in (
                                    eval (addBinding env' (elem, TermString(chosen))) body;
                                    iterate (SS.remove chosen set_iter)
                                )
                          in
                          iterate set;
                      )
                    | _ -> raise Not_a_set
            )
        with UnboundError -> failwith(iter ^ " not found.")
        )
    )
  (*| (TermPlus(TermInteger(n), TermInteger(m))) -> (TermInteger(n+m), env)*) 
  | _ -> raise Terminated
;;

(* let rec evalloop env e = try (let (e',env') = (eval env e) in (evalloop env' e')) with Terminated -> if (isValue e) then e else raise StuckTerm  ;; *)


let run =
  let env = readInput (Env []) 0 in
    try
      let lexbuf = Lexing.from_channel (open_in Sys.argv.(1)) in
        let token_stream = (Parser.start Lexer.next lexbuf) in
          eval env token_stream
    with Eof ->
        exit 0
