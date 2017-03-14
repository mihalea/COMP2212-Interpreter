exception UnboundError ;;

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
let addBinding env str thing = match env with
      Env(gs) -> Env ( (str, thing) :: gs ) ;;


let splitInput line =
  let delim = Str.regexp "[{},]" in
    let split = Str.split(delim) in
      List.map String.trim (split line)
;;

let rec print_list = function
[] -> ()
| e::l -> print_string e ; print_string " " ; print_list l
;;

let parseLine env line lineCount =
  print_endline line;
  addBinding env ("K" ^ (string_of_int lineCount)) (SS.of_list (splitInput line));
;;

let rec readInput env lineCount =
  try
    let line = input_line stdin in
    if (Str.string_match (Str.regexp "^[0-9]+$") line 0) then
      env
    else
      readInput (parseLine env line lineCount) (lineCount + 1);
  with
  End_of_file -> env;
;;

let run =
  let env = readInput (Env []) 0 in
    SS.iter print_endline (lookup env "K0");
;;
