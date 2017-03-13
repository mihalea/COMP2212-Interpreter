exception UnboundError ;;

module SS = Set.Make(String)

(* ENVIRONMENTS *)
type 'a ref = Env of (string * 'a) list;;

let varBind = Env []


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
;;

let splitInput line =
  let delim = Str.regexp "[{},]" in
    let split = Str.split(delim) in
      List.map String.trim (split line)
;;

let rec print_list = function
[] -> ()
| e::l -> print_string e ; print_string " " ; print_list l

let parseLine env line lineCount =
  addBinding env "K" (SS.of_list (splitInput line))
;;

let readInput =
  try
    let env = (Env []) in
    let lineCount = ref 0 in
      while true do
        let line = input_line stdin in
          if (Str.string_match (Str.regexp "^[0-9]+$") line 0) then
          (
            print_string "LINES: ";
            print_int !lineCount;
            )
          else
          (
            parseLine env line lineCount;
            SS.iter print_endline (lookup env "K");
            lineCount := !lineCount + 1;
            )
      done;
      None
  with
    End_of_file -> None
;;

let run =
  readInput;;
