exception UnboundError ;;

module SS = Set.Make(String)

(* ENVIRONMENTS *)
type 'a binding = {key: string; value: 'a};;
let environment = ref []

let addBinding k v =
  environment := {key=k; value=v} :: !environment
;;

let lookupVar e =
  let rec aux env =
  match !environment with
    [] -> raise UnboundError
    | {key=k; value=v}::t ->
    (
      match (e = k) with
        true -> v
        |false -> aux t
      )
  in
  aux !environment
;;


let splitInput line =
  let delim = Str.regexp "[{},]" in
    let split = Str.split(delim) in
      List.map String.trim (split line)
;;

let rec print_list = function
[] -> ()
| e::l -> print_string e ; print_string " " ; print_list l

let parseLine line lineCount =
  (* addBinding env "K" (SS.of_list (splitInput line)) *)
  print_endline line;
;;

let readInput =
  try
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
            parseLine line lineCount;
            (* SS.iter print_endline (lookup env "K"); *)
            lineCount := !lineCount + 1;
            )
      done;
      None
  with
    End_of_file -> None
;;

let run =
  readInput;;
