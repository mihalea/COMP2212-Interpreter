exception Eof;;
exception Terminated;;
exception UnboundError;;
exception StuckTerm;;
exception Not_a_set;;
exception Illegal_operation;;

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
  | TermString(n) -> true
  | _ -> false
;;

let splitInput line =
  let delim = Str.regexp "[{},]" in
    let split = Str.split(delim) in (
      List.filter (fun e -> (String.length e) > 0) (List.map String.trim (split line));
      )
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
    else (
      readInput (addBinding env (("INPUT" ^ (string_of_int lineCount)), TermSet(SS.of_list (splitInput line)))) (lineCount + 1)
    )
  with
  End_of_file -> env;
;;

let rec print_generic env var = match var with
  | TermVar x -> print_generic env (lookup env x)
  | TermInteger x -> print_endline (string_of_int x)
  | TermSet x -> (print_string "{";
                    let length = (SS.cardinal x) in
                      let k = (lookup env "K") in (
                        match k with
                          (TermInteger kint) ->
                            (List.iteri (fun i e ->  (
                              if (i < kint) then (
                                print_string e;
                                if ((i < (length - 1)) && (i < (kint - 1))) then
                                  print_string ", ";
                              )

                            )) (SS.elements x)
                            )
                          | _ -> failwith "Unbound K"
                      );
                    print_endline "}")
  | TermString x -> print_endline x
  | TermBool x -> if x then print_endline "true" else print_endline "false" 
| _ -> print_endline "Blabla"
;;

let rec eval env e = match e with
  | (MultiStatement (e1, e2)) ->let (e, env') = (eval env e1) in eval env' e2

  | (TermInteger x) -> (e, env)
  | (TermString x) ->(e, env)
  | (TermBool x) -> (e, env)
  | (TermSet x) -> (e, env)
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

  | (TermAdd(TermVar(var), action)) when (isValue action)-> (
      try
        let os = lookup env var in
          match (os, action) with
          | (TermSet(s), TermString(str)) ->  (TermSet(SS.add str s), env)
          | _ -> raise Illegal_operation
      with UnboundError -> failwith ("Variable " ^ var ^ " not declared.")
    )

  | (TermAdd(TermVar(var), action)) -> (
    try
      let os = lookup env var in
        let (e', env') = eval env action in
          match (os, e') with
            | (TermSet(s), TermString(str)) ->  (TermSet(SS.add str s), env')
            | _ -> raise Illegal_operation
    with UnboundError -> failwith ("Variable " ^ var ^ "is not declared.")
  )

  | (TermConcat (TermString(t1), TermString(t2))) ->(
      match (t1, t2) with
        | (":",_) -> (TermString(t2), env)
        | (_,":") -> (TermString(t1), env)
        | (_,_) -> (TermString(t1^t2), env)
    )
  | (TermConcat (TermString(t1), e2)) -> (
      let (e2', env') = (eval env e2) in
        match e2' with
              (TermString (s)) ->(
                  match (t1, s) with
                | (":",_) -> (TermString(s), env)
                | (_,":") -> (TermString(t1), env)
                | (_,_) -> (TermString(t1^s), env)
              )
            | _ -> raise Illegal_operation
  )
  | (TermConcat (e1, e2)) -> (
      let (e1', env') = (eval env e1) in
        let (e2', env'') = (eval env' e2) in
            match (e1', e2') with
                  (TermString(t1), TermString(t2)) -> (
                    match (t1, t2) with
                        | (":",_) -> (TermString(t2), env)
                        | (_,":") -> (TermString(t1), env)
                        | (_,_) -> (TermString(t1^t2), env)
                  )
                | _ -> raise Illegal_operation
  )

  | (TermUnion (TermVar(s1), TermVar(s2))) -> (
        let set1 = (lookup env s1) in
            let set2 =  (lookup env s2) in
                match (set1, set2) with
                    | (TermSet (set1'), TermSet (set2')) -> (TermSet(SS.union set1' set2'), env)
                    | _ -> raise Illegal_operation
  )
  | (TermUnion (TermVar(s1), e2)) -> (
        let (e', env') = (eval env e2) in
            let res = (lookup env s1) in
            match (res, e') with
                | (TermSet(res'),TermSet(e'')) -> (TermSet(SS.union res' e''), env')
                | _ -> raise Illegal_operation
    )
  | (TermUnion(e1,e2)) -> (
        let (e1', env') = (eval env e1) in
            let (e2', env'') = (eval env' e2) in
                match (e1', e2') with
                    | (TermSet(e1''), TermSet(e2'')) -> (TermSet (SS.union e1'' e2''), env)
                    | _ -> raise Illegal_operation
    )

  | (TermIntersection (TermVar(s1), TermVar(s2))) -> (
        let set1 = (lookup env s1) in
            let set2 =  (lookup env s2) in
                match (set1, set2) with
                    | (TermSet (set1'), TermSet (set2')) -> (TermSet(SS.inter set1' set2'), env)
                    | _ -> raise Illegal_operation
  )
  | (TermIntersection (TermVar(s1), e2)) -> (
        let (e', env') = (eval env e2) in
            let res = (lookup env s1) in
            match (res, e') with
                | (TermSet(res'),TermSet(e'')) -> (TermSet(SS.inter res' e''), env')
                | _ -> raise Illegal_operation
    )
  | (TermIntersection(e1,e2)) -> (
        let (e1', env') = (eval env e1) in
            let (e2', env'') = (eval env' e2) in
                match (e1', e2') with
                    | (TermSet(e1''), TermSet(e2'')) -> (TermSet (SS.inter e1'' e2''), env)
                    | _ -> raise Illegal_operation
    )

  | (TermDifference (TermVar(s1), TermVar(s2))) -> (
        let set1 = (lookup env s1) in
            let set2 =  (lookup env s2) in
                match (set1, set2) with
                    | (TermSet (set1'), TermSet (set2')) -> (TermSet(SS.diff set1' set2'), env)
                    | _ -> raise Illegal_operation
  )
  | (TermDifference (TermVar(s1), e2)) -> (
        let (e', env') = (eval env e2) in
            let res = (lookup env s1) in
            match (res, e') with
                | (TermSet(res'),TermSet(e'')) -> (TermSet(SS.diff res' e''), env')
                | _ -> raise Illegal_operation
    )
  | (TermDifference(e1,e2)) -> (
        let (e1', env') = (eval env e1) in
            let (e2', env'') = (eval env' e2) in
                match (e1', e2') with
                    | (TermSet(e1''), TermSet(e2'')) -> (TermSet (SS.diff e1'' e2''), env)
                    | _ -> raise Illegal_operation
    )
  | (TermPlus(TermInteger(n), TermInteger(m))) -> (TermInteger(n+m), env)
  | (TermPlus(TermInteger(n), e2)) -> (
        let (e2', env') = (eval env e2) in
            match e2' with
                | (TermInteger(m)) -> (TermInteger(n+m),env)
                | _ -> raise Illegal_operation
  )
  | (TermPlus(e1, e2)) -> (
      let (e1', env') = (eval env e1) in
        let (e2', env'') = (eval env' e2) in
            match (e1',e2') with
                | (TermInteger(n),TermInteger(m)) -> (TermInteger(n+m),env)
                | _ -> raise Illegal_operation
  )
  | (TermMinus(TermInteger(n), TermInteger(m))) -> (TermInteger(n-m), env)
  | (TermMinus(TermInteger(n), e2)) -> (
        let (e2', env') = (eval env e2) in
            match e2' with
                | (TermInteger(m)) -> (TermInteger(n-m),env)
                | _ -> raise Illegal_operation
  )
  | (TermMinus(e1, e2)) -> (
      let (e1', env') = (eval env e1) in
        let (e2', env'') = (eval env' e2) in
            match (e1',e2') with
                | (TermInteger(n),TermInteger(m)) -> (TermInteger(n-m),env)
                | _ -> raise Illegal_operation
  )
  | (TermMult(TermInteger(n), TermInteger(m))) -> (TermInteger(n*m), env)
  | (TermMult(TermInteger(n), e2)) -> (
        let (e2', env') = (eval env e2) in
            match e2' with
                | (TermInteger(m)) -> (TermInteger(n*m),env)
                | _ -> raise Illegal_operation
  )
  | (TermMult(e1, e2)) -> (
      let (e1', env') = (eval env e1) in
        let (e2', env'') = (eval env' e2) in
            match (e1',e2') with
                | (TermInteger(n),TermInteger(m)) -> (TermInteger(n*m),env)
                | _ -> raise Illegal_operation
  )
  | (TermDiv(TermInteger(n), TermInteger(m))) -> (
      try 
          (TermInteger(n/m), env)
        with Division_by_zero -> failwith ("DIV by zero")
     )
  | (TermDiv(TermInteger(n), e2)) -> (
        let (e2', env') = (eval env e2) in
            match e2' with
                | (TermInteger(m)) -> (
                    try 
                        TermInteger(n/m),env
                    with Division_by_zero -> failwith ("DIV by zero")
                    )
                | _ -> raise Illegal_operation
  )
  | (TermDiv(e1, e2)) -> (
      let (e1', env') = (eval env e1) in
        let (e2', env'') = (eval env' e2) in
            match (e1',e2') with
                | (TermInteger(n),TermInteger(m)) -> (
                    try
                        TermInteger(n/m),env
                    with Division_by_zero -> failwith ("DIV by zero") 
                    )
                | _ -> raise Illegal_operation
  )
  | (TermMod(TermInteger(n), TermInteger(m))) -> (TermInteger(n mod m), env)
  | (TermMod(TermInteger(n), e2)) -> (
        let (e2', env') = (eval env e2) in
            match e2' with
                | (TermInteger(m)) -> (TermInteger(n mod m),env)
                | _ -> raise Illegal_operation
  )
  | (TermMod(e1, e2)) -> (
      let (e1', env') = (eval env e1) in
        let (e2', env'') = (eval env' e2) in
            match (e1',e2') with
                | (TermInteger(n),TermInteger(m)) -> (TermInteger(n mod m),env)
                | _ -> raise Illegal_operation
  )
  |(TermLt (TermVar(v1), TermVar(v2))) -> (
      let val1 = lookup env v1 in
        let val2 = lookup env v2 in
            match (val1, val2) with
                (TermInteger(int1), TermInteger(int2)) -> (
                    if int1 < int2 then
                        (TermBool(true),env)
                    else
                        (TermBool(false),env)
                )
                | _ -> raise Illegal_operation
  )
  |(TermLt (TermVar(v1), e2)) -> (
      let (val2, env') = (eval env e2) in
        let val1 = lookup env v1 in
            match (val1, val2) with
                (TermInteger(int1), TermInteger(int2)) -> (
                    if int1 < int2 then
                        (TermBool(true),env)
                    else
                        (TermBool(false),env)
                )
                | _ -> raise Illegal_operation
  )
  |(TermLt (e1,e2)) -> (
      let (val1,env') = (eval env e1) in
        let (val2, env'') = (eval env' e2) in
            match (val1, val2) with
                (TermInteger(int1), TermInteger(int2)) -> (
                    if int1 < int2 then
                        (TermBool(true),env)
                    else
                        (TermBool(false),env)
                )
                | _ -> raise Illegal_operation
  )
  |(TermLte (TermVar(v1), TermVar(v2))) -> (
      let val1 = lookup env v1 in
        let val2 = lookup env v2 in
            match (val1, val2) with
                (TermInteger(int1), TermInteger(int2)) -> (
                    if int1 <= int2 then
                        (TermBool(true),env)
                    else
                        (TermBool(false),env)
                )
                | _ -> raise Illegal_operation
  )
  |(TermLte (TermVar(v1), e2)) -> (
      let (val2, env') = (eval env e2) in
        let val1 = lookup env v1 in
            match (val1, val2) with
                (TermInteger(int1), TermInteger(int2)) -> (
                    if int1 <= int2 then
                        (TermBool(true),env)
                    else
                        (TermBool(false),env)
                )
                | _ -> raise Illegal_operation
  )
  |(TermLte (e1,e2)) -> (
      let (val1,env') = (eval env e1) in
        let (val2, env'') = (eval env' e2) in
            match (val1, val2) with
                (TermInteger(int1), TermInteger(int2)) -> (
                    if int1 <= int2 then
                        (TermBool(true),env)
                    else
                        (TermBool(false),env)
                )
                | _ -> raise Illegal_operation
  )
  |(TermGt (TermVar(v1), TermVar(v2))) -> (
      let val1 = lookup env v1 in
        let val2 = lookup env v2 in
            match (val1, val2) with
                (TermInteger(int1), TermInteger(int2)) -> (
                    if int1 > int2 then
                        (TermBool(true),env)
                    else
                        (TermBool(false),env)
                )
                | _ -> raise Illegal_operation
  )
  |(TermGt (TermVar(v1), e2)) -> (
      let (val2, env') = (eval env e2) in
        let val1 = lookup env v1 in
            match (val1, val2) with
                (TermInteger(int1), TermInteger(int2)) -> (
                    if int1 > int2 then
                        (TermBool(true),env)
                    else
                        (TermBool(false),env)
                )
                | _ -> raise Illegal_operation
  )
  |(TermGt (e1,e2)) -> (
      let (val1,env') = (eval env e1) in
        let (val2, env'') = (eval env' e2) in
            match (val1, val2) with
                (TermInteger(int1), TermInteger(int2)) -> (
                    if int1 > int2 then
                        (TermBool(true),env)
                    else
                        (TermBool(false),env)
                )
                | _ -> raise Illegal_operation
  )
  |(TermGte (TermVar(v1), TermVar(v2))) -> (
      let val1 = lookup env v1 in
        let val2 = lookup env v2 in
            match (val1, val2) with
                (TermInteger(int1), TermInteger(int2)) -> (
                    if int1 >= int2 then
                        (TermBool(true),env)
                    else
                        (TermBool(false),env)
                )
                | _ -> raise Illegal_operation
  )
  |(TermGte (TermVar(v1), e2)) -> (
      let (val2, env') = (eval env e2) in
        let val1 = lookup env v1 in
            match (val1, val2) with
                (TermInteger(int1), TermInteger(int2)) -> (
                    if int1 >= int2 then
                        (TermBool(true),env)
                    else
                        (TermBool(false),env)
                )
                | _ -> raise Illegal_operation
  )
  |(TermGte (e1,e2)) -> (
      let (val1,env') = (eval env e1) in
        let (val2, env'') = (eval env' e2) in
            match (val1, val2) with
                (TermInteger(int1), TermInteger(int2)) -> (
                    if int1 >= int2 then
                        (TermBool(true),env)
                    else
                        (TermBool(false),env)
                )
                | _ -> raise Illegal_operation
  )
  |(TermEq (TermVar(v1), TermVar(v2))) -> (
      let val1 = lookup env v1 in
        let val2 = lookup env v2 in
            match (val1, val2) with
                (TermInteger(int1), TermInteger(int2)) -> (
                    if int1 == int2 then
                        (TermBool(true),env)
                    else
                        (TermBool(false),env)
                )
                |(TermBool(bool1), TermBool(bool2)) -> (
                    if bool1 == bool2 then
                        (TermBool(true),env)
                    else
                        (TermBool(false),env)
                )
                |(TermString(str1), (TermString(str2))) -> (
                    if (String.equal str1 str2) then
                        (TermBool(true),env)
                    else
                        (TermBool(false),env)
                ) 
                | _ -> raise Illegal_operation
  )
  |(TermEq (TermVar(v1), e2)) -> (
      let (val2, env') = (eval env e2) in
          let val1 = lookup env' v1 in
            match (val1, val2) with
                (TermInteger(int1), TermInteger(int2)) -> (
                    if int1 == int2 then
                        (TermBool(true),env)
                    else
                        (TermBool(false),env)
                )
                |(TermBool(bool1), TermBool(bool2)) -> (
                    if bool1 == bool2 then
                        (TermBool(true),env)
                    else
                        (TermBool(false),env)
                )
                |(TermString(str1), (TermString(str2))) -> (
                    if (String.equal str1 str2) then
                        (TermBool(true),env)
                    else
                        (TermBool(false),env)
                ) 
                | _ -> raise Illegal_operation
  )
  |(TermEq (e1,e2)) -> (
      let (val1,env') = (eval env e1) in
        let (val2, env'') = (eval env' e2) in
            match (val1, val2) with
                (TermInteger(int1), TermInteger(int2)) -> (
                    if int1 == int2 then
                        (TermBool(true),env)
                    else
                        (TermBool(false),env)
                )
                |(TermBool(bool1), TermBool(bool2)) -> (
                    if bool1 == bool2 then
                        (TermBool(true),env)
                    else
                        (TermBool(false),env)
                )
                |(TermString(str1), (TermString(str2))) -> (
                    if (String.equal str1 str2) then
                        (TermBool(true),env)
                    else
                        (TermBool(false),env)
                ) 
                | _ -> raise Illegal_operation
  )
  |(TermNeq (TermVar(v1), TermVar(v2))) -> (
      let val1 = lookup env v1 in
        let val2 = lookup env v2 in
            match (val1, val2) with
                (TermInteger(int1), TermInteger(int2)) -> (
                    if int1 != int2 then
                        (TermBool(true),env)
                    else
                        (TermBool(false),env)
                )
                |(TermBool(bool1), TermBool(bool2)) -> (
                    if bool1 != bool2 then
                        (TermBool(true),env)
                    else
                        (TermBool(false),env)
                )
                |(TermString(str1), TermString(str2)) -> (
                    if (String.equal str1 str2) then
                        (TermBool(false),env)
                    else
                        (TermBool(true),env)
                ) 
                | _ -> raise Illegal_operation
  )
  |(TermNeq (TermVar(v1), e2)) -> (
      let (val2, env') = (eval env e2) in
        let val1 = lookup env' v1 in
            match (val1, val2) with
                (TermInteger(int1), TermInteger(int2)) -> (
                    if int1 != int2 then
                        (TermBool(true),env)
                    else
                        (TermBool(false),env)
                )
                |(TermBool(bool1), TermBool(bool2)) -> (
                    if bool1 != bool2 then
                        (TermBool(true),env)
                    else
                        (TermBool(false),env)
                )
                |(TermString(str1), TermString(str2)) -> (
                    if (String.equal str1 str2) then
                        (TermBool(false),env)
                    else
                        (TermBool(true),env)
                ) 
                | _ -> raise Illegal_operation
  )
  |(TermNeq (e1,e2)) -> (
      let (val1,env') = (eval env e1) in
        let (val2, env'') = (eval env' e2) in
            match (val1, val2) with
                (TermInteger(int1), TermInteger(int2)) -> (
                    if int1 != int2 then
                        (TermBool(true),env)
                    else
                        (TermBool(false),env)
                )
                |(TermBool(bool1), TermBool(bool2)) -> (
                    if bool1 != bool2 then
                        (TermBool(true),env)
                    else
                        (TermBool(false),env)
                )
                |(TermString(str1), TermString(str2)) -> (
                    if (String.equal str1 str2) then
                        (TermBool(false),env)
                    else
                        (TermBool(true),env)
                ) 
                | _ -> raise Illegal_operation
  )
  | (TermNot (TermVar (v1))) -> (
      let val1 = lookup env v1 in
        match val1 with 
            | TermBool(bool1) -> (TermBool(not bool1),env)
            | _ -> raise Illegal_operation
  )
  | (TermNot (e1)) -> (
        let (e1', env') = (eval env e1) in
            match e1' with 
                | TermBool(bool1) -> (TermBool(not bool1),env)
                | _ -> raise Illegal_operation
  )
  | (PrintOperation x) when (isValue x) -> print_generic env x;(TermNull, env)
  | (PrintOperation x) -> let (e', env') =  (eval env x) in print_generic env' e';(TermNull, env')

  | (ForOperation (TermVar(elem), TermVar(iter), body)) -> (
    try
      lookup env elem;
      failwith("Variable already in use " ^ elem)
    with UnboundError -> (
        try
            match (lookup env iter) with
                  (TermSet set) -> (
                      let rec iterate bindings set_iter =
                        if (SS.is_empty set_iter) then
                            (TermNull, bindings)
                        else
                            let chosen = SS.choose set_iter in (
                                let (t, env') = eval (addBinding bindings (elem, TermString(chosen))) body in
                                    iterate (remove_binding env' elem) (SS.remove chosen set_iter)
                            )
                      in iterate env set;
                    )

                | _ -> raise Not_a_set
        with UnboundError -> failwith(iter ^ " not found.")
        )
    )
  | (ForLoop (TermVar(elem), operation, body)) -> (
      try
          let var_term = lookup env elem in
            let (integer, env') = (eval env operation) in
                match (var_term, integer) with
                    | (TermInteger(start_int), TermInteger(end_int)) -> (
                        let rec iterate bindings i stop =
                            if (i == stop) then
                               (TermNull, bindings)
                            else
                                let (t, env'') = eval (addBinding bindings (elem, TermInteger(i))) body in
                                    iterate (remove_binding env'' elem) (i + 1) stop
                        in
                        iterate env' start_int end_int;

                    )
                    | _ -> raise Illegal_operation
      with UnboundError -> failwith ("Variable " ^ elem ^ " not declared.")
  )
  |(IfStatement (TermVar(elem), statements)) -> (
      let bool_term = lookup env elem in 
        match bool_term with 
            | TermBool (boolean) -> (
                if boolean then 
                    eval env statements
                else
                    (TermNull, env)
            )
            | _ -> raise Illegal_operation
  )
  |(IfStatement (bool_op, statements)) -> (
      let (bool_val, env') = (eval env bool_op) in
        match bool_val with
            | TermBool (value) -> (
                if value then
                    eval env' statements
                else
                    (TermNull, env)
            )
            | _ -> raise Illegal_operation
  )
  |(IfElseStatement (TermVar(elem),if_stmt, else_stmt)) -> (
      let bool_term = lookup env elem in 
        match bool_term with 
            | TermBool (boolean) -> (
                if boolean then 
                    eval env if_stmt 
                else
                    eval env else_stmt 
            )
            | _ -> raise Illegal_operation
  )
  |(IfElseStatement (bool_op, if_stmt, else_stmt)) -> (
      let (bool_val, env') = (eval env bool_op) in
        match bool_val with
            | TermBool (value) -> (
                if value then
                    eval env' if_stmt 
                else
                    eval env' else_stmt
            )
            | _ -> raise Illegal_operation
  )

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
