(*

Checks if a string's parentheses are correctly formatted

  ex: check_paren "5(x + (2y)👍)^3 + 4(z - 1)"
      : bool = true
      check_paren "5(x + (2y))⬅️)^3 + 4(z - 1)"
      : bool = false


*)

let str_to_char_arr (str : string) : char array =
  Array.of_seq (String.to_seq str)

let check_paren (text : string) : bool =
  let rec check_paren_util (opener : int) (closer : int) (str : char array) :
      bool =
    match (opener >= Array.length str, closer >= Array.length str) with
    | true, true -> true
    | true, false ->
        if str.(closer) = ')' then false
        else check_paren_util opener (closer + 1) str
    | false, true ->
        if str.(opener) = '(' then false
        else check_paren_util (opener + 1) closer str
    | false, false -> (
        match (str.(opener), str.(closer)) with
        | '(', ')' -> check_paren_util (opener + 1) (closer + 1) str
        | '(', _ -> check_paren_util opener (closer + 1) str
        | _, _ -> check_paren_util (opener + 1) closer str)
  in
  check_paren_util 0 0 (str_to_char_arr text)
