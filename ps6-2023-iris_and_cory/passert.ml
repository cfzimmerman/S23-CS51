let passert ?(suite = "") (cond : bool) (message : string) =
  try
    assert cond;
    print_endline
      ("✅ " ^ (if String.length suite > 0 then suite ^ ": " else "") ^ message)
  with _ ->
    print_endline ("❌ FAILED " ^ message);
    raise (Failure "Test failed")