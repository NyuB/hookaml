open Sexp_light

let a s = Sexp.Atom s
let l sl = Sexp.List sl

let print_parsed ~label sexp_str =
  let sexp = Sexp.of_string sexp_str in
  print_endline (Printf.sprintf "%s: %s" label (Sexp.to_string_hum sexp))
;;

let print_parse_error ~label sexp_str =
  try
    Sexp.of_string sexp_str |> ignore;
    print_endline (Printf.sprintf "%s: /!\\ Unexpected parse sucess /!\\" label)
  with
  | exn -> print_endline (Printf.sprintf "%s: %s" label (Printexc.to_string exn))
;;

let%expect_test "Parse (success)" =
  print_parsed ~label:"empty" "()";
  print_parsed ~label:"atom" "abcd";
  print_parsed ~label:"special characters" "a.b:c@d_e'";
  print_parsed ~label:"spaces" "(\t a  b c\nd)";
  print_parsed ~label:"nested" "(())";
  print_parsed ~label:"nested" "(a (b (c d)) e ())";
  [%expect
    {|
    empty: ()
    atom: abcd
    special characters: a.b:c@d_e'
    spaces: (a b c d)
    nested: (())
    nested: (a (b (c d)) e ())
    |}]
;;

let%expect_test "Parse (errors)" =
  print_parse_error ~label:"missing closing paren" "(";
  [%expect {| missing closing paren: Invalid_argument("index out of bounds") |}]
;;
