open Sexp_light

let a s = Sexp.Atom s
let l sl = Sexp.List sl

let print_parsed ~label sexp_str =
  let sexp = Sexp.of_string sexp_str in
  print_endline (Printf.sprintf "%s: %s" label (Sexp.to_string_hum sexp))
;;

let print_parse_error ~label sexp_str =
  try
    let parsed = Sexp.of_string sexp_str in
    print_endline
      (Printf.sprintf
         "/!\\ Unexpected parse success /!\\ %s: %s"
         label
         (Sexp.to_string_hum parsed))
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
  print_parse_error ~label:"Missing closing paren" "(";
  print_parse_error ~label:"Missing opening paren" ")";
  print_parse_error ~label:"Missing closing quote" {|"atom|};
  print_parse_error ~label:"Dangling quote" {|(at"om)|};
  print_parse_error ~label:"Unexpected list" "a b";
  print_parse_error ~label:"Unexpected list" "a (b)";
  [%expect
    {|
    Missing closing paren: Failure("Unexpected end of input at character 1")
    Missing opening paren: Failure("Unexpected ')' without matching '(' at character 0")
    Missing closing quote: Failure("Unexpected end of input at character 5, expected closing quote '\"'")
    Dangling quote: Failure("Invalid atom character '\"' at character 3")
    Unexpected list: Failure("Unexpected character 'b' at position 2, expected end of input")
    Unexpected list: Failure("Unexpected character '(' at position 2, expected end of input")
    |}]
;;
