open Sexp_light

(* ppx_sexp_conv compatibility *)
module Sexplib0 = Sexp_light
module Sexplib = Sexp_light
open Sexp_light.Std

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

let%expect_test "Sexp of_string (success)" =
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

let%expect_test "Sexp of_string (errors)" =
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

type record =
  { field_str : string
  ; field_int : int
  ; field_float : float
  ; field_char : char
  ; field_opt_str : string option
  ; field_list_str : string list
  }
[@@deriving sexp]

let show_option show_item = function
  | None -> "None"
  | Some item -> Printf.sprintf "Some %s" (show_item item)
;;

let quote s = Printf.sprintf "\"%s\"" s

let show_list show_item l =
  Printf.sprintf "[%s]" (String.concat "; " (List.map show_item l))
;;

let show_record
      { field_str : string
      ; field_int
      ; field_float
      ; field_char
      ; field_opt_str
      ; field_list_str
      }
  =
  Printf.sprintf
    {|{ field_str = "%s"; field_int = %d; field_float = %f; field_char : %c; field_opt_str : %s; field_list_str : %s }|}
    field_str
    field_int
    field_float
    field_char
    (show_option quote field_opt_str)
    (show_list quote field_list_str)
;;

let print_parsed ~label record_str =
  print_endline
    (Printf.sprintf
       "%s: %s"
       label
       (show_record (record_of_sexp (Sexp.of_string record_str))))
;;

let print_parse_error ~of_sexp ~label record_str =
  try
    let record = of_sexp (Sexp.of_string record_str) in
    print_endline
      (Printf.sprintf
         "/!\\ Unexpected parse success /!\\ %s: %s"
         label
         (show_record record))
  with
  | exn -> print_endline (Printf.sprintf "%s: %s" label (Printexc.to_string exn))
;;

let%expect_test "Record of_sexp (success)" =
  print_parsed
    ~label:"Happy path"
    {|(
      (field_str str) (field_int 1) (field_float 2) (field_char a)
      (field_opt_str (some opt))
      (field_list_str (stra strb strc))
      )|};
  print_parsed
    ~label:"Out of order"
    {|(
      (field_int 1) (field_float 2)
      (field_opt_str (some opt)) (field_str str)
      (field_list_str (stra strb strc))
      (field_char a)
      )|};
  [%expect
    {|
    Happy path: { field_str = "str"; field_int = 1; field_float = 2.000000; field_char : a; field_opt_str : Some "opt"; field_list_str : ["stra"; "strb"; "strc"] }
    Out of order: { field_str = "str"; field_int = 1; field_float = 2.000000; field_char : a; field_opt_str : Some "opt"; field_list_str : ["stra"; "strb"; "strc"] }
    |}]
;;

let%expect_test "Record of_sexp (errors)" =
  let print_parser_record_error = print_parse_error ~of_sexp:record_of_sexp in
  print_parser_record_error ~label:"Missing all fields" "()";
  print_parser_record_error ~label:"Not a list" "atom";
  print_parser_record_error
    ~label:"Missing a field"
    {|(
      (field_str str) (field_int 1) (field_float 2) (field_char a)
      (field_opt_str none)
      
      )|};
  print_parser_record_error
    ~label:"Extra field"
    {|(
      (field_str str) (field_int 1) (field_float 2) (field_char a)
      (field_opt_str none)
      (field_list_str ())
      (field_oops oops)
      )|};
  [%expect
    {|
    Missing all fields: Invalid_argument("sexp_light_inline_tests.ml.record: Expected a list of (atom * sexp), received an empty list")
    Not a list: Invalid_argument("sexp_light_inline_tests.ml.record: Expected a list of (atom * sexp), received a lonely atom")
    Missing a field: Invalid_argument("sexp_light_inline_tests.ml.record: (Missing field field_list_str, ((field_str str) (field_int 1) (field_float 2) (field_char a) (field_opt_str none)))")
    Extra field: Invalid_argument("sexp_light_inline_tests.ml.record: Unexpected field field_oops")
    |}]
;;

type record_light =
  { i : int
  ; s : string
  ; opt_str : string option
  ; opt_opt_str : string option option
  ; list_str : string list
  }
[@@deriving sexp_light]

let show_record_light
      { i : int
      ; s : string
      ; opt_str : string option
      ; opt_opt_str : string option option
      ; list_str : string list
      }
  =
  Printf.sprintf
    "{ i = %d; s = %s; opt_str = %s; opt_opt_str = %s; list_str = %s }"
    i
    s
    (show_option quote opt_str)
    (show_option (show_option quote) opt_opt_str)
    (show_list quote list_str)
;;

type variant_light =
  | One
  | Two of int
  | Three of (int * string)
[@@deriving sexp_light]

type recursive_variant =
  | Root
  | Pair of recursive_variant * recursive_variant
[@@deriving sexp_light]

type record_light_option_list = record_light option list [@@deriving sexp_light]

module M = struct
  type t = M [@@deriving sexp_light]

  let show = function
    | M -> "M"
  ;;
end

type m = M.t [@@deriving sexp_light]
type m_open = M.(t) [@@deriving sexp_light]

let print_serialized ~sexp_of ~label record =
  print_endline (Printf.sprintf "%s: %s" label (Sexp.to_string_hum (sexp_of record)))
;;

let%expect_test "Record sexp_of (light)" =
  let print_serialized ~label record =
    print_serialized ~sexp_of:sexp_of_record_light ~label record
  in
  print_serialized
    ~label:"Happy path"
    { i = 1; s = "one"; opt_str = None; opt_opt_str = Some None; list_str = [ "a"; "b" ] };
  [%expect
    {| Happy path: ((i 1) (s one) (opt_str none) (opt_opt_str (some none)) (list_str (a b))) |}]
;;

let%expect_test "Variant sexp_of (light)" =
  (let print_serialized ~label variant =
     print_serialized ~sexp_of:sexp_of_variant_light ~label variant
   in
   print_serialized ~label:"No arg" One;
   print_serialized ~label:"Single arg" (Two 2);
   print_serialized ~label:"Multiple args" (Three (3, "Trois")));
  print_serialized
    ~label:"Recursive"
    ~sexp_of:sexp_of_recursive_variant
    (Pair (Root, Root));
  [%expect
    {|
    No arg: one
    Single arg: (two 2)
    Multiple args: (three (3 Trois))
    Recursive: (pair root root)
    |}]
;;

let%expect_test "Alias sexp_of (light)" =
  print_serialized
    ~label:"Alias"
    ~sexp_of:sexp_of_record_light_option_list
    [ None
    ; Some
        { i = 1
        ; s = "one"
        ; opt_str = None
        ; opt_opt_str = Some None
        ; list_str = [ "a"; "b" ]
        }
    ];
  print_serialized ~label:"M.t" ~sexp_of:sexp_of_m M.M;
  print_serialized ~label:"M.(t)" ~sexp_of:sexp_of_m_open M.M;
  [%expect
    {|
    Alias: (none
     (some
      ((i 1) (s one) (opt_str none) (opt_opt_str (some none)) (list_str (a b)))))
    M.t: m
    M.(t): m
    |}]
;;

let show_variant = function
  | One -> "One"
  | Two n -> Printf.sprintf "Two %d" n
  | Three (n, s) -> Printf.sprintf "Three (%d, %s)" n s
;;

let rec show_recursive_variant = function
  | Root -> "Root"
  | Pair (a, b) ->
    Printf.sprintf "Pair<%s, %s>" (show_recursive_variant a) (show_recursive_variant b)
;;

let print_deserialized ~label of_sexp show sexp =
  try
    print_endline (Printf.sprintf "%s: %s" label (show (of_sexp (Sexp.of_string sexp))))
  with
  | exn -> print_endline (Printf.sprintf "%s: %s" label (Printexc.to_string exn))
;;

let%expect_test "Variant of_sexp (light)" =
  print_deserialized ~label:"No arg" variant_light_of_sexp show_variant "one";
  print_deserialized ~label:"Single arg" variant_light_of_sexp show_variant "(two 2)";
  print_deserialized
    ~label:"Multiple args"
    variant_light_of_sexp
    show_variant
    "(three (3 Trois))";
  print_deserialized
    ~label:"Recursive"
    recursive_variant_of_sexp
    show_recursive_variant
    "(pair root root)";
  [%expect
    {|
    No arg: One
    Single arg: Two 2
    Multiple args: Three (3, Trois)
    Recursive: Pair<Root, Root>
    |}]
;;

let%expect_test "Alias of_sexp (light)" =
  print_deserialized ~label:"alias" m_of_sexp M.show "m";
  [%expect {| alias: M |}]
;;

let%expect_test "Record of_sexp (light)" =
  print_deserialized
    ~label:"Happy path"
    record_light_of_sexp
    show_record_light
    "((i 1) (s one) (opt_str none) (opt_opt_str (some none)) (list_str (a b)))";
  print_deserialized
    ~label:"Out of order"
    record_light_of_sexp
    show_record_light
    "((opt_str none) (s one) (list_str (a b)) (opt_opt_str (some none)) (i 1))";
  [%expect
    {|
    Happy path: { i = 1; s = one; opt_str = None; opt_opt_str = Some None; list_str = ["a"; "b"] }
    Out of order: { i = 1; s = one; opt_str = None; opt_opt_str = Some None; list_str = ["a"; "b"] }
    |}]
;;
