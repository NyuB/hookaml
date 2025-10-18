module Sexp = struct
  type t =
    | Atom of string
    | List of t list

  let string_of_atom s =
    let l = String.length s in
    if l = 0
    then "\"\""
    else (
      let buff = Buffer.create l in
      let final should_quote =
        if should_quote
        then Printf.sprintf "\"%s\"" (Buffer.to_bytes buff |> String.of_bytes)
        else Buffer.to_bytes buff |> String.of_bytes
      in
      let rec aux should_quote i =
        if i = l
        then final should_quote
        else (
          match String.get s i with
          | '"' ->
            Buffer.add_string buff "\\\"";
            aux true (i + 1)
          | '\n' ->
            Buffer.add_string buff "\\n";
            aux true (i + 1)
          | ' ' ->
            Buffer.add_char buff ' ';
            aux true (i + 1)
          | '\t' ->
            Buffer.add_char buff '\t';
            aux true (i + 1)
          | '(' ->
            Buffer.add_char buff '(';
            aux true (i + 1)
          | ')' ->
            Buffer.add_char buff ')';
            aux true (i + 1)
          | c ->
            Buffer.add_char buff c;
            aux should_quote (i + 1))
      in
      aux false 0)
  ;;

  let rec format_sexp fmt = function
    | Atom a -> Format.pp_print_string fmt (string_of_atom a)
    | List l ->
      Format.pp_print_char fmt '(';
      Format.pp_open_box fmt 0;
      List.fold_left
        (fun first sexp ->
           if not first then Format.pp_print_space fmt ();
           format_sexp fmt sexp;
           false)
        true
        l
      |> ignore;
      Format.pp_close_box fmt ();
      Format.pp_print_char fmt ')'
  ;;

  let to_string_hum sexp =
    let buff = Buffer.create 16 in
    let fmt = Format.formatter_of_buffer buff in
    format_sexp fmt sexp;
    Format.pp_print_flush fmt ();
    String.of_bytes (Buffer.to_bytes buff)
  ;;

  type parser =
    { mutable index : int
    ; s : string
    }

  let skip parser = parser.index <- parser.index + 1

  let expect parser c =
    if String.get parser.s parser.index != c
    then
      failwith
        (Printf.sprintf
           "Unexpected character at position %d, expected '%c'"
           parser.index
           c);
    skip parser
  ;;

  let peek parser = String.get parser.s parser.index

  let read parser =
    let c = peek parser in
    skip parser;
    c
  ;;

  let skip_whitespaces parser =
    let l = String.length parser.s in
    while
      parser.index < l
      && (peek parser == ' ' || peek parser == '\t' || peek parser == '\n')
    do
      skip parser
    done
  ;;

  let read_atom parser =
    let quoted = peek parser = '"' in
    if quoted then skip parser;
    let buff = Buffer.create 16 in
    let final () = String.of_bytes (Buffer.to_bytes buff) in
    let push c = Buffer.add_char buff c in
    let rec aux () =
      if parser.index = String.length parser.s
      then if quoted then failwith "Expected closing quote '\"'" else final ()
      else (
        match peek parser with
        | '"' ->
          if quoted
          then (
            skip parser;
            final ())
          else failwith "Invalid atom character '\"'"
        | ')' | '(' | ' ' | '\t' | '\n' ->
          if quoted
          then (
            push (read parser);
            aux ())
          else final ()
        | '\\' ->
          (match read parser with
           | ('\\' as c) | ('"' as c) ->
             push c;
             aux ()
           | 'n' ->
             push '\n';
             aux ()
           | 't' ->
             push '\t';
             aux ()
           | c -> failwith (Printf.sprintf "Invalid escape character %c" c))
        | _ ->
          push (read parser);
          aux ())
    in
    aux ()
  ;;

  let rec read_sexp parser =
    skip_whitespaces parser;
    if peek parser = '('
    then (
      skip parser;
      skip_whitespaces parser;
      let items = Dynarray.create () in
      while peek parser != ')' do
        Dynarray.add_last items (read_sexp parser);
        skip_whitespaces parser
      done;
      expect parser ')';
      List (Dynarray.to_list items))
    else Atom (read_atom parser)
  ;;

  let of_string s = read_sexp { s; index = 0 }

  let rec equal sa sb =
    match sa, sb with
    | Atom a, Atom b -> String.equal a b
    | List la, List lb -> List.equal equal la lb
    | _ -> false
  ;;

  let rec compare sa sb =
    match sa, sb with
    | Atom a, Atom b -> String.compare a b
    | List la, List lb -> List.compare compare la lb
    | Atom _, List _ -> 1
    | List _, Atom _ -> -1
  ;;
end

module Sexp_conv_error = struct
  exception Conversion_error of string * string

  (** This tag expects some arguments but received an atom *)
  exception Missing_arguments_for_tag of string * Sexp.t

  (** This tag expects n arguments but received a list with > or < arguments count *)
  exception Incorrect_argument_count_for_tag of string * string * Sexp.t

  (** This tag expects no argument but received a list *)
  exception Unexpected_arguments_for_tag of string * Sexp.t

  (** Unexpected tag *)
  exception Unexpected_tag of string * Sexp.t

  (** Expected a tag but a list was given *)
  exception Nested_list_invalid_sum of string * Sexp.t

  (** Expected a tagged list but an empty list was given *)
  exception Empty_list_invalid_sum of string * Sexp.t

  (** Expected a tuple with n field but received a list with > or < arguments *)
  exception Incorrect_tuple_size of string * int * Sexp.t

  let conversion_error from msg = Conversion_error (from, msg)
  let stag_takes_args from tag = raise @@ Missing_arguments_for_tag (from, tag)

  let stag_incorrect_n_args from tag sexp =
    raise @@ Incorrect_argument_count_for_tag (from, tag, sexp)
  ;;

  let stag_no_args from tag = raise @@ Unexpected_arguments_for_tag (from, tag)
  let nested_list_invalid_sum from tag = raise @@ Nested_list_invalid_sum (from, tag)
  let empty_list_invalid_sum from tag = raise @@ Empty_list_invalid_sum (from, tag)
  let unexpected_stag from tag = raise @@ Unexpected_tag (from, tag)

  let tuple_of_size_n_expected from actual_n sexp =
    raise @@ Incorrect_tuple_size (from, actual_n, sexp)
  ;;
end

module Sexp_conv_record = struct
  open Sexp

  type (_, _) kind = Required : ('a, Sexp.t -> 'a) kind

  type _ fields =
    | Empty : unit fields
    | Field :
        { name : string
        ; kind : ('a, 'conv) kind
        ; conv : 'conv
        ; rest : 'b fields
        }
        -> ('a * 'b) fields

  let rec conv_fields_of_sexps : type a. a fields -> Sexp.t list -> a =
    fun fields sexp ->
    match fields, sexp with
    | Empty, [] -> ()
    | Field { conv; kind; rest; name; _ }, head :: tail ->
      (match kind with
       | Required ->
         let a =
           try conv head with
           | e ->
             prerr_endline (Printf.sprintf "Error while trying to read field %s" name);
             raise e
         in
         a, conv_fields_of_sexps rest tail)
    | _ -> failwith "unreachable"
  ;;

  let record_of_sexp
        ~(caller : string)
        ~(create : 'fields -> 'record)
        ~(fields : 'fields fields)
        ~(index_of_field : string -> int)
        ~allow_extra_fields
        l
    : 'record
    =
    let shape_error describe =
      raise
      @@ Invalid_argument
           (Printf.sprintf
              "%s: Expected a list of (atom * sexp), received %s"
              caller
              describe)
    in
    let pairs =
      match l with
      | Atom _ -> shape_error "a lonely atom"
      | List [] -> shape_error "an empty list"
      | List l ->
        List.map
          (function
            | Atom _ -> shape_error "a lonely atom item"
            | List [] -> shape_error "an empty list item"
            | List [ Atom field; sexp ] -> field, sexp
            | List _ -> shape_error "a list of more than 2 items")
          l
    in
    let sorted_by_field =
      pairs
      |> List.filter (fun (name, _) ->
        let index = index_of_field name in
        if index >= 0
        then true
        else if allow_extra_fields
        then false
        else failwith (Printf.sprintf "%s: Unexpected field %s" caller name))
      |> List.sort (fun (fa, _) (fb, _) ->
        Int.compare (index_of_field fa) (index_of_field fb))
      |> List.map snd
    in
    conv_fields_of_sexps fields sorted_by_field |> create
  ;;
end

module Std = struct
  open Sexp

  let string_of_sexp = function
    | Atom s -> s
    | List _ as l ->
      raise
      @@ Invalid_argument
           (Printf.sprintf "Expected an atom got a list [%s]" (Sexp.to_string_hum l))
  ;;

  let list_of_sexp item_of_sexp = function
    | Atom _ -> raise @@ Invalid_argument "Expected a list got an atom"
    | List l -> List.map item_of_sexp l
  ;;

  let sexp_of_list sexp_of_item l = List ((List.map sexp_of_item) l)
  let sexp_of_string s = Atom s
end
