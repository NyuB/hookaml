module MySexplib = struct
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
end

module Sexplib0 = MySexplib
module Sexplib = MySexplib
open MySexplib.Std

let sexp_repr_long = Sexplib.Sexp.to_string_hum

module Git : sig
  (** [exec repository command] runs the git [command] in the given [repository]*)
  val exec : string -> string array -> string
end = struct
  let trim_trailing_newline s =
    if String.ends_with ~suffix:"\r\n" s
    then String.sub s 0 (String.length s - 2)
    else if String.ends_with ~suffix:"\n" s
    then String.sub s 0 (String.length s - 1)
    else s
  ;;

  let exec repo args =
    let ic =
      Unix.open_process_args_in "git" (Array.append [| Sys.argv.(0); "-C"; repo |] args)
    in
    In_channel.input_all ic |> trim_trailing_newline
  ;;
end

(** Description of your workspace *)
module Workspace = struct
  type commit_ref =
    | Ref of string
    | Merge_base of commit_ref * commit_ref

  let rec sexp_of_commit_ref = function
    | Ref s -> Sexplib.Sexp.Atom s
    | Merge_base (a, b) ->
      Sexplib.Sexp.List [ Atom "Merge_base"; sexp_of_commit_ref a; sexp_of_commit_ref b ]
  ;;

  let rec commit_ref_of_sexp : Sexplib.Sexp.t -> commit_ref = function
    | Atom s -> Ref s
    | List [ Atom tag; sa; sb ]
      when String.equal (String.lowercase_ascii tag) "merge_base" ->
      Merge_base (commit_ref_of_sexp sa, commit_ref_of_sexp sb)
    | _ -> raise @@ Sexplib.Sexp_conv_error.conversion_error "commit_ref" "merge_base"
  ;;

  type select =
    | Get of string
    | S of string

  let sexp_of_select = function
    | Get s -> Sexplib.Sexp.Atom s
    | S s -> Sexplib.Sexp.List [ Sexplib.Sexp.Atom "s"; Sexplib.Sexp.Atom s ]
  ;;

  let select_of_sexp = function
    | Sexplib.Sexp.Atom s when String.starts_with ~prefix:":" s -> Get s
    | Sexplib.Sexp.Atom _ ->
      raise
      @@ Sexplib.Sexp_conv_error.conversion_error
           "select"
           "Expected an atom starting with ':'"
    | Sexplib.Sexp.List [ Sexplib.Sexp.Atom get; Sexplib.Sexp.Atom s ]
      when String.equal "get" (String.lowercase_ascii get)
           && String.starts_with ~prefix:":" s -> Get s
    | Sexplib.Sexp.List [ Sexplib.Sexp.Atom s; Sexplib.Sexp.Atom str ]
      when String.equal "s" (String.lowercase_ascii s) -> S str
    | _ -> raise @@ Sexplib.Sexp_conv_error.conversion_error "select" "Unexpected variant"
  ;;

  type predicate =
    | Starts_with of string
    | Ends_with of string
    | Contains of string
    | On of (select * predicate)
  [@@deriving sexp]

  type show =
    | Worktree
    | Diff_files of commit_ref * commit_ref
    | Commit_range of commit_ref * commit_ref
    | Union of show * show
    | Filter of predicate * show
    | Select of select * show
    | Format of select list * show
  [@@deriving sexp]

  type projection =
    { repo : string
    ; describe : string
    ; show : show
    }
  [@@deriving sexp]

  type item =
    | Projection of projection
    | Debug

  let sexp_of_item = function
    | Debug -> Sexplib.Sexp.Atom "debug"
    | Projection projection ->
      Sexplib.Sexp.(List [ Atom "projection"; sexp_of_projection projection ])
  ;;

  let item_of_sexp = function
    | Sexplib.Sexp.Atom "Debug" -> Debug
    | Sexplib.Sexp.(List [ Atom ("projection" | "Projection"); projection ]) ->
      Projection (projection_of_sexp projection)
    | s ->
      raise
      @@ Sexplib.Sexp_conv_error.conversion_error
           "item_of_sexp"
           (Sexplib.Sexp.to_string_hum s)
  ;;

  type t = item list [@@deriving sexp]
end

module Status = struct
  module Item = struct
    type t =
      | Added of string
      | Modified of string
      | Untracked of string
      | Removed of string
    [@@deriving sexp]

    let status = function
      | Added _ -> "Added"
      | Modified _ -> "Modified"
      | Untracked _ -> "Untracked"
      | Removed _ -> "Removed"
    ;;

    let file = function
      | Added f | Modified f | Untracked f | Removed f -> f
    ;;

    let compare a b =
      let a_first = -1
      and b_first = 1 in
      match a, b with
      | Added a, Added b
      | Modified a, Modified b
      | Untracked a, Untracked b
      | Removed a, Removed b -> String.compare a b
      | Added _, _ -> a_first
      | _, Added _ -> b_first
      | Modified _, _ -> a_first
      | _, Modified _ -> b_first
      | Removed _, _ -> a_first
      | _, Removed _ -> b_first
    ;;

    let modified s = Modified s
    let untracked s = Untracked s
    let added s = Added s
    let removed s = Removed s
  end

  module ItemSet = Set.Make (Item)

  let parse_git git_short_line =
    let starts_with prefix = String.starts_with ~prefix git_short_line
    and scanf fmt = Scanf.sscanf_opt git_short_line fmt in
    if starts_with " M"
    then scanf " M %s" Item.modified
    else if starts_with "?? "
    then scanf "?? %s" Item.untracked
    else if starts_with "A "
    then scanf " A %s" Item.added
    else if starts_with " D"
    then scanf " D %s" Item.removed
    else None
  ;;

  let of_git repo =
    Git.exec repo [| "status"; "--short"; "--porcelain" |]
    |> String.split_on_char '\n'
    |> List.filter_map parse_git
    |> ItemSet.of_list
  ;;
end

let rec get_ref repo (r : Workspace.commit_ref) =
  match r with
  | Ref s -> s
  | Merge_base (a, b) -> Git.exec repo [| "merge-base"; get_ref repo a; get_ref repo b |]
;;

(** Workspace status representation at a given instant *)
module Show = struct
  module Item = struct
    type t =
      | String of string
      | Record of (string * t) list
      | Error of string

    let rec sexp_of_t =
      let recurse = sexp_of_t in
      function
      | String s -> Sexplib.Sexp.Atom s
      | Error s -> Sexplib.Sexp.(List [ Atom "Error"; Atom s ])
      | Record r ->
        Sexplib.Sexp.(List (List.map (fun (k, v) -> List [ Atom k; recurse v ]) r))
    ;;

    let rec string_of_t = function
      | String s -> s
      | Error s -> Printf.sprintf "Error: %s" s
      | Record r ->
        r
        |> List.map (fun (k, v) -> Printf.sprintf "(%s %s)" k (string_of_t v))
        |> String.concat " "
    ;;

    let compare_record_item compare (ka, a) (kb, b) =
      let key_compare = String.compare ka kb in
      if key_compare != 0 then compare a b else key_compare
    ;;

    let rec compare a b =
      match a, b with
      | String a, String b -> String.compare a b
      | Error a, Error b -> String.compare a b
      | Record a, Record b -> List.compare (compare_record_item compare) a b
      | _, Record _ -> -1
      | Record _, _ -> 1
      | String _, _ -> 1
      | _, String _ -> -1
    ;;

    let string_content = function
      | String s -> Some s
      | Record _ -> None
      | Error _ -> None
    ;;
  end

  module ItemSet = Set.Make (Item)

  type t = Item.t list

  let sexp_of_t t = sexp_of_list Item.sexp_of_t t

  let union (a : t) (b : t) : t =
    ItemSet.union (ItemSet.of_list a) (ItemSet.of_list b) |> ItemSet.to_list
  ;;
end

(* Actual execution *)

let apply_select (select : Workspace.select) (s : Show.Item.t) =
  match select, s with
  | Get field, Record r ->
    List.find_opt (fun (k, _) -> String.equal k field) r
    |> Option.map snd
    |> Option.value
         ~default:(Show.Item.Error (Printf.sprintf "Cannot select field %s" field))
  | Get f, _ -> Error (Printf.sprintf "Cannot get field %s" f)
  | S s, _ -> String s
;;

let rec apply_predicate (p : Workspace.predicate) (s : Show.Item.t) : bool =
  match p with
  | Starts_with prefix ->
    (match Show.Item.string_content s with
     | None -> false
     | Some s -> String.starts_with ~prefix s)
  | Ends_with suffix ->
    (match Show.Item.string_content s with
     | None -> false
     | Some s -> String.ends_with ~suffix s)
  | Contains needle ->
    (match Show.Item.string_content s with
     | None -> false
     | Some s -> Substring.contains ~needle s)
  | On (selector, predicate) -> apply_predicate predicate (apply_select selector s)
;;

let filter_show (p : Workspace.predicate) (s : Show.t) = List.filter (apply_predicate p) s

let sub_delimited s ~delimiter =
  let l = String.length s in
  let d = String.length delimiter in
  let rec aux acc i j =
    if j = l
    then List.rev acc
    else if j + d >= l
    then aux (String.sub s i (l - i) :: acc) l l
    else if String.equal delimiter (String.sub s j d)
    then aux (String.sub s i (j - i) :: acc) (j + d) (j + d)
    else aux acc i (j + 1)
  in
  aux [] 0 0
;;

let get_commit_range repo a b =
  Git.exec
    repo
    [| "log"; "--oneline"; "--format=%h@@@%an@@@%s"; Printf.sprintf "%s..%s" a b |]
  |> String.split_on_char '\n'
  |> List.map (sub_delimited ~delimiter:"@@@")
  |> List.map (function
    | [ h; author; message ] ->
      Show.Item.Record
        [ ":hash", Show.Item.String h
        ; ":author", Show.Item.String author
        ; ":message", Show.Item.String message
        ]
    | _ -> failwith "unreachable")
;;

let apply_format selectors s =
  List.to_seq selectors
  |> Seq.map (fun select -> apply_select select s)
  |> Seq.map Show.Item.string_of_t
  |> List.of_seq
  |> String.concat " "
  |> fun s -> Show.Item.String s
;;

let rec show repo (s : Workspace.show) : Show.t =
  match s with
  | Worktree ->
    Status.of_git repo
    |> Status.ItemSet.to_seq
    |> Seq.map (fun s ->
      Show.Item.Record
        [ ":status", Show.Item.String (Status.Item.status s)
        ; ":file", Show.Item.String (Status.Item.file s)
        ])
    |> List.of_seq
  | Diff_files (a, b) ->
    Git.exec repo [| "diff"; "--name-only"; get_ref repo a; get_ref repo b |]
    |> String.split_on_char '\n'
    |> fun l ->
    List.filter_map
      (function
        | "" -> None
        | s -> Some Show.Item.(Record [ ":file", String s ]))
      l
  | Commit_range (a, b) -> get_commit_range repo (get_ref repo a) (get_ref repo b)
  | Union (a, b) -> Show.union (show repo a) (show repo b)
  | Filter (p, s) -> filter_show p (show repo s)
  | Select (selector, from) ->
    List.filter_map
      (fun s ->
         match apply_select selector s with
         | Error _ -> None
         | s -> Some s)
      (show repo from)
  | Format (selectors, s) ->
    show repo s
    |> List.to_seq
    |> Seq.map (apply_format selectors)
    |> Seq.map Show.Item.string_of_t
    |> Seq.map (fun s -> Show.Item.String s)
    |> List.of_seq
;;

let status workspace =
  let items_repr =
    List.map
      (fun (item : Workspace.item) ->
         match item with
         | Debug -> Workspace.sexp_of_t workspace |> sexp_repr_long
         | Projection p ->
           Printf.sprintf
             "%s:\n%s"
             p.describe
             (show p.repo p.show |> Show.sexp_of_t |> sexp_repr_long))
      workspace
  in
  String.concat "\n\n" items_repr
;;

(* read/write/main machinery *)

let read_file f = In_channel.with_open_bin f In_channel.input_all

let write_file f content =
  Out_channel.with_open_bin f @@ fun oc -> Out_channel.output_string oc content
;;

let write_if_diff content out =
  if not @@ Sys.file_exists out
  then write_file out content
  else (
    let current = read_file out in
    if String.equal content current then () else write_file out content)
;;

let expand_repo workspace_file (Workspace.{ repo; _ } as projection) =
  let base_dir = Filename.dirname workspace_file in
  let repo = Filename.concat base_dir repo in
  { projection with repo }
;;

(** Resolve repository paths assumed relative to [workspace_file] *)
let expand_repos workspace_file workspace =
  List.map
    Workspace.(
      function
      | Debug -> Debug
      | Projection p -> Workspace.Projection (expand_repo workspace_file p))
    workspace
;;

let () =
  let workspace_file = Sys.argv.(1)
  and out = Sys.argv.(2) in
  let workspace_sexp = read_file workspace_file |> Sexplib.Sexp.of_string in
  let workspace = Workspace.t_of_sexp workspace_sexp |> expand_repos workspace_file in
  write_if_diff (status workspace) out
;;
