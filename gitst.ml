(* ppx_sexp_conv compatibility *)
module Sexplib0 = Sexp_light
module Sexplib = Sexp_light
open Sexp_light.Std

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
  [@@deriving sexp_light]

  type show =
    | Worktree
    | Diff_files of commit_ref * commit_ref
    | Commit_range of commit_ref * commit_ref
    | Union of show * show
    | Filter of predicate * show
    | Select of select * show
    | Format of select list * show
  [@@deriving sexp_light]

  type projection =
    { repo : string
    ; describe : string
    ; show : show
    }
  [@@deriving sexp_light]

  type item =
    | Projection of projection
    | Debug
  [@@deriving sexp_light]

  type t = item list [@@deriving sexp_light]
end

module Status = struct
  module Item = struct
    type t =
      | Added of string
      | Modified of string
      | Untracked of string
      | Removed of string
    [@@deriving sexp_light]

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
         | Debug -> Workspace.sexp_of_t workspace |> Sexplib.Sexp.to_string_hum
         | Projection p ->
           Printf.sprintf
             "%s:\n%s"
             p.describe
             (show p.repo p.show |> Show.sexp_of_t |> Sexplib.Sexp.to_string_hum))
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
