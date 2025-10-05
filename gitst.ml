open Sexplib.Std

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
    | sexp ->
      raise @@ Sexplib.Conv_error.stag_incorrect_n_args "commit_ref" "merge_base" sexp
  ;;

  type predicate =
    | Starts_with of string
    | Ends_with of string
  [@@deriving sexp]

  type show =
    | Worktree
    | Diff_files of commit_ref * commit_ref
    | Union of show * show
    | Filter of predicate * show
  [@@deriving sexp]

  type projection =
    { repo : string [@default "."]
    ; describe : string
    ; show : show
    }
  [@@deriving sexp]

  type item =
    | Projection of projection
    | Debug
  [@@deriving sexp]

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
    then scanf "A %s" Item.added
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
      | Status_file of Status.Item.t
      | File of string

    let sexp_of_t = function
      | Status_file f -> Status.Item.sexp_of_t f
      | File f -> Sexplib.Sexp.Atom f
    ;;

    let compare a b =
      match a, b with
      | Status_file a, Status_file b -> Status.Item.compare a b
      | File a, File b -> String.compare a b
      | Status_file _, _ -> 1
      | _, Status_file _ -> -1
    ;;
  end

  module ItemSet = Set.Make (Item)

  type t = ItemSet.t

  let sexp_of_t t = sexp_of_list Item.sexp_of_t (ItemSet.to_list t)
  let union (a : t) (b : t) : t = ItemSet.union a b
end

(* Actual execution *)

let apply_predicate (p : Workspace.predicate) (s : Show.Item.t) : bool =
  let as_string =
    match s with
    | File f -> f
    | Status_file f -> Status.Item.file f
  in
  match p with
  | Starts_with prefix -> String.starts_with ~prefix as_string
  | Ends_with suffix -> String.ends_with ~suffix as_string
;;

let filter_show (p : Workspace.predicate) (s : Show.t) =
  Show.ItemSet.filter (apply_predicate p) s
;;

let rec show repo (s : Workspace.show) : Show.t =
  match s with
  | Worktree ->
    Status.of_git repo
    |> Status.ItemSet.to_seq
    |> Seq.map (fun s -> Show.Item.Status_file s)
    |> Show.ItemSet.of_seq
  | Diff_files (a, b) ->
    Git.exec repo [| "diff"; "--name-only"; get_ref repo a; get_ref repo b |]
    |> String.split_on_char '\n'
    |> fun l ->
    Show.ItemSet.of_list
      (List.filter_map
         (function
           | "" -> None
           | s -> Some (Show.Item.File s))
         l)
  | Union (a, b) -> Show.union (show repo a) (show repo b)
  | Filter (p, s) -> filter_show p (show repo s)
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
  let workspace =
    Workspace.t_of_sexp (read_file workspace_file |> Sexplib.Sexp.of_string)
    |> expand_repos workspace_file
  in
  write_if_diff (status workspace) out
;;
