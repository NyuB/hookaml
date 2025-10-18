open Ppxlib
open Ast_builder.Default

(* Unused section for future of_sexp implementation *)
let case_of_indexed_name ~loc i name =
  case
    ~lhs:(ppat_constant ~loc (Pconst_string (name, loc, None)))
    ~guard:None
    ~rhs:(pexp_constant ~loc (Pconst_integer (string_of_int i, None)))
;;

let generate_index_of_field ~loc names =
  pexp_function_cases ~loc (List.mapi (case_of_indexed_name ~loc) names @ [])
;;

(* End of unused section *)

(** [(Lident "A") %. "B"] is [A.B] *)
let ( %. ) a b = Longident.Ldot (a, b)

module Modules = struct
  let std_list = lident "Stdlib" %. "List"
  let sexplib = lident "Sexplib"
end

let rec pexp_list ~loc = function
  | [] -> pexp_construct ~loc { loc; txt = Modules.std_list %. "[]" } None
  | exp :: tail ->
    pexp_construct
      ~loc
      { loc; txt = Modules.std_list %. "::" }
      (Some (pexp_tuple ~loc [ exp; pexp_list ~loc tail ]))
;;

let constant_atom ~loc s =
  pexp_construct
    ~loc
    { loc; txt = Modules.sexplib %. "Sexp" %. "Atom" }
    (Some (pexp_constant ~loc (Pconst_string (s, loc, None))))
;;

let sexp_list ~loc expressions =
  pexp_construct
    ~loc
    { loc; txt = Modules.sexplib %. "Sexp" %. "List" }
    (Some (pexp_list ~loc expressions))
;;

let access_field ~loc field_name record_exp =
  pexp_field
    ~loc
    (pexp_ident ~loc { loc; txt = lident record_exp })
    { loc; txt = lident field_name }
;;

let is_type typename (type_desc : longident_loc) =
  match type_desc.txt with
  | Lident i when String.equal i typename -> true
  | _ -> false
;;

let rec sexp_of_desc ~loc t =
  let ident s = pexp_ident ~loc { loc; txt = lident s } in
  let dot_ident modul name = pexp_ident ~loc { loc; txt = Ldot (modul, name) } in
  match t with
  | Ptyp_constr ({ loc = _; txt = qualified_type }, []) ->
    (match qualified_type with
     | Lident name -> ident (Printf.sprintf "sexp_of_%s" name)
     | Ldot (modul, name) -> dot_ident modul (Printf.sprintf "sexp_of_%s" name)
     | Lapply (Lident name, opt) when opt = Lident "option" ->
       pexp_apply
         ~loc
         (ident "sexp_of_option")
         [ Nolabel, ident (Printf.sprintf "sexp_of_%s" name) ]
     | Lapply (Ldot (modul, name), opt) when opt = Lident "option" ->
       pexp_apply
         ~loc
         (ident "sexp_of_option")
         [ Nolabel, dot_ident modul (Printf.sprintf "sexp_of_%s" name) ]
     | Lapply (_, _) -> ident "sexp_of_apply")
  | Ptyp_constr (opt, [ t ]) when is_type "option" opt ->
    pexp_apply ~loc (ident "sexp_of_option") [ Nolabel, sexp_of_desc ~loc t.ptyp_desc ]
  | Ptyp_constr (lst, [ t ]) when is_type "list" lst ->
    pexp_apply ~loc (ident "sexp_of_list") [ Nolabel, sexp_of_desc ~loc t.ptyp_desc ]
  (* Unsupported *)
  | Ptyp_constr (_, _) -> ident "sexp_of_constr"
  | Ptyp_any -> ident "sexp_of_any"
  | Ptyp_var _ -> ident "sexp_of_var"
  | Ptyp_arrow (_, _, _) -> ident "sexp_of_arrow"
  | Ptyp_tuple _ -> ident "sexp_of_tuple"
  | Ptyp_object (_, _) -> ident "sexp_of_object"
  | Ptyp_class (_, _) -> ident "sexp_of_class"
  | Ptyp_alias (_, _) -> ident "sexp_of_alias"
  | Ptyp_variant (_, _, _) -> ident "sexp_of_variant"
  | Ptyp_poly (_, _) -> ident "sexp_of_poly"
  | Ptyp_package _ -> ident "sexp_of_package"
  | Ptyp_open (_, _) -> ident "sexp_of_open"
  | Ptyp_extension _ -> ident "sexp_of_extension"
;;

let sexp_of_field_declaration ~loc (typ : label_declaration) : expression =
  let t = typ.pld_type.ptyp_desc in
  sexp_of_desc ~loc t
;;

let generate_sexp_of_field ~loc record_exp field_declaration =
  sexp_list
    ~loc
    [ constant_atom ~loc field_declaration.pld_name.txt
    ; pexp_apply
        ~loc
        (sexp_of_field_declaration ~loc field_declaration)
        [ Nolabel, access_field ~loc field_declaration.pld_name.txt record_exp ]
    ]
;;

type pattern_and_expression =
  { pattern : pattern
  ; expression : expression
  }

let uncapitalize_ascii s =
  let l = String.length s in
  if l = 0
  then s
  else
    Printf.sprintf
      "%s%s"
      (String.sub s 0 1 |> String.lowercase_ascii)
      (String.sub s 1 (l - 1))
;;

let sexp_of_case_of_constructor ~loc (constructor : constructor_declaration) =
  let args =
    match constructor.pcd_args with
    | Pcstr_tuple l ->
      List.mapi
        (fun i arg ->
           let name = Printf.sprintf "arg_%d" i in
           let pattern = ppat_var ~loc { loc; txt = Printf.sprintf "arg_%d" i }
           and expression =
             pexp_apply
               ~loc
               (sexp_of_desc ~loc arg.ptyp_desc)
               [ Nolabel, pexp_ident ~loc { loc; txt = lident name } ]
           in
           { pattern; expression })
        l
    | Pcstr_record _ -> failwith "Unsupported inline record in variant constructor"
  in
  case
    ~lhs:
      (ppat_construct
         ~loc
         { loc; txt = lident constructor.pcd_name.txt }
         (match args with
          | [] -> None
          | [ single ] -> Some single.pattern
          | multiple ->
            Some (ppat_tuple ~loc (List.map (fun arg -> arg.pattern) multiple))))
    ~guard:None
    ~rhs:
      (let constructor_atom =
         constant_atom ~loc (uncapitalize_ascii constructor.pcd_name.txt)
       in
       match args with
       | [] -> constructor_atom
       | [ single ] -> sexp_list ~loc [ constructor_atom; single.expression ]
       | multiple ->
         sexp_list
           ~loc
           [ constructor_atom
           ; sexp_list ~loc (List.map (fun arg -> arg.expression) multiple)
           ])
;;

let sexp_of_body ~loc (td : type_declaration) (argument_name : string) : expression =
  match td.ptype_kind with
  | Ptype_record fields ->
    sexp_list ~loc (List.map (generate_sexp_of_field ~loc argument_name) fields)
  | Ptype_variant constructors ->
    pexp_match
      ~loc
      (pexp_ident ~loc { loc; txt = lident argument_name })
      (List.map (sexp_of_case_of_constructor ~loc) constructors)
  | _ -> failwith "Unsupported type kind, only records and variants are supported"
;;

let generate_sexp_of (td : type_declaration) : structure_item list =
  let sexp_of_t = Printf.sprintf "sexp_of_%s" td.ptype_name.txt in
  let loc = td.ptype_loc in
  [ pstr_value
      ~loc
      Nonrecursive
      [ { pvb_pat = ppat_var ~loc { loc; txt = sexp_of_t }
        ; pvb_loc = loc
        ; pvb_attributes = []
        ; pvb_constraint = None
        ; pvb_expr =
            (let record_argument_name = td.ptype_name.txt in
             pexp_fun
               ~loc
               Nolabel
               None
               (ppat_var ~loc { loc; txt = record_argument_name })
               (sexp_of_body ~loc td record_argument_name))
        }
      ]
  ]
;;

let generate_impl ~ctxt (_rec_flag, (type_declarations : type_declaration list)) =
  ignore ctxt;
  List.concat_map generate_sexp_of type_declarations
;;

let impl_generator = Deriving.Generator.V2.make_noarg generate_impl
let my_deriver = Deriving.add "sexp_light" ~str_type_decl:impl_generator
