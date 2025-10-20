open Ppxlib
open Ast_builder.Default

(** [(Lident "A") %. "B"] is [A.B] *)
let ( %. ) a b = Longident.Ldot (a, b)

let ident ~loc s = pexp_ident ~loc { loc; txt = lident s }
let dot_ident ~loc modul name = pexp_ident ~loc { loc; txt = Ldot (modul, name) }
let false_constant ~loc = pexp_construct ~loc { loc; txt = lident "false" } None

module Embed_error = struct
  let exp ~loc msg = pexp_extension ~loc (Location.error_extensionf ~loc msg)
  let pat ~loc msg = ppat_extension ~loc (Location.error_extensionf ~loc msg)

  let failwith ~loc msg =
    pexp_apply
      ~loc
      (pexp_ident ~loc { loc; txt = lident "failwith" })
      [ Nolabel, pexp_constant ~loc (Pconst_string (msg, loc, None)) ]
  ;;
end

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

type pattern_and_expression =
  { pattern : pattern
  ; expression : expression
  }

(** [sexp_of_tuple ~loc sexp_of_desc types] is a function applicable to an expression [e] which is a tuple of [(types)] that would produce the conversion of [e] to a sexp 
@param sexp_of_desc the recursive definition for the inner types of the tuple
*)
let sexp_of_tuple
      ~loc
      (sexp_of_desc : loc:location -> core_type_desc -> expression)
      (types : core_type list)
  : expression
  =
  let args =
    List.mapi
      (fun i typ ->
         let param_name = Printf.sprintf "arg_%d" i in
         { pattern = ppat_var ~loc { loc; txt = param_name }
         ; expression =
             pexp_apply
               ~loc
               (sexp_of_desc ~loc typ.ptyp_desc)
               [ Nolabel, pexp_ident ~loc { loc; txt = lident param_name } ]
         })
      types
  in
  pexp_function
    ~loc
    [ { pparam_loc = loc
      ; pparam_desc =
          Pparam_val
            (Nolabel, None, ppat_tuple ~loc (List.map (fun arg -> arg.pattern) args))
      }
    ]
    None
    (Pfunction_body (sexp_list ~loc (List.map (fun arg -> arg.expression) args)))
;;

(** [sexp_of_desc ~loc t] is a function applicable to an expression [e] of type [t] that would produce the conversion of [e] to a sexp *)
let rec sexp_of_desc ~loc (t : core_type_desc) : expression =
  let ident = ident ~loc
  and dot_ident = dot_ident ~loc in
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
  | Ptyp_tuple types -> sexp_of_tuple ~loc sexp_of_desc types
  | Ptyp_open (modul, typ) ->
    pexp_open
      ~loc
      { popen_expr =
          { pmod_loc = loc; pmod_desc = Pmod_ident modul; pmod_attributes = [] }
      ; popen_override = Fresh
      ; popen_attributes = []
      ; popen_loc = loc
      }
      (sexp_of_desc ~loc typ.ptyp_desc)
  (* Unsupported *)
  | Ptyp_constr (_, _) ->
    Embed_error.exp
      ~loc
      "Cannot derive sexp_of for parameterized types other than option and list"
  | Ptyp_any -> Embed_error.exp ~loc "Cannot derive sexp_of for any types"
  | Ptyp_var _ -> Embed_error.exp ~loc "Cannot derive sexp_of for type variables"
  | Ptyp_arrow (_, _, _) ->
    Embed_error.exp ~loc "Cannot derive sexp_of for function types"
  | Ptyp_object (_, _) -> Embed_error.exp ~loc "Cannot derive sexp_of for object types"
  | Ptyp_class (_, _) -> Embed_error.exp ~loc "Cannot derive sexp_of for class types"
  | Ptyp_alias (_, _) -> Embed_error.exp ~loc "Cannot derive sexp_of for alias types"
  | Ptyp_variant (_, _, _) ->
    Embed_error.exp ~loc "Cannot derive sexp_of for variant types"
  | Ptyp_poly (_, _) -> Embed_error.exp ~loc "Cannot derive sexp_of for poly types"
  | Ptyp_package _ -> Embed_error.exp ~loc "Cannot derive sexp_of for package types"
  | Ptyp_extension _ -> Embed_error.exp ~loc "Cannot derive sexp_of for extension types"
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
    | Pcstr_record _ ->
      [ { pattern = ppat_any ~loc
        ; expression =
            Embed_error.exp ~loc "Unsupported inline record in variant constructor"
        }
      ]
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
           ([ constructor_atom ] @ List.map (fun arg -> arg.expression) multiple))
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
  | Ptype_abstract ->
    (match td.ptype_manifest with
     | None (* type t *) -> Embed_error.exp ~loc "Cannot derive sexp_of for empty types"
     | Some t (* type alias = t *) ->
       pexp_apply
         ~loc
         (sexp_of_desc ~loc t.ptyp_desc)
         [ Nolabel, pexp_ident ~loc { loc; txt = lident argument_name } ])
  | Ptype_open -> Embed_error.exp ~loc "Cannot derive sexp_of for open types"
;;

let rec declaration_found_in_description
          (declaration : type_declaration)
          (desc : core_type_desc)
  : bool
  =
  match desc with
  | Ptyp_arrow (_, ta, tb) ->
    declaration_found_in_description declaration ta.ptyp_desc
    || declaration_found_in_description declaration tb.ptyp_desc
  | Ptyp_constr ({ txt = Lident id; _ }, ts) ->
    String.equal id declaration.ptype_name.txt
    || List.exists (fun t -> declaration_found_in_description declaration t.ptyp_desc) ts
  | Ptyp_constr (_, ts) | Ptyp_tuple ts ->
    List.exists (fun t -> declaration_found_in_description declaration t.ptyp_desc) ts
  | Ptyp_alias (t, _) | Ptyp_poly (_, t) | Ptyp_open (_, t) ->
    declaration_found_in_description declaration t.ptyp_desc
  | Ptyp_variant (_, _, _)
  | Ptyp_package _ | Ptyp_extension _
  | Ptyp_class (_, _)
  | Ptyp_object (_, _)
  | Ptyp_any | Ptyp_var _ -> false
;;

let type_recursivity (td : type_declaration) : rec_flag =
  let all_implied_types =
    match td.ptype_kind with
    | Ptype_variant constructors ->
      List.concat_map
        (fun c ->
           match c.pcd_args with
           | Pcstr_tuple ts -> ts
           | Pcstr_record _ -> [])
        constructors
    | Ptype_record fields -> List.map (fun f -> f.pld_type) fields
    | _ -> []
  in
  if
    List.exists
      (fun t -> declaration_found_in_description td t.ptyp_desc)
      all_implied_types
  then Recursive
  else Nonrecursive
;;

let generate_sexp_of (td : type_declaration) : structure_item =
  let sexp_of_t = Printf.sprintf "sexp_of_%s" td.ptype_name.txt in
  let loc = td.ptype_loc in
  pstr_value
    ~loc
    (type_recursivity td)
    [ { pvb_pat = ppat_var ~loc { loc; txt = sexp_of_t }
      ; pvb_loc = loc
      ; pvb_attributes = []
      ; pvb_constraint = None
      ; pvb_expr =
          (let argument_name = td.ptype_name.txt in
           pexp_fun
             ~loc
             Nolabel
             None
             (ppat_var ~loc { loc; txt = argument_name })
             (sexp_of_body ~loc td argument_name))
      }
    ]
;;

let pattern_constant_string ~loc s = ppat_constant ~loc (Pconst_string (s, loc, None))

let rec ppat_list ~loc = function
  | [] -> ppat_construct ~loc { loc; txt = Modules.std_list %. "[]" } None
  | exp :: tail ->
    ppat_construct
      ~loc
      { loc; txt = Modules.std_list %. "::" }
      (Some (ppat_tuple ~loc [ exp; ppat_list ~loc tail ]))
;;

let pattern_constant_atom_constructor ~loc constructor_name =
  ppat_construct
    ~loc
    { loc; txt = Modules.sexplib %. "Sexp" %. "Atom" }
    (Some
       (ppat_or
          ~loc
          (pattern_constant_string ~loc constructor_name)
          (pattern_constant_string ~loc (uncapitalize_ascii constructor_name))))
;;

let pattern_sexp_list ~loc patterns =
  ppat_construct
    ~loc
    { loc; txt = Modules.sexplib %. "Sexp" %. "List" }
    (Some (ppat_list ~loc patterns))
;;

let tuple_of_sexp ~loc desc_of_sexp (types : core_type list) =
  let args =
    List.mapi
      (fun i typ ->
         let param_name = Printf.sprintf "arg_%d" i in
         { pattern = ppat_var ~loc { loc; txt = param_name }
         ; expression =
             pexp_apply
               ~loc
               (desc_of_sexp ~loc typ.ptyp_desc)
               [ Nolabel, pexp_ident ~loc { loc; txt = lident param_name } ]
         })
      types
  in
  pexp_function_cases
    ~loc
    [ case
        ~lhs:(pattern_sexp_list ~loc (List.map (fun arg -> arg.pattern) args))
        ~guard:None
        ~rhs:(pexp_tuple ~loc (List.map (fun arg -> arg.expression) args))
    ; case
        ~lhs:(ppat_any ~loc)
        ~guard:None
        ~rhs:(Embed_error.failwith ~loc "tuple_of_sexp error")
    ]
;;

let rec desc_of_sexp ~loc (desc : core_type_desc) =
  match desc with
  | Ptyp_constr ({ txt = qualified_type; loc }, []) ->
    (match qualified_type with
     | Lident typ -> ident ~loc (Printf.sprintf "%s_of_sexp" typ)
     | Ldot (modul, typ) -> dot_ident ~loc modul (Printf.sprintf "%s_of_sexp" typ)
     | Lapply (_, _) -> Embed_error.exp ~loc "Unsupported of_sexp for applied types")
  | Ptyp_tuple types -> tuple_of_sexp ~loc desc_of_sexp types
  | Ptyp_constr (opt, [ t ]) when is_type "option" opt ->
    pexp_apply
      ~loc
      (ident ~loc "option_of_sexp")
      [ Nolabel, desc_of_sexp ~loc t.ptyp_desc ]
  | Ptyp_constr (list, [ t ]) when is_type "list" list ->
    pexp_apply ~loc (ident ~loc "list_of_sexp") [ Nolabel, desc_of_sexp ~loc t.ptyp_desc ]
  | Ptyp_open (modul, typ) ->
    pexp_open
      ~loc
      { popen_expr =
          { pmod_loc = loc; pmod_desc = Pmod_ident modul; pmod_attributes = [] }
      ; popen_override = Fresh
      ; popen_attributes = []
      ; popen_loc = loc
      }
      (desc_of_sexp ~loc typ.ptyp_desc)
  (* Unsupported *)
  | Ptyp_constr (_, _) ->
    Embed_error.exp
      ~loc
      "Unsupported of_sexp for parameterized types other than list and option"
  | Ptyp_any -> Embed_error.exp ~loc "Unsupported of_sexp for any type"
  | Ptyp_var _ -> Embed_error.exp ~loc "Unsupported of_sexp for type parameters"
  | Ptyp_arrow (_, _, _) -> Embed_error.exp ~loc "Unsupported of_sexp for function types"
  | Ptyp_object (_, _) -> Embed_error.exp ~loc "Unsupported of_sexp for object types"
  | Ptyp_class (_, _) -> Embed_error.exp ~loc "Unsupported of_sexp for class types"
  | Ptyp_alias (_, _) -> Embed_error.exp ~loc "Unsupported of_sexp for alias types"
  | Ptyp_variant (_, _, _) -> Embed_error.exp ~loc "Unsupported of_sexp for variant types"
  | Ptyp_poly (_, _) -> Embed_error.exp ~loc "Unsupported of_sexp for polymorphic types"
  | Ptyp_package _ -> Embed_error.exp ~loc "Unsupported of_sexp for module types"
  | Ptyp_extension _ -> Embed_error.exp ~loc "Unsupported of_sexp for extension types"
;;

let of_sexp_case_of_constructor (constructor : constructor_declaration) =
  let loc = constructor.pcd_loc in
  let args =
    match constructor.pcd_args with
    | Pcstr_record _ ->
      [ { pattern = Embed_error.pat ~loc "Unsupported of_sexp for inline records"
        ; expression = Embed_error.exp ~loc "Unsupported of_sexp for inline records"
        }
      ]
    | Pcstr_tuple l ->
      List.mapi
        (fun i (arg : core_type) ->
           let arg_name = Printf.sprintf "arg_%d" i in
           let pattern = ppat_var ~loc { loc; txt = arg_name } in
           let expression =
             pexp_apply
               ~loc
               (desc_of_sexp ~loc arg.ptyp_desc)
               [ Nolabel, ident ~loc arg_name ]
           in
           { pattern; expression })
        l
  in
  let constructor_name = constructor.pcd_name.txt in
  case
    ~lhs:
      (match args with
       | [] -> pattern_constant_atom_constructor ~loc constructor_name
       | [ single ] ->
         pattern_sexp_list
           ~loc
           [ pattern_constant_atom_constructor ~loc constructor_name; single.pattern ]
       | multiple ->
         pattern_sexp_list
           ~loc
           ([ pattern_constant_atom_constructor ~loc constructor_name ]
            @ List.map (fun arg -> arg.pattern) multiple))
    ~guard:None
    ~rhs:
      (match args with
       | [] -> pexp_construct ~loc { loc; txt = lident constructor_name } None
       | [ single ] ->
         pexp_construct
           ~loc
           { loc; txt = lident constructor_name }
           (Some single.expression)
       | multiple ->
         pexp_construct
           ~loc
           { loc; txt = lident constructor_name }
           (Some (pexp_tuple ~loc (List.map (fun arg -> arg.expression) multiple))))
;;

let case_of_indexed_name ~loc i name =
  case
    ~lhs:(ppat_constant ~loc (Pconst_string (name, loc, None)))
    ~guard:None
    ~rhs:(pexp_constant ~loc (Pconst_integer (string_of_int i, None)))
;;

let generate_index_of_field ~loc fields =
  pexp_function_cases
    ~loc
    (List.mapi (case_of_indexed_name ~loc) (List.map (fun f -> f.pld_name.txt) fields)
     @ [ case
           ~lhs:(ppat_any ~loc)
           ~guard:None
           ~rhs:(Embed_error.failwith ~loc "of_sexp error")
       ])
;;

let generate_create ~loc (fields : label_declaration list) =
  let rec generate_pat i = function
    | [] -> ppat_any ~loc
    | (field : label_declaration) :: tail ->
      ppat_tuple
        ~loc
        [ ppat_var ~loc { loc; txt = Printf.sprintf "%s_%d" field.pld_name.txt i }
        ; generate_pat (i + 1) tail
        ]
  in
  let arg_pattern = generate_pat 0 fields in
  pexp_fun
    ~loc
    Nolabel
    None
    arg_pattern
    (pexp_record
       ~loc
       (List.mapi
          (fun i (field : label_declaration) ->
             ( { loc = field.pld_loc; txt = lident field.pld_name.txt }
             , ident ~loc:field.pld_loc (Printf.sprintf "%s_%d" field.pld_name.txt i) ))
          fields)
       None)
;;

type field_gadt =
  { name : string
  ; conv : expression
  ; rest : expression
  }

let field_ident ~loc s = { loc; txt = lident s }
let string_constant ~loc s = pexp_constant ~loc (Pconst_string (s, loc, None))

let sexp_conv_field ~loc (field : field_gadt) =
  pexp_construct
    ~loc
    { loc; txt = Modules.sexplib %. "Sexp_conv_record" %. "Field" }
    (Some
       (pexp_record
          ~loc
          [ field_ident ~loc "name", string_constant ~loc field.name
          ; ( field_ident ~loc "kind"
            , pexp_construct
                ~loc
                { loc; txt = Modules.sexplib %. "Sexp_conv_record" %. "Required" }
                None )
          ; field_ident ~loc "conv", field.conv
          ; field_ident ~loc "rest", field.rest
          ]
          None))
;;

let generate_fields ~loc (fields : label_declaration list) : expression =
  let rec aux = function
    | [] ->
      pexp_construct
        ~loc
        { loc; txt = Modules.sexplib %. "Sexp_conv_record" %. "Empty" }
        None
    | (field : label_declaration) :: tail ->
      sexp_conv_field
        ~loc
        { name = field.pld_name.txt
        ; rest = aux tail
        ; conv = desc_of_sexp ~loc:field.pld_loc field.pld_type.ptyp_desc
        }
  in
  aux fields
;;

let of_sexp_body ~loc (td : type_declaration) argument_name =
  match td.ptype_kind with
  | Ptype_variant constructors ->
    let matching_cases = List.map of_sexp_case_of_constructor constructors in
    let failing_case =
      case
        ~lhs:(ppat_any ~loc)
        ~guard:None
        ~rhs:(Embed_error.failwith ~loc (Printf.sprintf "%s_of_sexp error" argument_name))
    in
    pexp_match ~loc (ident ~loc argument_name) (matching_cases @ [ failing_case ])
  | Ptype_abstract ->
    (match td.ptype_manifest with
     | None -> Embed_error.failwith ~loc "Unsupported of_sexp for empty type"
     | Some t ->
       pexp_apply
         ~loc
         (desc_of_sexp ~loc t.ptyp_desc)
         [ Nolabel, ident ~loc argument_name ])
  | Ptype_record fields ->
    pexp_apply
      ~loc
      (dot_ident ~loc (Modules.sexplib %. "Sexp_conv_record") "record_of_sexp")
      [ Labelled "fields", generate_fields ~loc fields
      ; Labelled "index_of_field", generate_index_of_field ~loc fields
      ; Labelled "create", generate_create ~loc fields
      ; ( Labelled "caller"
        , string_constant ~loc (Printf.sprintf "%s_of_sexp" argument_name) )
      ; Labelled "allow_extra_fields", false_constant ~loc
      ; Nolabel, ident ~loc argument_name
      ]
  | Ptype_open -> Embed_error.failwith ~loc "Unsupported of_sexp for M.(t) type open"
;;

let generate_of_sexp (td : type_declaration) : structure_item =
  let t_of_sexp = Printf.sprintf "%s_of_sexp" td.ptype_name.txt in
  let loc = td.ptype_loc in
  pstr_value
    ~loc
    (type_recursivity td)
    [ { pvb_pat = ppat_var ~loc { loc; txt = t_of_sexp }
      ; pvb_loc = loc
      ; pvb_attributes = []
      ; pvb_constraint = None
      ; pvb_expr =
          (let argument_name = td.ptype_name.txt in
           pexp_fun
             ~loc
             Nolabel
             None
             (ppat_var ~loc { loc; txt = argument_name })
             (of_sexp_body ~loc td argument_name))
      }
    ]
;;

let generate_sexp_conv (td : type_declaration) : structure_item list =
  [ generate_sexp_of td; generate_of_sexp td ]
;;

let generate_impl ~ctxt (_rec_flag, (type_declarations : type_declaration list)) =
  ignore ctxt;
  List.concat_map generate_sexp_conv type_declarations
;;

let impl_generator = Deriving.Generator.V2.make_noarg generate_impl
let my_deriver = Deriving.add "sexp_light" ~str_type_decl:impl_generator
