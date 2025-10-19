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

module Embed_error = struct
  let exp ~loc msg = pexp_extension ~loc (Location.error_extensionf ~loc msg)

  let failwith ~loc msg =
    pexp_apply
      ~loc
      (pexp_ident ~loc { loc; txt = lident "failwith" })
      [ Nolabel, pexp_constant ~loc (Pconst_string (msg, loc, None)) ]
  ;;
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

let generate_sexp_of (td : type_declaration) : structure_item =
  let sexp_of_t = Printf.sprintf "sexp_of_%s" td.ptype_name.txt in
  let loc = td.ptype_loc in
  pstr_value
    ~loc
    Nonrecursive
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

let of_sexp_body ~loc (td : type_declaration) argument_name =
  match td.ptype_kind with
  | Ptype_variant _ ->
    Embed_error.failwith ~loc (Printf.sprintf "Not implemented: %s_of_sexp" argument_name)
  | Ptype_abstract -> Embed_error.failwith ~loc "Unsupported of_sexp for abstract types"
  | Ptype_record _ -> Embed_error.failwith ~loc "Unsupported of_sexp for record types"
  | Ptype_open -> Embed_error.failwith ~loc "Unsupported of_sexp for M.(t) type open"
;;

let generate_of_sexp (td : type_declaration) : structure_item =
  let t_of_sexp = Printf.sprintf "%s_of_sexp" td.ptype_name.txt in
  let loc = td.ptype_loc in
  pstr_value
    ~loc
    Nonrecursive
    [ { pvb_pat = ppat_var ~loc { loc; txt = t_of_sexp }
      ; pvb_loc = loc
      ; pvb_attributes = []
      ; pvb_constraint = None
      ; pvb_expr =
          (let argument_name = Printf.sprintf "_%s" td.ptype_name.txt in
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
