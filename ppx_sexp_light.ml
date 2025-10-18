open Ppxlib
open Ast_builder.Default

let case_of_indexed_name ~loc i name =
  case
    ~lhs:(ppat_constant ~loc (Pconst_string (name, loc, None)))
    ~guard:None
    ~rhs:(pexp_constant ~loc (Pconst_integer (string_of_int i, None)))
;;

let generate_index_of_field ~loc names =
  pexp_function_cases ~loc (List.mapi (case_of_indexed_name ~loc) names @ [])
;;

let generate_sexp_of (td : type_declaration) : structure_item list =
  let sexp_of_t = Printf.sprintf "sexp_of_%s" td.ptype_name.txt in
  match td.ptype_kind with
  | Ptype_record _fields ->
    let loc = td.ptype_loc in
    [ pstr_value
        ~loc
        Nonrecursive
        [ { pvb_pat = ppat_var ~loc { loc; txt = sexp_of_t }
          ; pvb_loc = loc
          ; pvb_attributes = []
          ; pvb_constraint = None
          ; pvb_expr =
              pexp_fun
                ~loc
                Nolabel
                None
                (ppat_var ~loc { loc; txt = td.ptype_name.txt })
                (pexp_apply
                   ~loc
                   (pexp_ident ~loc { loc; txt = lident "sexp_of_string" })
                   [ Nolabel, pexp_constant ~loc (Pconst_string ("atom", loc, None)) ])
          }
        ]
    ]
  | _ -> failwith "Unsupported type kind"
;;

let generate_impl ~ctxt (_rec_flag, (type_declarations : type_declaration list)) =
  ignore ctxt;
  List.concat_map generate_sexp_of type_declarations
;;

let impl_generator = Deriving.Generator.V2.make_noarg generate_impl
let my_deriver = Deriving.add "sexp_light" ~str_type_decl:impl_generator
