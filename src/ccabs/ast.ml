open Sexplib.Conv


type label = string with sexp
type linkage_type = string with sexp
type string_literal = string list with sexp (* non-empty *)


type modifier = [
    (* decl flags *)
  | `UM_AUTO
  | `UM_REGISTER
  | `UM_STATIC
  | `UM_EXTERN
  | `UM_MUTABLE
  | `UM_INLINE
  | `UM_VIRTUAL
  | `UM_EXPLICIT
  | `UM_FRIEND
  | `UM_TYPEDEF

    (* cv-qualifier *)
  | `UM_CONST
  | `UM_VOLATILE
  | `UM_RESTRICT

    (* type keywords *)
  | `UM_WCHAR_T
  | `UM_BOOL
  | `UM_SHORT
  | `UM_INT
  | `UM_LONG
  | `UM_SIGNED
  | `UM_UNSIGNED
  | `UM_FLOAT
  | `UM_DOUBLE
  | `UM_VOID
  | `UM_LONG_LONG
  | `UM_CHAR
  | `UM_COMPLEX
  | `UM_IMAGINARY
] with sexp
type modifiers = modifier list with sexp


type cv_flag = [
  | `UM_CONST
  | `UM_VOLATILE
  | `UM_RESTRICT
] with sexp
type cv_flags = cv_flag list with sexp


type decl_flag = [
  | `UM_AUTO
  | `UM_REGISTER
  | `UM_STATIC
  | `UM_EXTERN
  | `UM_MUTABLE
  | `UM_INLINE
  | `UM_VIRTUAL
  | `UM_EXPLICIT
  | `UM_FRIEND
  | `UM_TYPEDEF
] with sexp
type decl_flags = decl_flag list with sexp


type simple_type_id =
  | ST_CHAR
  | ST_UNSIGNED_CHAR
  | ST_SIGNED_CHAR
  | ST_BOOL
  | ST_IMPLINT
  | ST_INT
  | ST_UNSIGNED_INT
  | ST_LONG_INT
  | ST_UNSIGNED_LONG_INT
  | ST_LONG_LONG
  | ST_UNSIGNED_LONG_LONG
  | ST_SHORT_INT
  | ST_UNSIGNED_SHORT_INT
  | ST_WCHAR_T
  | ST_FLOAT
  | ST_DOUBLE
  | ST_LONG_DOUBLE
  | ST_FLOAT_COMPLEX
  | ST_DOUBLE_COMPLEX
  | ST_LONG_DOUBLE_COMPLEX
  | ST_FLOAT_IMAGINARY
  | ST_DOUBLE_IMAGINARY
  | ST_LONG_DOUBLE_IMAGINARY
  | ST_VOID
  | ST_ELLIPSIS
  with sexp

type type_intr =
  | TI_STRUCT
  | TI_CLASS
  | TI_UNION
  | TI_ENUM
  with sexp


type unary_op =
  | UNY_PLUS
  | UNY_MINUS
  | UNY_NOT
  | UNY_BITNOT
  with sexp

type effect_op =
  | EFF_POSTINC
  | EFF_POSTDEC
  | EFF_PREINC
  | EFF_PREDEC
  with sexp

type binary_op =
  | BIN_EQUAL
  | BIN_NOTEQUAL
  | BIN_LESS
  | BIN_GREATER
  | BIN_LESSEQ
  | BIN_GREATEREQ
  | BIN_MULT
  | BIN_DIV
  | BIN_MOD
  | BIN_PLUS
  | BIN_MINUS
  | BIN_LSHIFT
  | BIN_RSHIFT
  | BIN_BITAND
  | BIN_BITXOR
  | BIN_BITOR
  | BIN_AND
  | BIN_OR
  | BIN_COMMA
  | BIN_MINIMUM
  | BIN_MAXIMUM
  | BIN_BRACKETS
  | BIN_ASSIGN
  | BIN_DOT_STAR
  | BIN_ARROW_STAR
  with sexp


type access_keyword =
  | AK_PUBLIC
  | AK_PROTECTED
  | AK_PRIVATE
  | AK_UNSPECIFIED
  with sexp


type cast_keyword =
  | CK_DYNAMIC
  | CK_STATIC
  | CK_REINTERPRET
  | CK_CONST
  with sexp


type overloadable_op =
    (* unary only *)
  | OP_NOT           (* ! *)
  | OP_BITNOT        (* ~ *)
  
    (* unary  both prefix and postfix; latter is declared as 2-arg function *)
  | OP_PLUSPLUS      (* ++ *)
  | OP_MINUSMINUS    (* -- *)
  
    (* unary or binary *)
  | OP_PLUS          (* + *)
  | OP_MINUS         (* - *)
  | OP_STAR          (* * *)
  | OP_AMPERSAND     (* & *)
  
    (* arithmetic *)
  | OP_DIV           (* / *)
  | OP_MOD           (* % *)
  | OP_LSHIFT        (* << *)
  | OP_RSHIFT        (* >> *)
  | OP_BITXOR        (* ^ *)
  | OP_BITOR         (* | *)
  
    (* arithmetic+assignment *)
  | OP_ASSIGN        (* = *)
  | OP_PLUSEQ        (* += *)
  | OP_MINUSEQ       (* -= *)
  | OP_MULTEQ        (* *= *)
  | OP_DIVEQ         (* /= *)
  | OP_MODEQ         (* %= *)
  | OP_LSHIFTEQ      (* <<= *)
  | OP_RSHIFTEQ      (* >>= *)
  | OP_BITANDEQ      (* &= *)
  | OP_BITXOREQ      (* ^= *)
  | OP_BITOREQ       (* |= *)
  
    (* comparison *)
  | OP_EQUAL         (* == *)
  | OP_NOTEQUAL      (* != *)
  | OP_LESS          (* < *)
  | OP_GREATER       (* > *)
  | OP_LESSEQ        (* <= *)
  | OP_GREATEREQ     (* >= *)
  
    (* logical *)
  | OP_AND           (* && *)
  | OP_OR            (* || *)
  
    (* arrows *)
  | OP_ARROW         (* -> *)
  | OP_ARROW_STAR    (* ->* *)

    (* misc *)
  | OP_BRACKETS      (* [] *)
  | OP_PARENS        (* () *)
  | OP_COMMA         (* , *)
  | OP_QUESTION      (* ?:  (not overloadable, but resolution used nonetheless) *)

    (* gcc extensions *)
  | OP_MINIMUM       (* <? *)
  | OP_MAXIMUM       (* >? *)
  with sexp


type translation_unit =
  top_form list

and top_form =
  | TF_ambig of top_form * top_form
  | TF_decl of declaration
  | TF_func of function_definition
  | TF_template of template_declaration
  | TF_explicitInst of decl_flags * declaration
  | TF_linkage of linkage_type * translation_unit
  | TF_one_linkage of linkage_type * top_form
  | TF_asm of string_literal
  | TF_namespaceDefn of string option * translation_unit
  | TF_namespaceDecl of namespace_decl

and function_definition = {
  fdflags : decl_flags;
  retspec : type_specifier;
  name_and_params : declarator;
  inits : member_init list;
  fbody : statement; (* TODO: restrict to S_compound *)
  handlers : handler list;
}

and member_init = {
  mname : pq_name;
  args : arg_expression;
}

and declaration = {
  dflags : decl_flags;
  spec : type_specifier;
  decllist : declarator list;
}

and type_id =
  | T_ambig of type_id * type_id
  | T_type of type_specifier * declarator

and pq_name =
  | PQ_ambig of pq_name * pq_name
  | PQ_qualifier of string option * template_argument list * pq_name
  | PQ_name of string
  | PQ_operator of operator_name * string
  | PQ_template of string * template_argument

and type_specifier =
  | TS_name of cv_flags * pq_name
  | TS_simple of cv_flags * simple_type_id
  | TS_elaborated of cv_flags * type_intr * pq_name
  | TS_classSpec of cv_flags * type_intr * pq_name * base_class_spec list * member list
  | TS_enumSpec of cv_flags * string * enumerator list

and base_class_spec = {
  is_virtual : bool;
  access : access_keyword;
  cname : pq_name;
}

and enumerator = {
  ename : string;
  expr : expression option;
}

and member =
  | MR_decl of declaration
  | MR_func of function_definition
  | MR_access of access_keyword
  | MR_usingDecl of pq_name
  | MR_template of template_declaration

and declarator =
  | DC_ambig of declarator * declarator
  | DC_decl of ideclarator * initialiser option

and ideclarator =
  | D_name of pq_name option
  | D_pointer of cv_flags * ideclarator
  | D_reference of cv_flags * ideclarator
  | D_func of ideclarator * type_id list * cv_flags * exception_spec option
  | D_array of ideclarator * expression option
  | D_bitfield of pq_name option * expression
  | D_ptrToMember of pq_name * cv_flags * ideclarator
  | D_grouping of ideclarator

and exception_spec =
  type_id list

and operator_name =
  | ON_new
  | ON_delete
  | ON_newArray
  | ON_deleteArray
  | ON_operator of overloadable_op
  | ON_conversion of type_id

and statement =
  | S_ambig of statement * statement
  | S_skip
  | S_label of label * statement
  | S_case of expression * statement
  | S_default of statement
  | S_expr of expression
  | S_compound of statement list
  | S_if of condition * statement * statement
  | S_switch of condition * statement
  | S_while of condition * statement
  | S_doWhile of statement * expression
  | S_for of statement * condition * expression * statement
  | S_break
  | S_continue
  | S_return of expression option
  | S_goto of label
  | S_decl of declaration
  | S_try of statement * handler list
  | S_asm of string_literal
  | S_namespaceDecl of namespace_decl

and condition =
  | CN_ambig of condition * condition
  | CN_expr of expression
  | CN_decl of type_id

and handler = {
  type_id : type_id;
  hbody : statement;
}

and expression =
  | E_ambig of expression * expression
  | E_boolLit of bool
  | E_intLit of string
  | E_floatLit of string
  | E_stringLit of string_literal
  | E_charLit of string
  | E_this
  | E_variable of pq_name
  | E_funCall of expression * arg_expression list
  | E_constructor of type_specifier * arg_expression list
  | E_fieldAcc of expression * pq_name
  | E_unary of unary_op * expression
  | E_effect of effect_op * expression
  | E_binary of expression * binary_op * expression
  | E_addrOf of expression
  | E_deref of expression
  | E_cast of type_id * expression
  | E_cond of expression * expression * expression
  | E_sizeofExpr of expression
  | E_sizeofType of type_id
  | E_assign of expression * binary_op * expression
  | E_new of bool * arg_expression list * type_id * arg_expression list option
  | E_delete of bool * bool * expression
  | E_throw of expression option
  | E_keywordCast of cast_keyword * type_id * expression
  | E_typeidExpr of expression
  | E_typeidType of type_id
  | E_grouping of expression
  | E_arrow of expression * pq_name

and arg_expression =
  | AE_ambig of arg_expression * arg_expression
  | AE_expr of expression

and initialiser =
  | IN_expr of expression
  | IN_compound of initialiser list
  | IN_ctor of arg_expression list

and template_declaration =
  | TD_func of template_parameter list * function_definition
  | TD_decl of template_parameter list * declaration
  | TD_tmember of template_parameter list * template_declaration

and template_parameter =
  | TP_ambig of template_parameter * template_parameter
  | TP_type of string option * type_id option
  | TP_nontype of type_id
  | TP_template of template_parameter list * string option * pq_name option

and template_argument =
  | TA_ambig of template_argument * template_argument
  | TA_type of type_id
  | TA_nontype of expression

and namespace_decl =
  | ND_alias of string * pq_name
  | ND_usingDecl of pq_name
  | ND_usingDir of pq_name

  (* all of the above gets sexp *)
  with sexp
