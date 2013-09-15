open Sexplib.Conv


type label = string with sexp
type linkage_type = string with sexp
type string_literal = string list with sexp (* non-empty *)


type simple_type_id =
  | ST_Char
  | ST_UChar
  | ST_SChar

  | ST_SInt
  | ST_UInt

  | ST_SIntN of int
  | ST_UIntN of int

  | ST_SLong
  | ST_ULong

  | ST_SLLong
  | ST_ULLong

  | ST_SShort
  | ST_UShort

  | ST_Float
  | ST_Double
  | ST_LDouble

  | ST_CFloat
  | ST_CDouble
  | ST_CLDouble

  | ST_IFloat
  | ST_IDouble
  | ST_ILDouble

  | ST_ImplInt
  | ST_Bool
  | ST_WCharT

  | ST_Void
  | ST_Ellipsis

  | ST_CDtor
  with sexp

type type_intr =
  | TI_Struct
  | TI_Class
  | TI_Union
  | TI_Enum
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
  | BIN_MUL
  | BIN_DIV
  | BIN_MOD
  | BIN_ADD
  | BIN_SUB
  | BIN_LSH
  | BIN_RSH
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
  | BIN_DSTAR
  | BIN_ASTAR
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
  | OP_LSH           (* "<<" *)
  | OP_RSH           (* ">>" *)
  | OP_BITXOR        (* ^ *)
  | OP_BITOR         (* | *)
  
    (* arithmetic+assignment *)
  | OP_ASSIGN        (* = *)
  | OP_ADDEQ         (* += *)
  | OP_SUBEQ         (* -= *)
  | OP_MULEQ         (* *= *)
  | OP_DIVEQ         (* /= *)
  | OP_MODEQ         (* %= *)
  | OP_LSHEQ         (* "<<=" *)
  | OP_RSHEQ         (* ">>=" *)
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


(* ---------------- file ------------- *)
(* an entire file (with #included stuff) of toplevel forms *)
type translation_unit =
  top_form list

(* a toplevel form *)
and top_form =
  (* represent ambiguous topforms by forming a linked list of
   * alternatives; the only reason topforms might be ambiguous is
   * because of implicit int (for old-style C) *)
  | TF_ambig of top_form * top_form
  (* includes function prototypes *)
  | TF_decl of (*decl*)declaration
  (* functions with bodies *)
  | TF_func of (*f*)function_definition
  (* template functions or template classes *)
  | TF_template of (*td*)template_declaration
  (* explicit instantiation request; 'instFlags' is for the GNU
   * "extern template" extension, and is always DF_NONE for ANSI C++ *)
  | TF_explicitInst of (*instFlags*)decl_flags * (*d*)declaration
  (* linkage specification enclosing a bunch of forms, e.g.
   * extern "C" { /*...*/ } *)
  | TF_linkage of (*linkageType*)linkage_type * (*forms*)translation_unit
  (* linkage spec with one form and no braces; it turns out this has
   * different semantics [cppstd 7.5 para 7] *)
  | TF_one_linkage of (*linkageType*)linkage_type * (*form*)top_form
  (* assembly directive at toplevel *)
  | TF_asm of (*text*)string_literal
  (* namespace definition [cppstd 7.3.1] *)
  | TF_namespaceDefn of (*name*)string option * (*forms*)translation_unit
  (* one of three namespace-related declarations that can occur
   * at toplevel or as a function statement *)
  | TF_namespaceDecl of (*decl*)namespace_decl


(* ----------------------- function ------------------------- *)
(* a function definition (toplevel or class member) *)
and function_definition = {
  (* static, extern, etc. *)
  fdflags : decl_flags;
  (* type specifier for return value *)
  retspec : type_specifier;
  (* 1. remainder of return value type
   * 2. name of function
   * 3. names/types of parameters *)
  name_and_params : declarator;
  (* (for ctors only) member initialisation list *)
  inits : member_init list;
  (* body of function *)
  fbody : statement; (* TODO: restrict to S_compound *)
  (* handlers for ctor "try" block *)
  handlers : handler list;
}

and member_init = {
  (* name of member or base class *)
  mname : pq_name;
  (* arguments to its constructor *)
  args : arg_expression list;
}


(* --------------- types and declarators --------------- *)
(* variable declaration or definition, or function declaration *)
and declaration = {
  (* typedef, virtual, extern, etc. *)
  dflags : decl_flags;
  (* e.g. "int" *)
  spec : type_specifier;
  (* e.g. "x=3, y" *)
  decllist : declarator list;
}

(* just one complete type; appears in parameter decls and in casts; the
 * main difference between an ASTTypeId and a Declaration is that the
 * former can have only one declarator, while the latter can have several *)
and type_id =
  | T_ambig of type_id * type_id
  | T_type of (*spec*)type_specifier (* "int" *)
            * (*decl*)declarator (* this will be abstract sometimes (e.g. casts) *)

(* a name with optional class qualifiers (PQ: "possibly qualified");
 * if the first qualifier's 'qualifier' name is NULL, then it means
 * there was a leading "::" symbol; each level of qualification has a
 * 'loc' because each level is a remote reference to some entity, and
 * that lookup might fail *)
and pq_name =
  | PQ_ambig of pq_name * pq_name
  (* outer qualifier applied to some inner PQName, plus an optional
   * list of template arguments to the qualifier *)
  | PQ_qualifier of (*qualifier*)string option * (*templArgs*)template_argument_list * (*rest*)pq_name
  (* final name, when it's an ordinary identifier
   * NOTE: 'name' here is *never* NULL--instead, I use NULL
   * PQName pointers in abstract declarators *)
  | PQ_name of (*name*)string
  (* "operator" names; 'o' has the full info *)
  | PQ_operator of (*o*)operator_name
  (* template instances: a template function or class name, plus
   * some template arguments *)
  | PQ_template of (*name*)string * (*templArgs*)template_argument_list

(* a name of an "atomic" type--one to which type constructors
 * (e.g. '*') can be applied, but which itself is not syntactically
 * built with type constructors (typedef'd types may have been built
 * with type constructors when the name was defined, but a
 * TypeSpecifier refers only to the name) *)
and type_specifier =
  (* a typedef'd name (typedef might be implicit, as for classes);
   * if 'typenamedUsed' is true, then the user said "typename", so
   * we don't regard an error as disambiguating *)
  | TS_name of (*cv*)cv_flags * (*name*)pq_name * (*typenameUsed*)bool
  (* int or char or float or .. *)
  | TS_simple of (*cv*)cv_flags * (*stype*)simple_type_id
  (* "class Foo" *)
  | TS_elaborated of (*cv*)cv_flags * (*keyword*)type_intr * (*name*)pq_name
  (* class/struct/union definition *)
  | TS_classSpec of (*cv*)cv_flags
  		  * (*keyword*)type_intr (* "class", "struct", "union" *)
                  
                    (* 'name' is the user-provided name, if any.
                     * Why is this a PQName instead of just a string?
                     *   - it could be a template specialisation, and therefore
                     *     has template arguments
                     *   - it could be a definition or specialisation of a class
                     *     declared in another namespace
                     * See cppstd 14.5.4 para 6 for an example of both at once. *)
                  * (*name*)pq_name option
                  * (*bases*)base_class_spec list (* base classes *)
                  * (*members*)member list (* field and methods of the class *)
  (* "enum { ... }" *)
  | TS_enumSpec of (*cv*)cv_flags
  		 * (*name*)string option (* name of enum, if any *)
                 * (*elts*)enumerator list (* elements of the enumeration *)

  | TS_typeof of (*cv*)cv_flags * (*atype*)typeof

(* base class specification *)
and base_class_spec = {
  is_virtual : bool; (* true for virtual base classes *)
  access : access_keyword; (* public/protected/private *)
  cname : pq_name; (* name of base class *)
}

(* a binding of a name to a constant value *)
and enumerator = {
  ename : string; (* name of this constant *)
  eexpr : expression option; (* constant expr, or None for "next" *)
}

(* member of a class *)
and member =
  (* data members or functions w/o bodies *)
  | MR_decl of (*d*)declaration
  (* function with body *)
  | MR_func of (*f*)function_definition
  (* section header *)
  | MR_access of (*k*)access_keyword
  (* namespace "using" declaration *)
  | MR_usingDecl of (*decl*)pq_name
  (* member template... *)
  | MR_template of (*d*)template_declaration

(* Syntactically, a Declarator introduces a name of a declared thing,
 * and also optionally adds type constructors to the base type of the
 * specifier.  It may have an initializing expression, depending on
 * the context. *)
and declarator =
  (* ambiguity representation *)
  | DC_ambig of declarator * declarator
  | DC_decl of (*decl*)ideclarator (* syntax of type designation *)
  	     * (*init*)initialiser option (* optional data initialiser *)

(* inner declarator; things *recursively* buried inside declarators;
 * cannot have initialisers; the internal structure can be ignored
 * once typechecking determines what type is denoted *)
and ideclarator =
  (* "x" (None means abstract declarator or anonymous parameter);
   * this is used for ctors and dtors as well as ordinary names
   * (dtor names start with "~"); it's also used for operator names *)
  | D_name of (*name*)pq_name option
  (* "*x" (as in "int *x") *)
  | D_pointer of (*cv*)cv_flags (* optional qualifiers applied to ptr type *)
  	       * (*base*)ideclarator
  (* "&x" *)
  | D_reference of cv_flags * (*base*)ideclarator
  (* "f(int)" *)
  | D_func of (*base*)ideclarator (* D_name of function, typically *)
  	    * (*params*)type_id list (* params with optional default values *)
            * (*cv*)cv_flags (* optional "const" for member functions *)
            * (*exnSpec*)exception_spec option (* throwable exceptions *)
  (* "a[5]" or "b[]" *)
  | D_array of (*base*)ideclarator * (*size*)expression option
  (* "c : 2"
   *
   * I use a PQName here instead of a string for uniformity
   * (so every IDeclarator ends with a PQName); there are never
   * qualifiers on a bitfield name *)
  | D_bitfield of (*name*)pq_name option * (*bits*)expression
  (* "X::*p" *)
  | D_ptrToMember of (*nestedName*)pq_name * (*cv*)cv_flags * (*base*)ideclarator
  (* declarator grouping operator: it's semantically irrelevant
   * (i.e. equivalent to just 'base' alone), but plays a role in
   * disambiguation *)
  | D_grouping of (*base*)ideclarator
  (* __attribute__ occurring directly after a declarator *)
  | D_attribute of (*base*)ideclarator * (*alist*)attribute list

(* specification of what a function can throw; if an exception_spec option
 * pointer is None, it means there is no specification, i.e. anything
 * can be thrown; if the exception_spec is empty, nothing can be thrown *)
and exception_spec =
  (* (possibly empty) list of allowable types *)
  type_id list

(* names for operator and conversion functions *)
and operator_name =
  (* operator new & delete *)
  | ON_new
  | ON_delete
  | ON_newArray
  | ON_deleteArray
  (* arithmetic-type operator *)
  | ON_operator of (*op*)overloadable_op
  (* conversion operator to convert to 'type'; type will always have an
   * abstract declarator, with only pointer-type constructors (if any) *)
  | ON_conversion of (*typ*)type_id

(* ------------------- statements ----------------- *)
and statement =
  (* represent ambiguous statements by forming a linked list of alternatives *)
  | S_ambig of statement * statement
  (* no-op; used whenever optional Statement is not present *)
  | S_skip
  | S_label of (*name*)label * (*s*)statement
  | S_case of (*expr*)expression * (*s*)statement
  | S_default of (*s*)statement
  | S_expr of (*expr*)full_expression (* expression evaluated for side effect *)
  | S_compound of (*stmts*)statement list
  | S_if of (*cond*)condition * (*thenBranch*)statement * (*elseBranch*)statement
  | S_switch of (*cond*)condition * (*branches*)statement
  | S_while of (*cond*)condition * (*body*)statement
  | S_doWhile of (*body*)statement * (*expr*)full_expression (* note: 'expr' is not a condition *)
  | S_for of (*init*)statement * (*cond*)condition * (*after*)full_expression * (*body*)statement
  | S_break
  | S_continue
  | S_return of (*expr*)full_expression option
  | S_goto of (*target*)label
  | S_decl of (*decl*)declaration
  | S_try of (*body*)statement * (*handlers*)handler list
  | S_asm of (*text*)string_literal
  | S_namespaceDecl of (*decl*)namespace_decl

(* condition expression in a control-flow statement; it's allowed
 * to declare a variable that will hold the condition's value for
 * the duration of the substatement(s) *)
and condition =
  (* ambiguity representation *)
  | CN_ambig of condition * condition
  (* simple expression *)
  | CN_expr of (*expr*)full_expression
  (* type, name, & initialiser (must all be present) *)
  | CN_decl of (*typeId*)type_id

(* exception handler *)
and handler = {
  (* type of exception objects this handler catches; note that it
   * might be ST_Ellipsis, which corresponds to the "..." syntax *)
  type_id : type_id;
  (* code to run when handler catches an exception *)
  hbody : statement;
}

(* ----------------- expressions ----------------- *)
(* NOTE: we never make lists of Expressions, only of ArgExpressions *)
and expression =
  | E_ambig of expression * expression
  | E_boolLit of (*b*)bool
  (* most of the literals are handled by simply storing the textual
   * representation the user typed in, including delimiters like
   * quotes; the tcheck pass could interpret those if desired (and
   * does in some cases); concatenation of string literals is handled
   * by making a list *)
  | E_intLit of (*text*)string
  | E_floatLit of (*text*)string
  | E_stringLit of (*text*)string_literal
  | E_charLit of (*text*)string
  (* reference to 'this', the address of the receiver object *)
  | E_this
  (* variable reference *)
  | E_variable of pq_name
  | E_funCall of (*func*)expression * (*args*)arg_expression list
  (* call to constructor as an expression; the expression's overall
   * type says which type is being constructed *)
  | E_constructor of (*spec*)type_specifier * (*args*)arg_expression list
  (* field within an object; as a special case, fieldName might begin
   * with "~", meaning we're naming the destructor *)
  | E_fieldAcc of (*obj*)expression * (*fieldName*)pq_name
  | E_unary of (*op*)unary_op * (*expr*)expression
  | E_effect of (*op*)effect_op * (*expr*)expression
  | E_binary of (*left*)expression * (*op*)binary_op * (*right*)expression
  | E_addrOf of (*expr*)expression
  | E_deref of (*expr*)expression
  | E_cast of (*ctype*)type_id * (*expr*)expression
  | E_cond of (*cond*)expression * (*thenExpr*)expression * (*elseExpr*)expression
  | E_sizeofExpr of (*expr*)expression
  | E_sizeofType of (*atype*)type_id
  (* this is a simple assignment if op==BIN_ASSIGN, otherwise it's an
   * incremental assignment, like a += 3 (e.g. for op==BIN_PLUS) *)
  | E_assign of (*target*)expression * (*op*)binary_op * (*source*)expression
  | E_new of (*global*)bool (* true if "::" preceeds "new" *)
  	   * (*placementArgs*)arg_expression list (* arguments to placement-new (empty if no placement syntax) *)
           * (*atype*)type_id (* type to allocate *)
           * (*ctorArgs*)arg_expression list option (* arguments to type's constructor (None if no ctor call syntax *)
  | E_delete of (*global*)bool (* true if "::" preceeds "delete" *)
              * (*array*)bool (* true if "[]" follows "delete" *)
              * (*expr*)expression (* address of obj to deallocate *)
  | E_throw of (*expr*)expression option
  | E_keywordCast of (*keyword*)cast_keyword (* dynamic_cast, static_cast, etc. *)
  		   * (*ctype*)type_id (* type to cast to *)
                   * (*expr*)expression (* expression being cast *)
  | E_typeidExpr of (*expr*)expression
  | E_typeidType of (*ttype*)type_id
  (* Both of the following exist only between parsing and
   * type-checking.  The type checker discards them.
   *
   * an E_grouping is a pair of grouping parentheses; it's present in
   * the AST for syntactic disambiguation of angle brackets, and an
   * obscure rule about pointer-to-member *)
  | E_grouping of (*expr*)expression
  (* retained explicitly for possible operator overloading *)
  | E_arrow of expression * pq_name

  (* gcc statement expression
   * http://gcc.gnu.org/onlinedocs/gcc-3.1/gcc/Statement-Exprs.html
   *
   * statement can only be S_compound.
   *)
  | E_statement of statement
  (* gcc compound literals
   * http://gcc.gnu.org/onlinedocs/gcc-3.1/gcc/Compound-Literals.html
   * and C99 6.5.2.5
   *
   * init can only be IN_compound.
   *)
  | E_compoundLit of (*stype*)type_id * (*init*)initialiser
  (* miscellanous builtins that for whatever reason find themselves
   * as AST nodes instead of ordinary function calls (...)
   * http://gcc.gnu.org/onlinedocs/gcc-3.1/gcc/Other-Builtins.html *)
  | E___builtin_constant_p of (*expr*)expression
  | E___builtin_va_arg of (*expr*)expression * (*atype*)type_id
  (* alignment inquiry
   * http://gcc.gnu.org/onlinedocs/gcc-3.1/gcc/Alignment.html *)
  | E_alignofType of (*atype*)type_id
  | E_alignofExpr of (*expr*)expression
  (* conditional with no middle operand
   * http://gcc.gnu.org/onlinedocs/gcc-3.1/gcc/Conditionals.html *)
  | E_gnuCond of (*cond*)expression * (*elseExpr*)expression
  | E_offsetof of (*atype*)type_id * (*fieldName*)expression

(* maximal expressions: the parent is not an expression
 * (cppstd 1.9 para 12) *)
and full_expression =
  | FullExpression of (*expr*)expression

and arg_expression =
  | AE_ambig of arg_expression list * arg_expression list
  (* expression in an argument list *)
  | AE_expr of (*expr*)expression

(* things that appear after declarations to assign initial values *)
and initialiser =
  (* simple initialiser, like "int x = 3" *)
  | IN_expr of (*e*)expression
  (* compound initialiser, like "int x[4] = { 1,2,3,4 };" *)
  | IN_compound of (*inits*)initialiser list
  (* constructor initialiser, like "int x(3);" *)
  | IN_ctor of (*args*)arg_expression list

(* ------------------- templates ------------------- *)
(* wrap some template parameters on a declaration or function *)
and template_declaration =
  (* define a template function *)
  | TD_func of (*params*)template_parameter_list * (*f*)function_definition
  (* declare a template function prototype, or declare or define a
   * template class *)
  | TD_decl of (*params*)template_parameter_list * (*d*)declaration
  (* wrap another set of parameters around a template decl;
   * this is for template members of template classes (14.5.2) *)
  | TD_tmember of (*params*)template_parameter_list * (*d*)template_declaration

(* one of the parameters to a template declaration *)
and template_parameter =
  | TP_ambig of template_parameter * template_parameter
  (* type parameter; when present, 'name' is what the template code
   * will use to refer to the actual argument type; when present,
   * 'defaultType' provides a default argument
   *
   * 'name' can be None after parsing, but the type checker sticks
   * in a synthesised name, so after tchecking it is never None *)
  | TP_type of (*name*)string option * (*defaultType*)type_id option * (*next*)template_parameter_list
  (* non-type parameters *)
  | TP_nontype of (*param*)type_id * (*next*)template_parameter_list
  (* template template parameter *)
  | TP_template of (*parameters*)template_parameter
  		 * (*name*)string option
                 * (*defaultTemplate*)pq_name option
                 * (*next*)template_parameter_list

(* the list is an intrusive linked list in template_parameter *)
and template_parameter_list =
  template_parameter option

(* one of the arguments to a template instantiation *)
and template_argument =
  | TA_ambig of template_argument * template_argument
  (* type argument, corresponds to a TP_type parameter *)
  | TA_type of (*type*)type_id * (*next*)template_argument_list
  (* non-type arguments, corresponds to a TP_nontype parameter *)
  | TA_nontype of (*expr*)expression * (*next*)template_argument_list
  (* This is a special element that, when found in the template
   * argument list of a PQ_qualifier or PQ_template, signals that
   * those names were prefixed by the "template" keyword (14.2 para
   * 4).  Doing it this way, instead of adding a boolean to the PQ_*
   * classes, saves a little space in the common case, and avoids
   * cluttering creation sites with "false /*templateUsed*/". *)
  | TA_templateUsed of (*next*)template_argument_list

and template_argument_list =
  template_argument option

(* -------------------- namespace declarations ---------------------- *)
(* since each of these three forms can appear either at toplevel
 * (or in a namespace) or inside a function, I've collected them
 * together; also, ND_usingDecl can appear as a class member *)
and namespace_decl =
  (* [cppstd 7.3.2] namespace alias definition: defines 'alias'
   * to refer to the same namespace as 'original' *)
  | ND_alias of (*alias*)string * (*original*)pq_name
  (* [cppstd 7.3.3] using declaration: the given name's unqualified
   * form is imported into the current scope as an alias for the named
   * entity; 'name' must be qualified *)
  | ND_usingDecl of (*name*)pq_name
  (* [cppstd 7.3.4] using directive: names from the given namespace
   * are accessible in the current scope without qualification, among
   * other effects (of the three NamespaceDecls, this is the most
   * complicated) *)
  | ND_usingDir of (*name*)pq_name


(* ----------- gcc attributes ----------- *)
(* one attribute, somewhere inside __attribute((...)); syntactically,
 * attributes are separated by commas in the parens *)
and attribute =
  (* e.g., __attribute__(( ))
   *                      ^
   * I decided to keep these even though the GNU documentation states
   * that they are always ignored, because it would be a bit awkward
   * to drop them in the parser, and they shouldn't occur frequently
   * anyway. *)
  | AT_empty
  (* e.g., __attribute__((packed))
   *                      ^^^^^^
   * Note that even C++ keywords will just get stored as strings. *)
  | AT_word of string
  (* e.g., __attribute__((format(printf, 1, 2)))
   *                      ^^^^^^^^^^^^^^^^^^^^
   * the 'args' can be empty to indicate an empty list (which
   * is still different from AT_word) *)
  | AT_func of string * arg_expression list


(* ----------- gcc typeof ----------- *)
(* types denoted with 'typeof' keyword, also used for decltype *)
and typeof =
  | TO_ambig of typeof * typeof
  | TO_expr of (*expr*)full_expression
  | TO_type of (*atype*)type_id


(* ----------- END OF AST ----------- *)
and modifier = [
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
  | `UM_CHAR
  | `UM_COMPLEX
  | `UM_IMAGINARY
  (* special __int128 and such *)
  | `UM_INT_N of int

  (* GCC Attributes *)
  | `UM_ATTRIB of attribute list
]
and modifiers = modifier list


and type_flag = [
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
  | `UM_CHAR
  | `UM_COMPLEX
  | `UM_IMAGINARY
  | `UM_INT_N of int
]
and type_flags = type_flag list


and cv_flag = [
  | `UM_CONST
  | `UM_VOLATILE
  | `UM_RESTRICT
  | `UM_ATTRIB of attribute list
]
and cv_flags = cv_flag list


and decl_flag = [
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
  | `UM_ATTRIB of attribute list
]
and decl_flags = decl_flag list


  (* all of the above gets sexp *)
  with sexp
