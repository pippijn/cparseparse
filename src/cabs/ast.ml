open Sexplib.Conv

(** The C abstract syntax definition. *)

(** {6 Basic types during construction in parser} *)

type partial_basic_type =
  | BT_Default		(** default-int *)

  (* Signedness *)
  | BT_Signed		(** [signed] *)
  | BT_Unsigned		(** [unsigned] *)

  (* Integer modifiers "short int", "long int", "long long int" *)
  | BT_Short		(** [short] *)
  | BT_Long		(** [long] *)

  (* int and sized integral types *)
  | BT_Int		(** [int] *)
  | BT_IntN of int	(** [__intNN] *)

  (* Other integral types *)
  | BT_Bool		(** [_Bool] *)
  | BT_Char		(** [char] *)
  | BT_WCharT		(** [wchar_t] *)

  (* Floating point types *)
  | BT_Float		(** [float] *)
  | BT_Double		(** [double] *)

  (* Sized floating point types *)
  | BT_FloatN of int	(** [__floatNN] *)
  | BT_DecimalN of int	(** [_DecimalNN] *)

  (* Other built-in types *)
  | BT_VaList		(** [__builtin_va_list] *)
  | BT_Ellipsis		(** [...] *)
  | BT_Void		(** [void] *)
  with sexp


(** {6 Basic types after normalisation} *)

type basic_type =
  (* Integral types. *)
  | Bool		(** [_Bool] *)
  | Char		(** [char] *)
  | SChar		(** [signed char] *)
  | UChar		(** [unsigned char] *)
  | SShort		(** [signed short] *)
  | UShort		(** [unsigned short] *)
  | SInt		(** [signed int] *)
  | UInt		(** [unsigned int] *)
  | SLong		(** [signed long] *)
  | ULong		(** [unsigned long] *)
  | SLongLong		(** [signed long long] *)
  | ULongLong		(** [unsigned long long] *)

  (* Floating point types. *)
  | Float		(** [float] *)
  | Double		(** [double] *)
  | LongDouble		(** [long double] *)

  (* Sized integral types. *)
  | SIntN of int	(** [signed __intNN] *)
  | UIntN of int	(** [unsigned __intNN] *)

  (* Sized floating point types. *)
  | FloatN of int	(** [__floatNN] *)
  | DecimalN of int	(** [_DecimalNN] *)

  (* Other built-in types *)
  | VaList		(** [__builtin_va_list] *)
  | Ellipsis		(** [...] *)
  | Void		(** [void] *)
  | WCharT		(** [wchar_t] *)
  with sexp


(** {6 User defined type tag kinds} *)

type sue_kind =
  | SUE_Struct		(** [struct] *)
  | SUE_Union		(** [union] *)
  | SUE_Enum		(** [enum] *)
  with sexp


(** {6 Numeric literal kinds} *)

type integer_literal_kind =
  (* Integer *)
  | LIT_Bin		(** [0b01011] *)
  | LIT_Dec		(** [12345] *)
  | LIT_Hex		(** [0xdeadbeef] *)
  | LIT_Oct		(** [0755] *)
  with sexp


type floating_literal_kind =
  (* Floating point *)
  | LIT_Float		(** [1.0e+24] *)
  | LIT_HexFloat	(** [0x1.0p-500] *)
  with sexp


(** {6 Character literal kinds} *)

type char_literal_kind =
  (* Character *)
  | LIT_Char		(** ['c'] *)
  | LIT_WChar		(** [L'c'] *)
  with sexp


(** {6 String literal kinds} *)

type string_literal_kind =
  | LIT_String		(** ["string"] *)
  | LIT_WString		(** [L"string"] *)
  with sexp


(** {6 Ternary operators} *)

type ternary_operator =
  (* Conditional expressions *)
  | OP_Conditional	(** [a ? b : c] *)
  with sexp


(** {6 Binary operators} *)

type binary_operator =
  (* Assignment expressions *)
  | OP_Assign		(** "[a = b]" *)
  | OP_MultiplyAssign	(** "[a *= b]" *)
  | OP_DivideAssign	(** "[a /= b]" *)
  | OP_ModuloAssign	(** "[a %= b]" *)
  | OP_AddAssign	(** "[a += b]" *)
  | OP_SubtractAssign	(** "[a -= b]" *)
  | OP_ShiftLeftAssign	(** "[a <<= b]" *)
  | OP_ShiftRightAssign	(** "[a >>= b]" *)
  | OP_BitwiseAndAssign	(** "[a &= b]" *)
  | OP_BitwiseXorAssign	(** "[a ^= b]" *)
  | OP_BitwiseOrAssign	(** "[a |= b]" *)

  (* Binary expressions *)
  | OP_Multiply		(** "[a * b]" *)
  | OP_Divide		(** "[a / b]" *)
  | OP_Modulo		(** "[a % b]" *)
  | OP_Add		(** "[a + b]" *)
  | OP_Subtract		(** "[a - b]" *)
  | OP_ShiftLeft	(** "[a << b]" *)
  | OP_ShiftRight	(** "[a >> b]" *)
  | OP_Less		(** "[a < b]" *)
  | OP_Greater		(** "[a > b]" *)
  | OP_LessEqual	(** "[a <= b]" *)
  | OP_GreaterEqual	(** "[a >= b]" *)
  | OP_Equal		(** "[a == b]" *)
  | OP_NotEqual		(** "[a != b]" *)
  | OP_BitwiseAnd	(** "[a & b]" *)
  | OP_BitwiseXor	(** "[a ^ b]" *)
  | OP_BitwiseOr	(** "[a | b]" *)
  | OP_LogicalAnd	(** "[a && b]" *)
  | OP_LogicalOr	(** "[a || b]" *)
  | OP_Ellipsis		(** "[a ... b]" *)
  | OP_Comma		(** "[a , b]" *)
  with sexp


(** {6 Unary operators} *)

type unary_operator =
  (* Unary expressions *)
  | OP_PostIncrement	(** "[a++]" *)
  | OP_PostDecrement	(** "[a--]" *)
  | OP_PreIncrement	(** "[++a]" *)
  | OP_PreDecrement	(** "[--a]" *)
  | OP_AddressOf	(** "[&a]" *)
  | OP_AddressOfLabel	(** "[&&a]" *)
  | OP_Dereference	(** "[*a]" *)
  | OP_Identity		(** "[+a]" *)
  | OP_Negate		(** "[-a]" *)
  | OP_BitwiseNot	(** "[~a]" *)
  | OP_LogicalNot	(** "[!a]" *)
  | OP_Imag		(** "[imag a]" *)
  | OP_Real		(** "[real a]" *)
  with sexp


(** {6 Pseudo operators} *)

type pseudo_operator =
  | OP_HighestPrecedence	(** Pseudo-operator with a precedence higher than all other operators. *)
  | OP_LowestPrecedence		(** Pseudo-operator with a precedence lower than all other operators. *)
  | OP_FunctionCall		(** "[a (b)]" *)
  | OP_ArrayAccess		(** "[a[b]]" *)
  | OP_MemberAccess		(** "[a.b]" *)
  | OP_PointerAccess		(** "[a->b]" *)
  | OP_DesignatedInitialiser	(** "[.a = b]" *)
  | OP_ArrayLabelledInitialiser	(** "[[a] = b]" *)
  | OP_Cast			(** "[(a)b]" *)
  | OP_Sizeof			(** "[sizeof (a)]" *)
  | OP_Alignof			(** "[__alignof (a)]" *)
  with sexp

(** {6 Statements} *)

type asm_argument =
  | AsmArgument of (*constraint*)string list * (*expr*)expression

and statement =
  | Nop
  (** Only a semicolon [;]. *)

  (* Statements *)
  | CompoundStatement of (*body*)statement list
  (** [CompoundStatement (_, body)] *)
  | ExpressionStatement of (*expr*)expression option
  (** [ExpressionStatement (_, expr)] *)
  | DeclarationStatement of (*decl*)declaration
  (** [DeclarationStatement (decl)] *)

  (* Labelled statements *)
  | LabelledStatement of (*label*)string * (*stmt*)statement
  (** [LabelledStatement (_, label, stmt)] *)
  | LocalLabel of (*labels*)string list
  (** [LocalLabel (_, labels)] *)
  | CaseStatement of (*expr*)expression
  (** [CaseStatement (_, expr)] *)
  | DefaultStatement
  (** [DefaultStatement (_)] *)

  (* Selection statements *)
  | IfStatement of (*cond*)expression * (*then*)statement * (*else*)statement
  (** [IfStatement (_, cond, then, else)] *)
  | SwitchStatement of (*expr*)expression * (*cases*)statement
  (** [SwitchStatement (_, expr, cases)] *)

  (* Iteration statements *)
  | WhileStatement of (*cond*)expression * (*body*)statement
  (** [WhileStatement (_, cond, body)] *)
  | DoWhileStatement of (*body*)statement * (*cond*)expression
  (** [DoWhileStatement (_, body, cond)] *)
  | ForStatement of (*init*)expression option * (*cond*)expression option * (*next*)expression option * (*body*)statement
  (** [ForStatement (_, init, cond, next, body)] *)

  (* Jump statements *)
  | GotoStatement of (*label*)expression
  (** [GotoStatement (_, label)] *)
  | ContinueStatement
  (** [ContinueStatement (_)] *)
  | BreakStatement
  (** [BreakStatement (_)] *)
  | ReturnStatement of (*expr*)expression option
  (** [ReturnStatement (_, expr)] *)

  (* GCC asm statement *)
  | AsmStatement of (*volatile*)bool * (*code*)string list * (*in_regs*)asm_argument list * (*out_regs*)asm_argument list * (*clobber*)string list list * (*labels*)string list
  (** [AsmStatement (_, volatile, code, in_regs, out_regs, clobber, labels)] *)


(** {6 Expressions} *)

and expression =
  | TypedExpression of (*ty*)ctype * (*value*)Constant.t * (*expr*)expression
  (** [TypedExpression (type, value, expr)] Expression with type information. *)

  | TernaryExpression of ternary_operator * (*cond*)expression * (*then*)expression option * (*else*)expression
  (** [TernaryExpression (_, op, cond, then, else)] *)
  | BinaryExpression of binary_operator * (*lhs*)expression * (*rhs*)expression
  (** [BinaryExpression (_, op, lhs, rhs)] *)
  | UnaryExpression of unary_operator * (*expr*)expression
  (** [UnaryExpression (_, op, expr)] *)
  | SizeofExpr of (*expr*)expression
  (** [SizeofExpr (_, expr)] *)
  | SizeofType of (*type*)ctype
  (** [SizeofType (_, type)] *)
  | AlignofExpr of (*expr*)expression
  (** [AlignofExpr (_, expr)] *)
  | AlignofType of (*type*)ctype
  (** [AlignofType (_, expr)] *)
  | Offsetof of (*type*)ctype * (*member*)expression
  (** [Offsetof (_, type, member)] *)
  | TypesCompatibleP of (*type1*)ctype * (*type2*)ctype
  (** [TypesCompatibleP (_, type1, type2)] *)
  | VaArg of (*ap*)expression * (*type*)ctype
  (** [VaArg (_, ap, type)] *)
  | FunctionCall of (*callee*)expression * (*args*)expression list
  (** [FunctionCall (_, callee, args)] *)
  | CompoundLiteral of (*type*)ctype * (*init*)expression
  (** [CompoundLiteral (_, type, inits)] *)
  | ArrayAccess of (*expr*)expression * (*index*)expression
  (** [ArrayAccess (_, expr, index)] *)
  | MemberAccess of (*expr*)expression * (*member*)string
  (** [MemberAccess (_, expr, member)] *)
  | PointerAccess of (*expr*)expression * (*member*)string
  (** [PointerAccess (_, expr, member)] *)

  (* Primary expression *)
  | Identifier of (*id*)string
  (** [Identifier (_, id)] *)
  | IntegerLiteral of integer_literal_kind * string * string option
  (** [IntegerLiteral (_, kind, number, suffix)] *)
  | FloatingLiteral of floating_literal_kind * string * string option
  (** [FloatingLiteral (_, kind, number, suffix)] *)
  | CharLiteral of char_literal_kind * string
  (** [CharLiteral (_, kind, character)] *)
  | StringLiteral of string_literal_kind * string list
  (** [StringLiteral (_, kind, strings)] *)
  | BraceExpression of (*stmt*)statement
  (** [BraceExpression (_, stmt)] *)

  (* Cast expression *)
  | Cast of (*type*)ctype * (*expr*)expression
  (** [Cast (_, type, expr)] *)

  (* Initialisers *)
  | InitialiserList of (*inits*)expression list
  (** [InitialiserList (_, inits)] *)
  | MemberDesignator of (*member*)string list
  (** [MemberDesignator (_, member)] *)
  | ArrayLabelledInitialiser of (*index*)expression * (*init*)expression
  (** [ArrayLabelledInitialiser (_, index, init)] *)
  | DesignatedInitialiser of (*designator*)expression * (*init*)expression
  (** [DesignatedInitialiser (_, designator, init)] *)


(** {6 Types} *)

and ctype =
  | NoType

  (* Wildcards *)
  | WildcardType of string
  (** [WildcardType (_, wildcard)] *)

  (* Types *)
  | PartialBasicType of partial_basic_type list
  (** [PartialBasicType (basics)] *)
  | BasicType of basic_type
  (** [BasicType (basic)] *)
  | QualifiedType of Tqual.type_qualifiers * (*unqual*)ctype
  (** [QualifiedType (tqs, unqual)] *)
  | PointerType of (*base*)ctype
  (** [PointerType (base)] *)
  | SUEType of sue_kind * (*tag*)string * (*members*)declaration list
  (** [SUEType (_, kind, tag, members)] *)
  | TypedefType of (*name*)string
  (** [TypedefType (name)] *)
  | ArrayType of (*arity*)expression option * (*base*)ctype
  (** [ArrayType (arity, base)] *)
  | FunctionType of (*rettype*)ctype * (*params*)declaration list
  (** [FunctionType (rettype, params)] *)
  | TypeofExpr of (*expr*)expression
  (** [TypeofExpr (expr)] *)
  | TypeofType of (*ty*)ctype
  (** [TypeofType (type)] *)


(** {6 Declarations} *)

and declaration =
  | Nothing

  | TranslationUnit of (*decls*)declaration list
  (** [TranslationUnit (decls)] *)

  (* Wildcards *)
  | WildcardDecl of string
  (** [WildcardDecl (_, wildcard)] *)

  (* Syntax errors *)
  | SyntaxError of (*msg*)string * (*node*)declaration
  (** [SyntaxError (_, msg, node)] *)

  (* #include etc. *)
  | PreprocessorDirective of string
  (** [PreprocessorDirective (_, directive)] *)

  (* Toplevel __asm__ *)
  | ToplevelAsm of (*code*)string list
  (** [ToplevelAsm (_, code)] *)

  (* Declarations *)
  | AsmSpecifier of (*register*)string list
  (** [AsmSpecifier (_, register)] *)
  | FunctionDefinition of (*decl*)declaration * (*body*)statement
  (** [FunctionDefinition (_, decl, body)] *)
  | IdentifierDeclarator of (*id*)string
  (** [IdentifierDeclarator (_, id)] *)
  | StructDeclarator of (*decl*)declaration * (*bitfield*)expression option
  (** [StructDeclarator (_, decl, bitfield)] *)
  | TypedDecl of Sclass.storage_classes * (*type*)ctype * (*untyped*)declaration * (*asm*)declaration * (*init*)expression option
  (** [TypedDecl (_, storage_classes, type, untyped_decl, asm, init)] *)
  | DeclaringList of (*decls*)declaration list
  (** [DeclaringList (_, decls)] *)

  (* Struct/union/enum types *)
  | Enumerator of (*id*)string * (*value*)expression option
  (** [Enumerator (_, id, value)] *)
  with sexp


(** {6 Main exception type} *)

type error =
  | Expression_error of string * string option * expression list
  | Statement_error of string * string option * statement list
  | Type_error of string * string option * ctype list
  | Declaration_error of string * string option * declaration list

  | Parse_error of string * declaration
  | Unimplemented of string

exception ASTError of error

(** {6 S-Expression I/O} *)

let die error =
  raise (ASTError error)
