open CcTokens

let keywords = List.fold_left (fun map (kw, tok) -> StringMap.add kw tok map) StringMap.empty [
  "asm",                        TOK_ASM;
  "__asm",                      TOK_ASM;
  "__asm__",                    TOK_ASM;
  "auto",                       TOK_AUTO;
  "break",                      TOK_BREAK;
  "bool",                       TOK_BOOL;
  "case",                       TOK_CASE;
  "catch",                      TOK_CATCH;
  "cdecl",                      TOK_CDECL;
  "char",                       TOK_CHAR;
  "class",                      TOK_CLASS;
  "__restrict",                 TOK_RESTRICT;
  "__restrict__",               TOK_RESTRICT;
  "__const",                    TOK_CONST;
  "const",                      TOK_CONST;
  "const_cast",                 TOK_CONST_CAST;
  "continue",                   TOK_CONTINUE;
  "default",                    TOK_DEFAULT;
  "delete",                     TOK_DELETE;
  "do",                         TOK_DO;
  "double",                     TOK_DOUBLE;
  "dynamic_cast",               TOK_DYNAMIC_CAST;
  "else",                       TOK_ELSE;
  "enum",                       TOK_ENUM;
  "explicit",                   TOK_EXPLICIT;
  "export",                     TOK_EXPORT;
  "extern",                     TOK_EXTERN;
  "false",                      TOK_FALSE;
  "float",                      TOK_FLOAT;
  "for",                        TOK_FOR;
  "friend",                     TOK_FRIEND;
  "goto",                       TOK_GOTO;
  "if",                         TOK_IF;
  "inline",                     TOK_INLINE;
  "int",                        TOK_INT;
  "long",                       TOK_LONG;
  "mutable",                    TOK_MUTABLE;
  "namespace",                  TOK_NAMESPACE;
  "new",                        TOK_NEW;
  "operator",                   TOK_OPERATOR;
  "pascal",                     TOK_PASCAL;
  "private",                    TOK_PRIVATE;
  "protected",                  TOK_PROTECTED;
  "public",                     TOK_PUBLIC;
  "register",                   TOK_REGISTER;
  "reinterpret_cast",           TOK_REINTERPRET_CAST;
  "return",                     TOK_RETURN;
  "short",                      TOK_SHORT;
  "signed",                     TOK_SIGNED;
  "sizeof",                     TOK_SIZEOF;
  "static",                     TOK_STATIC;
  "static_cast",                TOK_STATIC_CAST;
  "struct",                     TOK_STRUCT;
  "switch",                     TOK_SWITCH;
  "template",                   TOK_TEMPLATE;
  "this",                       TOK_THIS;
  "throw",                      TOK_THROW;
  "true",                       TOK_TRUE;
  "try",                        TOK_TRY;
  "typedef",                    TOK_TYPEDEF;
  "typeid",                     TOK_TYPEID;
  "typename",                   TOK_TYPENAME;
  "union",                      TOK_UNION;
  "unsigned",                   TOK_UNSIGNED;
  "using",                      TOK_USING;
  "virtual",                    TOK_VIRTUAL;
  "void",                       TOK_VOID;
  "volatile",                   TOK_VOLATILE;
  "__volatile__",               TOK_VOLATILE;
  "wchar_t",                    TOK_WCHAR_T;
  "while",                      TOK_WHILE;

  (* GNU *)
  "__builtin_va_arg",           TOK___BUILTIN_VA_ARG;
  "__builtin_constant_p",       TOK___BUILTIN_CONSTANT_P;
  "__attribute__",              TOK___ATTRIBUTE__;
  "__offsetof__",               TOK___OFFSETOF__;
  "__builtin_offsetof",         TOK___BUILTIN_OFFSETOF;
  "__FUNCTION__",               TOK___FUNCTION__;
  "__label__",                  TOK___LABEL__;
  "__PRETTY_FUNCTION__",        TOK___PRETTY_FUNCTION__;
  "typeof",                     TOK___TYPEOF__;
  "__typeof",                   TOK___TYPEOF__;
  "__typeof__",                 TOK___TYPEOF__;
  "__real__",                   TOK_REAL;
  "__imag__",                   TOK_IMAG;
  "__complex__",                TOK_COMPLEX;
  "__extension__",              TOK___EXTENSION__;
  "_Complex",                   TOK_COMPLEX;
  "_Imaginary",                 TOK_IMAGINARY;
  "__inline",                   TOK_INLINE;
  "__inline__",                 TOK_INLINE;

  (* C++11 *)
  "noexcept",                   TOK_NOEXCEPT;
  "constexpr",                  TOK_CONSTEXPR;
  "decltype",                   TOK_DECLTYPE;
  "__decltype",                 TOK_DECLTYPE;
  "__alignof",                  TOK_SIZEOF;
  "__alignof__",                TOK_SIZEOF;
  "nullptr",                    TOK_NULLPTR;
  "static_assert",              TOK_STATIC_ASSERT;
  "char16_t",                   TOK_CHAR16_t;
  "char32_t",                   TOK_CHAR32_t;
]

let string_table = Hashtbl.create 1009

let classify_cc id =
  try
    StringMap.find id keywords
  with Not_found ->
    try
      TOK_NAME (Hashtbl.find string_table id)
    with Not_found ->
      Hashtbl.add string_table id id;
      TOK_NAME id

let classify_c id =
  let open Cc_tokens in
  let tok = classify_cc id in
  if List.memq TF_CPLUSPLUS (token_flags tok) then
    TOK_NAME id
  else
    tok


let classify =
  if Options._xc then
    classify_c
  else
    classify_cc
