%{
#include "main.h"
%}

%option noinput noyywrap bison-bridge bison-locations yylineno
%option never-interactive

LOWER		[a-z]
UPPER		[A-Z]

DIGIT		[0-9]

ALPHA		({LOWER}|{UPPER}|$)
ALNUM		({ALPHA}|{DIGIT})

IDENTIFIER	({ALPHA}|_)({ALNUM}|_)*

BSTRING		`(\\.|[^\n\\`])*`
DSTRING		\"(\\.|[^\n\\\"])*\"
SSTRING		'(\\.|[^\n\\'])*'


D		{DIGIT}
O		[0-7]
H		[a-fA-F0-9]
XH		(0[xX])
B		[01]
XB		(0[bB])
E		([Ee][+-]?{D}+)
P		([Pp][+-]?{D}+)
FS		([ijfFlLqQdD]+)
IS		([ijulUL]+)

WS		[  \t \r]

U		[\x80-\xbf]

%%
 /* whitespace */
[ \t\r\n]+							{ }

 /* keywords, operators */
"__extension__"							{ }
"("								{ return TOK_LPAREN; }
")"								{ return TOK_RPAREN; }
"["|"<:"							{ return TOK_LBRACKET; }
"]"|":>"							{ return TOK_RBRACKET; }
"{"|"<%"							{ return TOK_LBRACE; }
"}"|"%>"							{ return TOK_RBRACE; }
"->"								{ return TOK_ARROW; }
"::"								{ return TOK_COLONCOLON; }
"."								{ return TOK_DOT; }
"!"								{ return TOK_BANG; }
"~"								{ return TOK_TILDE; }
"+"								{ return TOK_PLUS; }
"-"								{ return TOK_MINUS; }
"++"								{ return TOK_PLUSPLUS; }
"--"								{ return TOK_MINUSMINUS; }
"&"								{ return TOK_AND; }
"*"								{ return TOK_STAR; }
".*"								{ return TOK_DOTSTAR; }
"->*"								{ return TOK_ARROWSTAR; }
"/"								{ return TOK_SLASH; }
"%"								{ return TOK_PERCENT; }
"<<"								{ return TOK_LEFTSHIFT; }
">>"								{ return TOK_RIGHTSHIFT; }
"<"								{ return TOK_LESSTHAN; }
"<="								{ return TOK_LESSEQ; }
">"								{ return TOK_GREATERTHAN; }
">="								{ return TOK_GREATEREQ; }
"=="								{ return TOK_EQUALEQUAL; }
"!="								{ return TOK_NOTEQUAL; }
"^"								{ return TOK_XOR; }
"|"								{ return TOK_OR; }
"&&"								{ return TOK_ANDAND; }
"||"								{ return TOK_OROR; }
"?"								{ return TOK_QUESTION; }
":"								{ return TOK_COLON; }
"="								{ return TOK_EQUAL; }
"*="								{ return TOK_STAREQUAL; }
"/="								{ return TOK_SLASHEQUAL; }
"%="								{ return TOK_PERCENTEQUAL; }
"+="								{ return TOK_PLUSEQUAL; }
"-="								{ return TOK_MINUSEQUAL; }
"&="								{ return TOK_ANDEQUAL; }
"^="								{ return TOK_XOREQUAL; }
"|="								{ return TOK_OREQUAL; }
"<<="								{ return TOK_LEFTSHIFTEQUAL; }
">>="								{ return TOK_RIGHTSHIFTEQUAL; }
","								{ return TOK_COMMA; }
"..."								{ return TOK_ELLIPSIS; }
";"								{ return TOK_SEMICOLON; }

 /* GNU */
">?"								{ return TOK_MAX_OP; }
"<?"								{ return TOK_MIN_OP; }

 /* C++ comments */
"//"[^\n]*							{ }

 /* C comments */
"/*"([^*]|"*"[^/])*"*/"						{ }

 /* C keywords */
"asm"				return TOK_ASM;
"auto"				return TOK_AUTO;
"break"				return TOK_BREAK;
"case"				return TOK_CASE;
"char"				return TOK_CHAR;
"const"				return TOK_CONST;
"continue"			return TOK_CONTINUE;
"default"			return TOK_DEFAULT;
"do"				return TOK_DO;
"double"			return TOK_DOUBLE;
"else"				return TOK_ELSE;
"enum"				return TOK_ENUM;
"extern"			return TOK_EXTERN;
"float"				return TOK_FLOAT;
"for"				return TOK_FOR;
"goto"				return TOK_GOTO;
"if"				return TOK_IF;
"int"				return TOK_INT;
"long"				return TOK_LONG;
"register"			return TOK_REGISTER;
"return"			return TOK_RETURN;
"short"				return TOK_SHORT;
"signed"			return TOK_SIGNED;
"sizeof"			return TOK_SIZEOF;
"static"			return TOK_STATIC;
"struct"			return TOK_STRUCT;
"switch"			return TOK_SWITCH;
"typedef"			return TOK_TYPEDEF;
"union"				return TOK_UNION;
"unsigned"			return TOK_UNSIGNED;
"void"				return TOK_VOID;
"volatile"			return TOK_VOLATILE;
"wchar_t"			return TOK_WCHAR_T;
"while"				return TOK_WHILE;

 /* C99 */
"_Bool"				return TOK_BOOL;
"inline"			return TOK_INLINE;
"restrict"			return TOK_RESTRICT;
"_Complex"			return TOK_COMPLEX;
"_Imaginary"			return TOK_IMAGINARY;

 /* C++ keywords */
"bool"				return TOK_BOOL;
"catch"				return TOK_CATCH;
"class"				return TOK_CLASS;
"const_cast"			return TOK_CONST_CAST;
"delete"			return TOK_DELETE;
"dynamic_cast"			return TOK_DYNAMIC_CAST;
"explicit"			return TOK_EXPLICIT;
"export"			return TOK_EXPORT;
"false"				return TOK_FALSE;
"friend"			return TOK_FRIEND;
"mutable"			return TOK_MUTABLE;
"namespace"			return TOK_NAMESPACE;
"new"				return TOK_NEW;
"operator"			return TOK_OPERATOR;
"private"			return TOK_PRIVATE;
"protected"			return TOK_PROTECTED;
"public"			return TOK_PUBLIC;
"reinterpret_cast"		return TOK_REINTERPRET_CAST;
"static_cast"			return TOK_STATIC_CAST;
"template"			return TOK_TEMPLATE;
"this"				return TOK_THIS;
"throw"				return TOK_THROW;
"true"				return TOK_TRUE;
"try"				return TOK_TRY;
"typeid"			return TOK_TYPEID;
"typename"			return TOK_TYPENAME;
"using"				return TOK_USING;
"virtual"			return TOK_VIRTUAL;

 /* operators */
"and"			return TOK_ANDAND;
"or"				return TOK_OROR;
"bitor"				return TOK_OR;
"xor"			return TOK_XOR;
"compl"				return TOK_TILDE;
"bitand"			return TOK_AND;
"and_eq"			return TOK_ANDEQUAL;
"or_eq"				return TOK_OREQUAL;
"xor_eq"			return TOK_XOREQUAL;
"not"			return TOK_BANG;
"not_eq"			return TOK_NOTEQUAL;

 /* C++11 */
"noexcept"			return TOK_NOEXCEPT;
"constexpr"			return TOK_CONSTEXPR;
"decltype"			return TOK_DECLTYPE;
"nullptr"			return TOK_NULLPTR;
"static_assert"			return TOK_STATIC_ASSERT;
"char16_t"			return TOK_CHAR16_t;
"char32_t"			return TOK_CHAR32_t;

 /* GNU */
"cdecl"				return TOK_CDECL;
"pascal"			return TOK_PASCAL;
"__decltype"			return TOK_DECLTYPE;
"__alignof"			return TOK_SIZEOF;
"__alignof__"			return TOK_SIZEOF;
"__volatile__"			return TOK_VOLATILE;
"__asm"				return TOK_ASM;
"__asm__"			return TOK_ASM;
"__const"			return TOK_CONST;
"__const__"			return TOK_CONST;
"__builtin_va_arg"		return TOK___BUILTIN_VA_ARG;
"__builtin_constant_p"		return TOK___BUILTIN_CONSTANT_P;
"__attribute"			return TOK___ATTRIBUTE__;
"__restrict"			return TOK_RESTRICT;
"__restrict__"			return TOK_RESTRICT;
"__attribute__"			return TOK___ATTRIBUTE__;
"__offsetof__"			return TOK___OFFSETOF__;
"__builtin_offsetof"		return TOK___BUILTIN_OFFSETOF;
"__label__"			return TOK___LABEL__;
"__FUNCTION__"			return TOK___FUNCTION__;
"__PRETTY_FUNCTION__"		return TOK___PRETTY_FUNCTION__;
"typeof"			return TOK___TYPEOF__;
"__typeof"			return TOK___TYPEOF__;
"__typeof__"			return TOK___TYPEOF__;
"__real__"			return TOK_REAL;
"__imag__"			return TOK_IMAG;
"__complex__"			return TOK_COMPLEX;
"__signed"			return TOK_SIGNED;
"__signed__"			return TOK_SIGNED;
"__unsigned"			return TOK_UNSIGNED;
"__unsigned__"			return TOK_UNSIGNED;
"__inline"			return TOK_INLINE;
"__inline__"			return TOK_INLINE;

 /* identifier */
{IDENTIFIER}							{ return TOK_NAME; }

 /* integers */
{XH}{H}+{IS}?|{XB}{B}+{IS}?|"0"{O}+{IS}?|{D}+{IS}?		{ return TOK_INT_LITERAL; }

 /* floats */
({D}+{E}|{D}*"."{D}+{E}?|{D}+"."{D}*{E}?|{XH}{H}*{P}{H}*|{XH}{H}*"."{H}*{P}{H}*){FS}?	{ return TOK_FLOAT_LITERAL; }

 /* strings */
"L"?{SSTRING}							{ return TOK_CHAR_LITERAL; }
"L"?{DSTRING}							{ return TOK_STRING_LITERAL; }

"#pragma"[^\n]+							{ }

.								{ puts ("OI"); }
