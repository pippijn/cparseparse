%{
#include "main.h"

int
merge (int a, int b)
{
  return a;
}

#define YYMAXDEPTH 1000000
#define inline __inline__
%}

%glr-parser
%defines "parser.h"
%error-verbose
%locations
%define api.pure

/* automatically generated grammar */

/* -------- tokens -------- */
%token TOK_EOF 0
%token TOK_NAME 1
%token TOK_TYPE_NAME 2
%token TOK_VARIABLE_NAME 3
%token TOK_INT_LITERAL 4
%token TOK_FLOAT_LITERAL 5
%token TOK_STRING_LITERAL 6
%token TOK_CHAR_LITERAL 7
%token TOK_ASM 8
%token TOK_AUTO 9
%token TOK_BREAK 10
%token TOK_BOOL 11
%token TOK_CASE 12
%token TOK_CATCH 13
%token TOK_CDECL 14
%token TOK_CHAR 15
%token TOK_CLASS 16
%token TOK_CONST 17
%token TOK_CONST_CAST 18
%token TOK_CONTINUE 19
%token TOK_DEFAULT 20
%token TOK_DELETE 21
%token TOK_DO 22
%token TOK_DOUBLE 23
%token TOK_DYNAMIC_CAST 24
%token TOK_ELSE 25
%token TOK_ENUM 26
%token TOK_EXPLICIT 27
%token TOK_EXPORT 28
%token TOK_EXTERN 29
%token TOK_FALSE 30
%token TOK_FLOAT 31
%token TOK_FOR 32
%token TOK_FRIEND 33
%token TOK_GOTO 34
%token TOK_IF 35
%token TOK_INLINE 36
%token TOK_INT 37
%token TOK_LONG 38
%token TOK_MUTABLE 39
%token TOK_NAMESPACE 40
%token TOK_NEW 41
%token TOK_OPERATOR 42
%token TOK_PASCAL 43
%token TOK_PRIVATE 44
%token TOK_PROTECTED 45
%token TOK_PUBLIC 46
%token TOK_REGISTER 47
%token TOK_REINTERPRET_CAST 48
%token TOK_RETURN 49
%token TOK_SHORT 50
%token TOK_SIGNED 51
%token TOK_SIZEOF 52
%token TOK_STATIC 53
%token TOK_STATIC_CAST 54
%token TOK_STRUCT 55
%token TOK_SWITCH 56
%token TOK_TEMPLATE 57
%token TOK_THIS 58
%token TOK_THROW 59
%token TOK_TRUE 60
%token TOK_TRY 61
%token TOK_TYPEDEF 62
%token TOK_TYPEID 63
%token TOK_TYPENAME 64
%token TOK_UNION 65
%token TOK_UNSIGNED 66
%token TOK_USING 67
%token TOK_VIRTUAL 68
%token TOK_VOID 69
%token TOK_VOLATILE 70
%token TOK_WCHAR_T 71
%token TOK_WHILE 72
%token TOK_LPAREN 73
%token TOK_RPAREN 74
%token TOK_LBRACKET 75
%token TOK_RBRACKET 76
%token TOK_ARROW 77
%token TOK_COLONCOLON 78
%token TOK_DOT 79
%token TOK_BANG 80
%token TOK_TILDE 81
%token TOK_PLUS 82
%token TOK_MINUS 83
%token TOK_PLUSPLUS 84
%token TOK_MINUSMINUS 85
%token TOK_AND 86
%token TOK_STAR 87
%token TOK_DOTSTAR 88
%token TOK_ARROWSTAR 89
%token TOK_SLASH 90
%token TOK_PERCENT 91
%token TOK_LEFTSHIFT 92
%token TOK_RIGHTSHIFT 93
%token TOK_LESSTHAN 94
%token TOK_LESSEQ 95
%token TOK_GREATERTHAN 96
%token TOK_GREATEREQ 97
%token TOK_EQUALEQUAL 98
%token TOK_NOTEQUAL 99
%token TOK_XOR 100
%token TOK_OR 101
%token TOK_ANDAND 102
%token TOK_OROR 103
%token TOK_QUESTION 104
%token TOK_COLON 105
%token TOK_EQUAL 106
%token TOK_STAREQUAL 107
%token TOK_SLASHEQUAL 108
%token TOK_PERCENTEQUAL 109
%token TOK_PLUSEQUAL 110
%token TOK_MINUSEQUAL 111
%token TOK_ANDEQUAL 112
%token TOK_XOREQUAL 113
%token TOK_OREQUAL 114
%token TOK_LEFTSHIFTEQUAL 115
%token TOK_RIGHTSHIFTEQUAL 116
%token TOK_COMMA 117
%token TOK_ELLIPSIS 118
%token TOK_SEMICOLON 119
%token TOK_LBRACE 120
%token TOK_RBRACE 121
%token TOK_ANNOTATION 122
%token TOK_PREFER_REDUCE 123
%token TOK_PREFER_SHIFT 124
%token TOK___BUILTIN_CONSTANT_P 125
%token TOK___ALIGNOF__ 126
%token TOK___OFFSETOF__ 127
%token TOK___BUILTIN_OFFSETOF 128
%token TOK___ATTRIBUTE__ 129
%token TOK___FUNCTION__ 130
%token TOK___LABEL__ 131
%token TOK___PRETTY_FUNCTION__ 132
%token TOK___TYPEOF__ 133
%token TOK___EXTENSION__ 134
%token TOK___BUILTIN_EXPECT 135
%token TOK___BUILTIN_VA_ARG 136
%token TOK_MIN_OP 137
%token TOK_MAX_OP 138
%token TOK_REAL 139
%token TOK_IMAG 140
%token TOK_RESTRICT 141
%token TOK_COMPLEX 142
%token TOK_IMAGINARY 143

%token TOK_NOEXCEPT
%token TOK_CONSTEXPR
%token TOK_DECLTYPE
%token TOK_NULLPTR
%token TOK_STATIC_ASSERT
%token TOK_CHAR16_t
%token TOK_CHAR32_t


/* -------- precedence and associativity ---------*/
/* low precedence */
%nonassoc TOK_PREFER_SHIFT


















%left TOK_OROR









%left TOK_ANDAND









%left TOK_OR









%left TOK_XOR









%left TOK_AND









%left TOK_EQUALEQUAL TOK_NOTEQUAL



















%left TOK_LEFTSHIFT TOK_RIGHTSHIFT









%left TOK_PLUS TOK_MINUS









%left TOK_STAR TOK_SLASH TOK_PERCENT









%left TOK_DOTSTAR TOK_ARROWSTAR





































































%nonassoc TOK_CONST TOK_ELSE TOK_VOLATILE TOK_LBRACKET TOK___ATTRIBUTE__ TOK_RESTRICT




%right TOK_COLONCOLON




%nonassoc TOK_PREFER_REDUCE
/* high precedence */

/* -------- productions ------ */
%%

__EarlyStartSymbol: File TOK_EOF { $$=0; }
                  ;

File: TranslationUnit { $$=1; }
    ;

Identifier: TOK_NAME { $$=2; }
          ;

TranslationUnit: /* empty */ { $$=3; }
               | TranslationUnit Declaration { $$=4; }
               | TranslationUnit TOK_SEMICOLON { $$=5; }
               ;

PrimaryExpression: Literal { $$=6; }
                 | TOK_THIS { $$=7; }
                 | TOK_LPAREN Expression TOK_RPAREN { $$=8; }
                 | IdExpression { $$=9; }
                 | TOK_LPAREN CompoundStatement TOK_RPAREN { $$=10; }
                 | TOK_LPAREN TypeId TOK_RPAREN CompoundInitializer { $$=11; }
                 | TOK_REAL PrimaryExpression { $$=12; }
                 | TOK_IMAG PrimaryExpression { $$=13; }
                 ;

Literal: TOK_INT_LITERAL { $$=14; }
       | TOK_FLOAT_LITERAL { $$=15; }
       | StringLiteral { $$=16; }
       | TOK_CHAR_LITERAL { $$=17; }
       | TOK_TRUE { $$=18; }
       | TOK_FALSE { $$=19; }
       ;

PreprocString: TOK_STRING_LITERAL { $$=20; }
             | TOK___FUNCTION__ { $$=21; }
             | TOK___PRETTY_FUNCTION__ { $$=22; }
             ;

StringLiteral: PreprocString { $$=23; }
             | PreprocString StringLiteral { $$=24; }
             ;

IdExpression: PQualifiedId { $$=25; }
            | TOK_COLONCOLON PQualifiedId %prec TOK_COLONCOLON { $$=26; }
            ;

UnqualifiedId: Identifier { $$=27; }
             | OperatorFunctionId { $$=28; }
             | ConversionFunctionId { $$=29; }
             | TemplateId { $$=30; }
             ;

PQualifiedId: UnqualifiedId %prec TOK_COLONCOLON { $$=31; }
            | Identifier TOK_COLONCOLON PQualifiedId %prec TOK_COLONCOLON { $$=32; }
            | Identifier TOK_LESSTHAN TemplateArgumentListOpt TOK_GREATERTHAN TOK_COLONCOLON PQualifiedId %prec TOK_COLONCOLON { $$=33; }
            | TOK_TEMPLATE Identifier TOK_LESSTHAN TemplateArgumentListOpt TOK_GREATERTHAN TOK_COLONCOLON PQualifiedId %prec TOK_COLONCOLON { $$=34; }
            ;

ArgumentList: TOK_LPAREN ExpressionListOpt TOK_RPAREN { $$=35; }
            ;

PostfixExpression: PrimaryExpression { $$=36; }
                 | PostfixExpression TOK_LBRACKET Expression TOK_RBRACKET { $$=37; }
                 | PostfixExpression ArgumentList %merge<merge> { $$=38; }
                 | TOK_TYPENAME IdExpression ArgumentList { $$=39; }
                 | CtorExpressionType ArgumentList %merge<merge> { $$=40; }
                 | PostfixExpression TOK_DOT NameAfterDot { $$=41; }
                 | PostfixExpression TOK_ARROW NameAfterDot { $$=42; }
                 | PostfixExpression TOK_PLUSPLUS { $$=43; }
                 | PostfixExpression TOK_MINUSMINUS { $$=44; }
                 | CastKeyword TOK_LESSTHAN TypeId TOK_GREATERTHAN TOK_LPAREN Expression TOK_RPAREN { $$=45; }
                 | TOK_TYPEID TOK_LPAREN Expression TOK_RPAREN { $$=46; }
                 | TOK_TYPEID TOK_LPAREN TypeId TOK_RPAREN { $$=47; }
                 | TOK___BUILTIN_CONSTANT_P ParenthesizedExpression { $$=48; }
                 | TOK___ALIGNOF__ TOK_LPAREN TypeId TOK_RPAREN { $$=49; }
                 | TOK___ALIGNOF__ TOK_LPAREN Expression TOK_RPAREN { $$=50; }
                 | TOK___BUILTIN_OFFSETOF TOK_LPAREN TypeId TOK_COMMA NamesAfterDot TOK_RPAREN { $$=51; }
                 | TOK___OFFSETOF__ TOK_LPAREN Expression TOK_RPAREN { $$=52; }
                 | TOK___BUILTIN_EXPECT TOK_LPAREN Expression TOK_COMMA Expression TOK_RPAREN { $$=53; }
                 | TOK___BUILTIN_VA_ARG TOK_LPAREN Expression TOK_COMMA TypeId TOK_RPAREN { $$=54; }
                 ;

CtorExpressionType: PQTypeName { $$=55; }
                  | TOK_CHAR { $$=56; }
                  | TOK_WCHAR_T { $$=57; }
                  | TOK_BOOL { $$=58; }
                  | TOK_SHORT { $$=59; }
                  | TOK_INT { $$=60; }
                  | TOK_LONG { $$=61; }
                  | TOK_SIGNED { $$=62; }
                  | TOK_UNSIGNED { $$=63; }
                  | TOK_FLOAT { $$=64; }
                  | TOK_DOUBLE { $$=65; }
                  | TOK_VOID { $$=66; }
                  ;

CastKeyword: TOK_DYNAMIC_CAST { $$=67; }
           | TOK_STATIC_CAST { $$=68; }
           | TOK_REINTERPRET_CAST { $$=69; }
           | TOK_CONST_CAST { $$=70; }
           ;

ExpressionList: AssignmentExpression %merge<merge> { $$=71; }
              | AssignmentExpression TOK_COMMA ExpressionList %merge<merge> { $$=72; }
              ;

ExpressionListOpt: /* empty */ { $$=73; }
                 | ExpressionList { $$=74; }
                 ;

UnaryExpression: PostfixExpression { $$=75; }
               | TOK_PLUSPLUS CastExpression { $$=76; }
               | TOK_MINUSMINUS CastExpression { $$=77; }
               | TOK_SIZEOF UnaryExpression %merge<merge> { $$=78; }
               | DeleteExpression { $$=79; }
               | TOK_STAR CastExpression %prec TOK_STAR { $$=80; }
               | TOK_AND CastExpression %prec TOK_AND { $$=81; }
               | TOK_PLUS CastExpression %prec TOK_PLUS { $$=82; }
               | TOK_MINUS CastExpression %prec TOK_PLUS { $$=83; }
               | TOK_BANG CastExpression { $$=84; }
               | TOK_TILDE CastExpression { $$=85; }
               | TOK_SIZEOF TOK_LPAREN TypeId TOK_RPAREN %merge<merge> { $$=86; }
               | NewExpression { $$=87; }
               | TOK_ANDAND Identifier %prec TOK_ANDAND { $$=88; }
               ;

ColonColonOpt: /* empty */ { $$=89; }
             | TOK_COLONCOLON %prec TOK_COLONCOLON { $$=90; }
             ;

NewExpression: ColonColonOpt TOK_NEW NewPlacementOpt NewTypeId NewInitializerOpt { $$=91; }
             | ColonColonOpt TOK_NEW NewPlacementOpt TOK_LPAREN TypeId TOK_RPAREN NewInitializerOpt { $$=92; }
             ;

NewPlacementOpt: /* empty */ { $$=93; }
               | TOK_LPAREN ExpressionList TOK_RPAREN { $$=94; }
               ;

NewTypeId: TypeSpecifier NewDeclaratorOpt { $$=95; }
         ;

NewDeclaratorOpt: /* empty */ { $$=96; }
                | TOK_STAR CVQualifierSeqOpt NewDeclaratorOpt %prec TOK_STAR { $$=97; }
                | PtrToMemberName TOK_STAR CVQualifierSeqOpt NewDeclaratorOpt %prec TOK_STAR { $$=98; }
                | DirectNewDeclarator { $$=99; }
                ;

DirectNewDeclarator: TOK_LBRACKET Expression TOK_RBRACKET { $$=100; }
                   | DirectNewDeclarator TOK_LBRACKET ConstantExpression TOK_RBRACKET { $$=101; }
                   ;

NewInitializerOpt: /* empty */ { $$=102; }
                 | TOK_LPAREN ExpressionListOpt TOK_RPAREN { $$=103; }
                 ;

DeleteExpression: ColonColonOpt TOK_DELETE CastExpression { $$=104; }
                | ColonColonOpt TOK_DELETE TOK_LBRACKET TOK_RBRACKET CastExpression { $$=105; }
                ;

NameAfterDot: NAD1 { $$=106; }
            | TOK_COLONCOLON NAD2 %prec TOK_COLONCOLON { $$=107; }
            ;

NAD1: NAD2 { $$=108; }
    | TOK_TEMPLATE Identifier TOK_LESSTHAN TemplateArgumentListOpt TOK_GREATERTHAN { $$=109; }
    | TOK_TILDE Identifier { $$=110; }
    | TOK_TILDE Identifier TOK_LESSTHAN TemplateArgumentListOpt TOK_GREATERTHAN { $$=111; }
    | ConversionFunctionId { $$=112; }
    | TOK_TEMPLATE Identifier TOK_LESSTHAN TemplateArgumentListOpt TOK_GREATERTHAN TOK_COLONCOLON NAD1 %prec TOK_COLONCOLON { $$=113; }
    ;

NAD2: Identifier TOK_LESSTHAN TemplateArgumentListOpt TOK_GREATERTHAN { $$=114; }
    | Identifier { $$=115; }
    | OperatorFunctionId { $$=116; }
    | OperatorFunctionId TOK_LESSTHAN TemplateArgumentListOpt TOK_GREATERTHAN { $$=117; }
    | TOK_TEMPLATE OperatorFunctionId TOK_LESSTHAN TemplateArgumentListOpt TOK_GREATERTHAN { $$=118; }
    | Identifier TOK_LESSTHAN TemplateArgumentListOpt TOK_GREATERTHAN TOK_COLONCOLON NAD1 %prec TOK_COLONCOLON { $$=119; }
    | Identifier TOK_COLONCOLON NAD1 %prec TOK_COLONCOLON { $$=120; }
    ;

CastExpression: UnaryExpression %merge<merge> { $$=121; }
              | TOK_LPAREN TypeId TOK_RPAREN CastExpression %merge<merge> { $$=122; }
              | TOK_LPAREN ImplicitIntTypeId TOK_RPAREN CastExpression { $$=123; }
              ;

BinExp_high: CastExpression %merge<merge> { $$=124; }
           | BinExp_high TOK_DOTSTAR BinExp_high %prec TOK_DOTSTAR { $$=125; }
           | BinExp_high TOK_ARROWSTAR BinExp_high %prec TOK_DOTSTAR { $$=126; }
           | BinExp_high TOK_STAR BinExp_high %prec TOK_STAR { $$=127; }
           | BinExp_high TOK_SLASH BinExp_high %prec TOK_STAR { $$=128; }
           | BinExp_high TOK_PERCENT BinExp_high %prec TOK_STAR { $$=129; }
           | BinExp_high TOK_PLUS BinExp_high %prec TOK_PLUS %merge<merge> { $$=130; }
           | BinExp_high TOK_MINUS BinExp_high %prec TOK_PLUS { $$=131; }
           | BinExp_high TOK_LEFTSHIFT BinExp_high %prec TOK_LEFTSHIFT { $$=132; }
           | BinExp_high TOK_RIGHTSHIFT BinExp_high %prec TOK_LEFTSHIFT { $$=133; }
           ;

BinExp_mid: BinExp_high %merge<merge> { $$=134; }
          | BinExp_mid TOK_LESSTHAN BinExp_high %merge<merge> { $$=135; }
          | BinExp_mid TOK_GREATERTHAN BinExp_high %merge<merge> { $$=136; }
          | BinExp_mid TOK_LESSEQ BinExp_high { $$=137; }
          | BinExp_mid TOK_GREATEREQ BinExp_high { $$=138; }
          | BinExp_mid TOK_MIN_OP BinExp_high { $$=139; }
          | BinExp_mid TOK_MAX_OP BinExp_high { $$=140; }
          ;

BinaryExpression: BinExp_mid { $$=141; }
                | BinaryExpression TOK_EQUALEQUAL BinaryExpression %prec TOK_EQUALEQUAL { $$=142; }
                | BinaryExpression TOK_NOTEQUAL BinaryExpression %prec TOK_EQUALEQUAL { $$=143; }
                | BinaryExpression TOK_AND BinaryExpression %prec TOK_AND { $$=144; }
                | BinaryExpression TOK_XOR BinaryExpression %prec TOK_XOR { $$=145; }
                | BinaryExpression TOK_OR BinaryExpression %prec TOK_OR { $$=146; }
                | BinaryExpression TOK_ANDAND BinaryExpression %prec TOK_ANDAND { $$=147; }
                | BinaryExpression TOK_OROR BinaryExpression %prec TOK_OROR { $$=148; }
                ;

ConditionalExpression: BinaryExpression { $$=149; }
                     | BinaryExpression TOK_QUESTION Expression TOK_COLON AssignmentExpression { $$=150; }
                     | BinaryExpression TOK_QUESTION TOK_COLON AssignmentExpression { $$=151; }
                     ;

AssignmentExpression: ConditionalExpression { $$=152; }
                    | BinaryExpression AssignmentOperator AssignmentExpression { $$=153; }
                    | ThrowExpression { $$=154; }
                    ;

AssignmentOperator: TOK_STAREQUAL { $$=155; }
                  | TOK_SLASHEQUAL { $$=156; }
                  | TOK_PERCENTEQUAL { $$=157; }
                  | TOK_PLUSEQUAL { $$=158; }
                  | TOK_MINUSEQUAL { $$=159; }
                  | TOK_RIGHTSHIFTEQUAL { $$=160; }
                  | TOK_LEFTSHIFTEQUAL { $$=161; }
                  | TOK_ANDEQUAL { $$=162; }
                  | TOK_XOREQUAL { $$=163; }
                  | TOK_OREQUAL { $$=164; }
                  | TOK_EQUAL { $$=165; }
                  ;

Expression: AssignmentExpression %merge<merge> { $$=166; }
          | Expression TOK_COMMA AssignmentExpression %merge<merge> { $$=167; }
          ;

ExpressionOpt: /* empty */ { $$=168; }
             | Expression { $$=169; }
             ;

ConstantExpression: AssignmentExpression { $$=170; }
                  ;

ConstantExpressionOpt: /* empty */ { $$=171; }
                     | ConstantExpression { $$=172; }
                     ;

LabelAndColon: Identifier TOK_COLON %prec TOK_PREFER_SHIFT { $$=173; }
             | Identifier TOK_COLON AttributeSpecifierList %prec TOK_PREFER_REDUCE { $$=174; }
             ;

Statement: LabelAndColon Statement { $$=175; }
         | TOK_CASE ConstantExpression TOK_COLON Statement { $$=176; }
         | TOK_DEFAULT TOK_COLON Statement { $$=177; }
         | ExpressionStatement %merge<merge> { $$=178; }
         | CompoundStatement { $$=179; }
         | TOK_IF TOK_LPAREN Condition TOK_RPAREN Statement %prec TOK_PREFER_SHIFT { $$=180; }
         | TOK_IF TOK_LPAREN Condition TOK_RPAREN Statement TOK_ELSE Statement %prec TOK_CONST { $$=181; }
         | TOK_SWITCH TOK_LPAREN Condition TOK_RPAREN Statement { $$=182; }
         | TOK_WHILE TOK_LPAREN Condition TOK_RPAREN Statement { $$=183; }
         | TOK_DO Statement TOK_WHILE TOK_LPAREN Expression TOK_RPAREN TOK_SEMICOLON { $$=184; }
         | TOK_FOR TOK_LPAREN ForInitStatement ConditionOpt TOK_SEMICOLON ExpressionOpt TOK_RPAREN Statement { $$=185; }
         | TOK_BREAK TOK_SEMICOLON { $$=186; }
         | TOK_CONTINUE TOK_SEMICOLON { $$=187; }
         | TOK_RETURN Expression TOK_SEMICOLON { $$=188; }
         | TOK_RETURN TOK_SEMICOLON { $$=189; }
         | TOK_GOTO Identifier TOK_SEMICOLON { $$=190; }
         | BlockDeclaration %merge<merge> { $$=191; }
         | TryBlock { $$=192; }
         | AsmDefinition { $$=193; }
         | NamespaceDecl { $$=194; }
         | FunctionDefinition { $$=195; }
         | KandRFunctionDefinition { $$=196; }
         | TOK_CASE ConstantExpression TOK_ELLIPSIS ConstantExpression TOK_COLON Statement { $$=197; }
         | TOK_GOTO TOK_STAR Expression TOK_SEMICOLON { $$=198; }
         | UberModifierSeq InitDeclaratorList TOK_SEMICOLON %merge<merge> { $$=199; }
         ;

ExpressionStatement: TOK_SEMICOLON { $$=200; }
                   | Expression TOK_SEMICOLON { $$=201; }
                   ;

CompoundStatement: CompoundStmtHelper TOK_RBRACE { $$=202; }
                 | CompoundStmtHelper LabeledEmptyStatementList TOK_RBRACE { $$=203; }
                 ;

CompoundStmtHelper: TOK_LBRACE { $$=204; }
                  | CompoundStmtHelper Statement AnnotationOpt { $$=205; }
                  ;

Condition: Expression { $$=206; }
         | TypeSpecifier Declarator TOK_EQUAL AssignmentExpression { $$=207; }
         ;

ConditionOpt: /* empty */ { $$=208; }
            | Condition { $$=209; }
            ;

ForInitStatement: ExpressionStatement { $$=210; }
                | SimpleDeclaration { $$=211; }
                ;

Declaration: BlockDeclaration %merge<merge> { $$=212; }
           | FunctionDefinition { $$=213; }
           | TemplateDeclaration { $$=214; }
           | ExplicitInstantiation { $$=215; }
           | LinkageSpecification { $$=216; }
           | AsmDefinition { $$=217; }
           | NamespaceDefinition { $$=218; }
           | NamespaceDecl { $$=219; }
           | KandRFunctionDefinition { $$=220; }
           | KandRFunctionDefinition_implInt { $$=221; }
           | ImplIntFunctionDefinition { $$=222; }
           | UberModifierSeqOpt InitDeclaratorList TOK_SEMICOLON %merge<merge> { $$=223; }
           ;

BlockDeclaration: SimpleDeclaration { $$=224; }
                ;

SimpleDeclaration: DeclSpecifier InitDeclaratorList TOK_SEMICOLON %merge<merge> { $$=225; }
                 | DeclSpecifier TOK_SEMICOLON %merge<merge> { $$=226; }
                 ;

DeclSpecifier: PQTypeName UberModifierSeqOpt { $$=227; }
             | UberModifierSeq PQTypeName UberModifierSeqOpt { $$=228; }
             | UberTypeKeyword UberTypeAndModifierSeqOpt { $$=229; }
             | UberModifierSeq UberTypeKeyword UberTypeAndModifierSeqOpt { $$=230; }
             | ElaboratedOrSpecifier UberModifierSeqOpt { $$=231; }
             | UberModifierSeq ElaboratedOrSpecifier UberModifierSeqOpt { $$=232; }
             | TypeofTypeSpecifier UberModifierSeqOpt { $$=233; }
             | UberModifierSeq TypeofTypeSpecifier UberModifierSeqOpt { $$=234; }
             | PQTypeName BuggyGccTypeModifier UberModifierSeqOpt { $$=235; }
             | UberModifierSeq PQTypeName BuggyGccTypeModifier UberModifierSeqOpt { $$=236; }
             ;

ElaboratedOrSpecifier: ElaboratedTypeSpecifier { $$=237; }
                     | ClassSpecifier { $$=238; }
                     | EnumSpecifier { $$=239; }
                     ;

UberModifierSeq: UberModifier { $$=240; }
               | UberModifierSeq UberModifier { $$=241; }
               ;

UberModifierSeqOpt: /* empty */ { $$=242; }
                  | UberModifierSeq { $$=243; }
                  ;

UberTypeAndModifierSeqOpt: /* empty */ { $$=244; }
                         | UberTypeAndModifierSeqOpt UberModifier { $$=245; }
                         | UberTypeAndModifierSeqOpt UberTypeKeyword { $$=246; }
                         ;

UberCVQualifierSeq: UberCVQualifier { $$=247; }
                  | UberCVQualifierSeq UberCVQualifier { $$=248; }
                  ;

UberCVQualifierSeqOpt: /* empty */ { $$=249; }
                     | UberCVQualifierSeq { $$=250; }
                     ;

UberTypeAndCVQualifierSeqOpt: /* empty */ { $$=251; }
                            | UberTypeAndCVQualifierSeqOpt UberCVQualifier { $$=252; }
                            | UberTypeAndCVQualifierSeqOpt UberTypeKeyword { $$=253; }
                            ;

UberModifier: TOK_AUTO { $$=254; }
            | TOK_REGISTER { $$=255; }
            | TOK_STATIC { $$=256; }
            | TOK_EXTERN { $$=257; }
            | TOK_MUTABLE { $$=258; }
            | TOK_INLINE { $$=259; }
            | TOK_VIRTUAL { $$=260; }
            | TOK_FRIEND { $$=261; }
            | TOK_TYPEDEF { $$=262; }
            | TOK_CONST %prec TOK_CONST { $$=263; }
            | TOK_VOLATILE %prec TOK_CONST { $$=264; }
            | TOK_RESTRICT %prec TOK_CONST { $$=265; }
            | AttributeSpecifier { $$=266; }
            ;

UberCVQualifier: TOK_CONST %prec TOK_CONST { $$=267; }
               | TOK_VOLATILE %prec TOK_CONST { $$=268; }
               | TOK_RESTRICT %prec TOK_CONST { $$=269; }
               | AttributeSpecifier { $$=270; }
               ;

UberTypeKeyword: TOK_CHAR { $$=271; }
               | TOK_WCHAR_T { $$=272; }
               | TOK_BOOL { $$=273; }
               | TOK_SHORT { $$=274; }
               | TOK_INT { $$=275; }
               | TOK_LONG { $$=276; }
               | TOK_SIGNED { $$=277; }
               | TOK_UNSIGNED { $$=278; }
               | TOK_FLOAT { $$=279; }
               | TOK_DOUBLE { $$=280; }
               | TOK_VOID { $$=281; }
               | TOK_COMPLEX { $$=282; }
               | TOK_IMAGINARY { $$=283; }
               ;

ElaboratedTypeSpecifier: ClassKey PQTypeName { $$=284; }
                       | TOK_ENUM PQTypeName { $$=285; }
                       | TOK_TYPENAME PQTypeName { $$=286; }
                       | ClassKey AttributeSpecifier PQTypeName { $$=287; }
                       | TOK_ENUM AttributeSpecifierList PQTypeName { $$=288; }
                       ;

TypeSpecifier: PQTypeName UberCVQualifierSeqOpt { $$=289; }
             | UberCVQualifierSeq PQTypeName UberCVQualifierSeqOpt { $$=290; }
             | UberTypeKeyword UberTypeAndCVQualifierSeqOpt { $$=291; }
             | UberCVQualifierSeq UberTypeKeyword UberTypeAndCVQualifierSeqOpt { $$=292; }
             | ElaboratedOrSpecifier UberCVQualifierSeqOpt { $$=293; }
             | UberCVQualifierSeq ElaboratedOrSpecifier UberCVQualifierSeqOpt { $$=294; }
             | TypeofTypeSpecifier UberCVQualifierSeqOpt { $$=295; }
             | UberCVQualifierSeq TypeofTypeSpecifier UberCVQualifierSeqOpt { $$=296; }
             ;

PQTypeName: PQTypeName_ncc { $$=297; }
          | TOK_COLONCOLON PQTypeName_ncc %prec TOK_COLONCOLON { $$=298; }
          ;

PQTypeName_ncc: Identifier %prec TOK_COLONCOLON { $$=299; }
              | TemplateId %prec TOK_COLONCOLON { $$=300; }
              | Identifier TOK_COLONCOLON PQTypeName_notfirst %prec TOK_COLONCOLON { $$=301; }
              | Identifier TOK_LESSTHAN TemplateArgumentListOpt TOK_GREATERTHAN TOK_COLONCOLON PQTypeName_notfirst %prec TOK_COLONCOLON { $$=302; }
              ;

PQTypeName_notfirst: PQTypeName_ncc %prec TOK_COLONCOLON { $$=303; }
                   | TOK_TEMPLATE TemplateId %prec TOK_COLONCOLON { $$=304; }
                   | TOK_TEMPLATE Identifier TOK_LESSTHAN TemplateArgumentListOpt TOK_GREATERTHAN TOK_COLONCOLON PQTypeName_notfirst %prec TOK_COLONCOLON { $$=305; }
                   ;

EnumSpecifier: TOK_ENUM TOK_LBRACE EnumeratorListOpt TOK_RBRACE { $$=306; }
             | TOK_ENUM Identifier TOK_LBRACE EnumeratorListOpt TOK_RBRACE { $$=307; }
             | TOK_ENUM AttributeSpecifierList TOK_LBRACE EnumeratorListOpt TOK_RBRACE { $$=308; }
             | TOK_ENUM AttributeSpecifierList PQTypeName TOK_LBRACE EnumeratorListOpt TOK_RBRACE { $$=309; }
             ;

EnumeratorListOpt: /* empty */ { $$=310; }
                 | EnumeratorDefinition { $$=311; }
                 | EnumeratorDefinition TOK_COMMA EnumeratorListOpt { $$=312; }
                 ;

EnumeratorDefinition: Identifier { $$=313; }
                    | Identifier TOK_EQUAL ConstantExpression { $$=314; }
                    ;

AsmDefinition: TOK_ASM TOK_LPAREN StringLiteral TOK_RPAREN TOK_SEMICOLON { $$=315; }
             | TOK_ASM CVQualifierSeq TOK_LPAREN StringLiteral TOK_RPAREN TOK_SEMICOLON { $$=316; }
             | TOK_ASM CVQualifierSeq TOK_LPAREN StringLiteral NonemptyOpConstraints TOK_RPAREN TOK_SEMICOLON { $$=317; }
             | TOK_ASM TOK_LPAREN StringLiteral NonemptyOpConstraints TOK_RPAREN TOK_SEMICOLON { $$=318; }
             ;

LinkageSpecification: TOK_EXTERN TOK_STRING_LITERAL TOK_LBRACE TranslationUnit TOK_RBRACE { $$=319; }
                    | TOK_EXTERN TOK_STRING_LITERAL Declaration { $$=320; }
                    ;

InitDeclaratorList: InitDeclarator %merge<merge> { $$=321; }
                  | InitDeclarator TOK_COMMA InitDeclaratorList %merge<merge> { $$=322; }
                  | InitDeclarator TOK_COMMA AttributeSpecifierList InitDeclaratorList { $$=323; }
                  ;

InitDeclarator: Declarator %merge<merge> { $$=324; }
              | Declarator Initializer %merge<merge> { $$=325; }
              ;

Initializer: TOK_EQUAL SimpleInitializerClause { $$=326; }
           | TOK_LPAREN ExpressionList TOK_RPAREN { $$=327; }
           ;

SimpleInitializerClause: AssignmentExpression { $$=328; }
                       | CompoundInitializer { $$=329; }
                       ;

InitializerClause: SimpleInitializerClause { $$=330; }
                 | Identifier TOK_COLON SimpleInitializerClause { $$=331; }
                 | DesignatorList TOK_EQUAL SimpleInitializerClause { $$=332; }
                 | DesignatorList SimpleInitializerClause { $$=333; }
                 ;

CompoundInitializer: TOK_LBRACE InitializerList CommaOpt TOK_RBRACE { $$=334; }
                   | TOK_LBRACE TOK_RBRACE { $$=335; }
                   ;

CommaOpt: /* empty */ { $$=336; }
        | TOK_COMMA { $$=337; }
        ;

InitializerList: InitializerClause { $$=338; }
               | InitializerList TOK_COMMA InitializerClause { $$=339; }
               ;

Declarator: TOK_STAR CVQualifierSeqOpt Declarator %prec TOK_STAR { $$=340; }
          | TOK_AND CVQualifierSeqOpt Declarator %prec TOK_AND { $$=341; }
          | PtrToMemberName TOK_STAR CVQualifierSeqOpt Declarator %prec TOK_STAR { $$=342; }
          | DirectDeclarator { $$=343; }
          | DirectDeclarator TOK_ASM TOK_LPAREN StringLiteral TOK_RPAREN { $$=344; }
          | DirectDeclarator TOK_ASM TOK_LPAREN StringLiteral TOK_RPAREN AttributeSpecifierList { $$=345; }
          | TOK_STAR CVQualifierSeqOpt AttributeSpecifier CVQualAttrSeqOpt Declarator %prec TOK_STAR { $$=346; }
          | DirectDeclarator AttributeSpecifierList { $$=347; }
          ;

DirectDeclarator: IdExpression { $$=348; }
                | PQDtorName { $$=349; }
                | DirectDeclarator TOK_LPAREN ParameterDeclarationClause TOK_RPAREN CVQualifierSeqOpt ExceptionSpecificationOpt { $$=350; }
                | DirectDeclarator TOK_LBRACKET ConstantExpressionOpt TOK_RBRACKET { $$=351; }
                | TOK_LPAREN Declarator TOK_RPAREN { $$=352; }
                | DirectDeclarator TOK_LBRACKET CVQualifierSeq TOK_RBRACKET { $$=353; }
                | TOK_LPAREN AttributeSpecifierList Declarator TOK_RPAREN { $$=354; }
                ;

PQDtorName: TOK_TILDE Identifier { $$=355; }
          | TOK_TILDE Identifier TOK_LESSTHAN TemplateArgumentListOpt TOK_GREATERTHAN { $$=356; }
          | Identifier TOK_COLONCOLON PQDtorName %prec TOK_COLONCOLON { $$=357; }
          | Identifier TOK_LESSTHAN TemplateArgumentListOpt TOK_GREATERTHAN TOK_COLONCOLON PQDtorName %prec TOK_COLONCOLON { $$=358; }
          | TOK_TEMPLATE Identifier TOK_LESSTHAN TemplateArgumentListOpt TOK_GREATERTHAN TOK_COLONCOLON PQDtorName %prec TOK_COLONCOLON { $$=359; }
          ;

PtrToMemberName: IdExpression TOK_COLONCOLON %prec TOK_COLONCOLON { $$=360; }
               ;

CVQualifierSeqOpt: /* empty */ { $$=361; }
                 | CVQualifierSeq { $$=362; }
                 ;

CVQualifierSeq: CVQualifier { $$=363; }
              | CVQualifier CVQualifierSeq { $$=364; }
              ;

CVQualifier: TOK_CONST %prec TOK_CONST { $$=365; }
           | TOK_VOLATILE %prec TOK_CONST { $$=366; }
           | TOK_RESTRICT %prec TOK_CONST { $$=367; }
           ;

TypeId: TypeSpecifier AbstractDeclaratorOpt { $$=368; }
      ;

AbstractDeclaratorOpt: /* empty */ { $$=369; }
                     | AbstractDeclarator { $$=370; }
                     ;

AbstractDeclarator: TOK_STAR CVQualifierSeqOpt AbstractDeclaratorOpt %prec TOK_STAR { $$=371; }
                  | TOK_AND CVQualifierSeqOpt AbstractDeclaratorOpt %prec TOK_AND { $$=372; }
                  | PtrToMemberName TOK_STAR CVQualifierSeqOpt AbstractDeclaratorOpt %prec TOK_STAR { $$=373; }
                  | DirectAbstractDeclarator { $$=374; }
                  | TOK_STAR CVQualifierSeqOpt AttributeSpecifier CVQualAttrSeqOpt AbstractDeclaratorOpt %prec TOK_STAR { $$=375; }
                  | DirectAbstractDeclarator AttributeSpecifierList { $$=376; }
                  ;

DirectAbstractDeclaratorOpt: /* empty */ { $$=377; }
                           | DirectAbstractDeclarator { $$=378; }
                           ;

DirectAbstractDeclarator: DirectAbstractDeclaratorOpt TOK_LPAREN ParameterDeclarationClause TOK_RPAREN CVQualifierSeqOpt ExceptionSpecificationOpt { $$=379; }
                        | DirectAbstractDeclaratorOpt TOK_LBRACKET ConstantExpressionOpt TOK_RBRACKET { $$=380; }
                        | TOK_LPAREN AbstractDeclarator TOK_RPAREN { $$=381; }
                        | DirectAbstractDeclaratorOpt TOK_LBRACKET CVQualifierSeq TOK_RBRACKET { $$=382; }
                        | TOK_LPAREN AttributeSpecifierList AbstractDeclarator TOK_RPAREN { $$=383; }
                        ;

ParameterDeclarationClause: ParameterDeclarationList { $$=384; }
                          | /* empty */ { $$=385; }
                          ;

ParameterDeclarationList: TOK_ELLIPSIS { $$=386; }
                        | ParameterDeclaration TOK_ELLIPSIS { $$=387; }
                        | ParameterDeclaration %merge<merge> { $$=388; }
                        | ParameterDeclaration TOK_COMMA ParameterDeclarationList %merge<merge> { $$=389; }
                        ;

ParameterDeclaration: TypeSpecifier ParameterDeclarator %merge<merge> { $$=390; }
                    | TOK_REGISTER TypeSpecifier ParameterDeclarator { $$=391; }
                    | TypeSpecifier TOK_REGISTER ParameterDeclarator { $$=392; }
                    | UnqualifiedDeclarator %merge<merge> { $$=393; }
                    | TOK_REGISTER UnqualifiedDeclarator { $$=394; }
                    ;

ParameterDeclarator: UnqualifiedDeclarator %merge<merge> { $$=395; }
                   | UnqualifiedDeclarator TOK_EQUAL AssignmentExpression { $$=396; }
                   | AbstractDeclaratorOpt %merge<merge> { $$=397; }
                   | AbstractDeclaratorOpt TOK_EQUAL AssignmentExpression { $$=398; }
                   ;

FunctionDefinition: DeclSpecifier FDDeclarator FunctionBody %merge<merge> { $$=399; }
                  | DeclSpecifier FDDeclarator TOK_TRY FunctionBody HandlerSeq %merge<merge> { $$=400; }
                  | CDtorModifierSeq FDDeclarator CtorInitializerOpt FunctionBody %merge<merge> { $$=401; }
                  | FDDeclarator CtorInitializerOpt FunctionBody %merge<merge> { $$=402; }
                  | CDtorModifierSeq FDDeclarator TOK_TRY CtorInitializerOpt FunctionBody HandlerSeq %merge<merge> { $$=403; }
                  | FDDeclarator TOK_TRY CtorInitializerOpt FunctionBody HandlerSeq %merge<merge> { $$=404; }
                  ;

FDDeclarator: Declarator { $$=405; }
            ;

FunctionBody: CompoundStatement { $$=406; }
            ;

CtorInitializerOpt: /* empty */ { $$=407; }
                  | TOK_COLON MemInitializerList { $$=408; }
                  ;

ClassSpecifier: ClassKey ClassHeadNameOpt BaseClauseOpt TOK_LBRACE MemberDeclarationSeqOpt TOK_RBRACE { $$=409; }
              | ClassKey AttributeSpecifierList ClassHeadNameOpt BaseClauseOpt TOK_LBRACE MemberDeclarationSeqOpt TOK_RBRACE { $$=410; }
              ;

ClassHeadNameOpt: /* empty */ { $$=411; }
                | ClassHeadName { $$=412; }
                ;

ClassHeadName: Identifier %prec TOK_COLONCOLON { $$=413; }
             | TemplateId %prec TOK_COLONCOLON { $$=414; }
             | Identifier TOK_COLONCOLON ClassHeadName %prec TOK_COLONCOLON { $$=415; }
             | Identifier TOK_LESSTHAN TemplateArgumentListOpt TOK_GREATERTHAN TOK_COLONCOLON ClassHeadName %prec TOK_COLONCOLON { $$=416; }
             | TOK_TEMPLATE Identifier TOK_LESSTHAN TemplateArgumentListOpt TOK_GREATERTHAN TOK_COLONCOLON ClassHeadName %prec TOK_COLONCOLON { $$=417; }
             ;

ClassKey: TOK_CLASS { $$=418; }
        | TOK_STRUCT { $$=419; }
        | TOK_UNION { $$=420; }
        ;

MemberDeclarationSeqOpt: /* empty */ { $$=421; }
                       | MemberDeclarationSeqOpt TOK_SEMICOLON { $$=422; }
                       | MemberDeclarationSeqOpt MemberDeclaration { $$=423; }
                       | MemberDeclarationSeqOpt AccessSpecifier TOK_COLON { $$=424; }
                       ;

AccessSpecifier: TOK_PUBLIC { $$=425; }
               | TOK_PRIVATE { $$=426; }
               | TOK_PROTECTED { $$=427; }
               ;

MemberDeclaration: DeclSpecifier MemberDeclaratorList TOK_SEMICOLON %merge<merge> { $$=428; }
                 | DeclSpecifier TOK_SEMICOLON { $$=429; }
                 | PQualifiedId TOK_SEMICOLON { $$=430; }
                 | TOK_USING IdExpression TOK_SEMICOLON { $$=431; }
                 | FunctionDefinition { $$=432; }
                 | CDtorProtoDecl %merge<merge> { $$=433; }
                 | TemplateDeclaration { $$=434; }
                 ;

CDtorProtoDecl: CDtorModifierSeq MemberDeclarator TOK_SEMICOLON { $$=435; }
              | MemberDeclarator TOK_SEMICOLON { $$=436; }
              ;

MemberDeclaratorList: MemberDeclarator { $$=437; }
                    | MemberDeclarator TOK_COMMA MemberDeclaratorList { $$=438; }
                    ;

MemberDeclarator: Declarator { $$=439; }
                | Declarator TOK_EQUAL ConstantExpression { $$=440; }
                | IdentifierOpt TOK_COLON ConstantExpression { $$=441; }
                | IdentifierOpt TOK_COLON ConstantExpression AttributeSpecifierList { $$=442; }
                ;

IdentifierOpt: /* empty */ { $$=443; }
             | Identifier { $$=444; }
             ;

CDtorModifier: TOK_EXPLICIT { $$=445; }
             | TOK_VIRTUAL { $$=446; }
             | TOK_INLINE { $$=447; }
             | TOK_FRIEND { $$=448; }
             | AttributeSpecifier { $$=449; }
             ;

CDtorModifierSeq: CDtorModifier { $$=450; }
                | CDtorModifierSeq CDtorModifier { $$=451; }
                ;

BaseClauseOpt: /* empty */ { $$=452; }
             | TOK_COLON BaseSpecifierList { $$=453; }
             ;

BaseSpecifierList: BaseSpecifier { $$=454; }
                 | BaseSpecifier TOK_COMMA BaseSpecifierList { $$=455; }
                 ;

BaseSpecifier: PQClassName { $$=456; }
             | TOK_VIRTUAL AccessSpecifierOpt PQClassName { $$=457; }
             | AccessSpecifier VirtualOpt PQClassName { $$=458; }
             ;

VirtualOpt: /* empty */ { $$=459; }
          | TOK_VIRTUAL { $$=460; }
          ;

AccessSpecifierOpt: /* empty */ { $$=461; }
                  | AccessSpecifier { $$=462; }
                  ;

PQClassName: PQTypeName { $$=463; }
           ;

ConversionFunctionId: TOK_OPERATOR ConversionTypeId { $$=464; }
                    ;

ConversionTypeId: TypeSpecifier ConversionDeclaratorOpt { $$=465; }
                ;

ConversionDeclaratorOpt: /* empty */ %prec TOK_PREFER_SHIFT { $$=466; }
                       | TOK_STAR CVQualifierSeqOpt ConversionDeclaratorOpt %prec TOK_STAR { $$=467; }
                       | TOK_AND CVQualifierSeqOpt ConversionDeclaratorOpt %prec TOK_AND { $$=468; }
                       | PtrToMemberName TOK_STAR CVQualifierSeqOpt ConversionDeclaratorOpt %prec TOK_STAR { $$=469; }
                       ;

MemInitializerList: MemInitializer { $$=470; }
                  | MemInitializer TOK_COMMA MemInitializerList { $$=471; }
                  ;

MemInitializer: MemInitializerId TOK_LPAREN ExpressionListOpt TOK_RPAREN { $$=472; }
              ;

MemInitializerId: PQTypeName { $$=473; }
                ;

OperatorFunctionId: TOK_OPERATOR Operator { $$=474; }
                  ;

Operator: TOK_NEW %prec TOK_PREFER_SHIFT { $$=475; }
        | TOK_DELETE %prec TOK_PREFER_SHIFT { $$=476; }
        | TOK_NEW TOK_LBRACKET TOK_RBRACKET { $$=477; }
        | TOK_DELETE TOK_LBRACKET TOK_RBRACKET { $$=478; }
        | TOK_BANG { $$=479; }
        | TOK_TILDE { $$=480; }
        | TOK_PLUSPLUS { $$=481; }
        | TOK_MINUSMINUS { $$=482; }
        | TOK_PLUS %prec TOK_PLUS { $$=483; }
        | TOK_MINUS %prec TOK_PLUS { $$=484; }
        | TOK_STAR %prec TOK_STAR { $$=485; }
        | TOK_SLASH %prec TOK_STAR { $$=486; }
        | TOK_PERCENT %prec TOK_STAR { $$=487; }
        | TOK_LEFTSHIFT %prec TOK_LEFTSHIFT { $$=488; }
        | TOK_RIGHTSHIFT %prec TOK_LEFTSHIFT { $$=489; }
        | TOK_AND %prec TOK_AND { $$=490; }
        | TOK_XOR %prec TOK_XOR { $$=491; }
        | TOK_OR %prec TOK_OR { $$=492; }
        | TOK_EQUAL { $$=493; }
        | TOK_PLUSEQUAL { $$=494; }
        | TOK_MINUSEQUAL { $$=495; }
        | TOK_STAREQUAL { $$=496; }
        | TOK_SLASHEQUAL { $$=497; }
        | TOK_PERCENTEQUAL { $$=498; }
        | TOK_LEFTSHIFTEQUAL { $$=499; }
        | TOK_RIGHTSHIFTEQUAL { $$=500; }
        | TOK_ANDEQUAL { $$=501; }
        | TOK_XOREQUAL { $$=502; }
        | TOK_OREQUAL { $$=503; }
        | TOK_EQUALEQUAL %prec TOK_EQUALEQUAL { $$=504; }
        | TOK_NOTEQUAL %prec TOK_EQUALEQUAL { $$=505; }
        | TOK_LESSTHAN { $$=506; }
        | TOK_GREATERTHAN { $$=507; }
        | TOK_LESSEQ { $$=508; }
        | TOK_GREATEREQ { $$=509; }
        | TOK_ANDAND %prec TOK_ANDAND { $$=510; }
        | TOK_OROR %prec TOK_OROR { $$=511; }
        | TOK_ARROW { $$=512; }
        | TOK_ARROWSTAR %prec TOK_DOTSTAR { $$=513; }
        | TOK_LBRACKET TOK_RBRACKET { $$=514; }
        | TOK_LPAREN TOK_RPAREN { $$=515; }
        | TOK_COMMA { $$=516; }
        | TOK_MIN_OP { $$=517; }
        | TOK_MAX_OP { $$=518; }
        ;

TemplateDeclaration: TemplatePreamble FunctionDefinition { $$=519; }
                   | TemplatePreamble SimpleDeclaration %merge<merge> { $$=520; }
                   | TemplatePreamble TemplateDeclaration { $$=521; }
                   | TemplatePreamble CDtorProtoDecl %merge<merge> { $$=522; }
                   ;

TemplatePreamble: TOK_TEMPLATE TOK_LESSTHAN TemplateParameterList TOK_GREATERTHAN { $$=523; }
                | TOK_EXPORT TOK_TEMPLATE TOK_LESSTHAN TemplateParameterList TOK_GREATERTHAN { $$=524; }
                | TOK_TEMPLATE TOK_LESSTHAN TOK_GREATERTHAN { $$=525; }
                | TOK_EXPORT TOK_TEMPLATE TOK_LESSTHAN TOK_GREATERTHAN { $$=526; }
                ;

TemplateParameterList: TemplateParameter { $$=527; }
                     | TemplateParameterList TOK_COMMA TemplateParameter { $$=527; }
                     ;

TemplateParameter: ClassOrTypename IdentifierOpt DefaultTypeOpt %merge<merge> { $$=527; }
                     | ParameterDeclaration %merge<merge> { $$=528; }
                     | TOK_TEMPLATE TOK_LESSTHAN TemplateParameterList TOK_GREATERTHAN TOK_CLASS IdentifierOpt DefaultTemplateOpt { $$=529; }
                     ;

ClassOrTypename: TOK_CLASS { $$=532; }
               | TOK_TYPENAME { $$=533; }
               ;

DefaultTypeOpt: /* empty */ { $$=534; }
              | TOK_EQUAL TypeId { $$=535; }
              ;

DefaultTemplateOpt: /* empty */ { $$=536; }
                  | TOK_EQUAL IdExpression { $$=537; }
                  ;

TemplateArgumentListOpt: /* empty */ { $$=538; }
                       | TemplateArgumentList { $$=539; }
                       ;

TemplateId: Identifier TOK_LESSTHAN TemplateArgumentListOpt TOK_GREATERTHAN { $$=540; }
          | OperatorFunctionId TOK_LESSTHAN TemplateArgumentListOpt TOK_GREATERTHAN { $$=541; }
          ;

TemplateArgumentList: TemplateArgument %merge<merge> { $$=542; }
                    | TemplateArgumentList TOK_COMMA TemplateArgument %merge<merge> { $$=542; }
                    ;

TemplateArgument: TypeId %merge<merge> { $$=545; }
                | AssignmentExpression %merge<merge> { $$=546; }
                ;

ExplicitInstantiation: TOK_TEMPLATE BlockDeclaration { $$=547; }
                     | TOK_INLINE TOK_TEMPLATE BlockDeclaration { $$=548; }
                     | TOK_EXTERN TOK_TEMPLATE BlockDeclaration { $$=549; }
                     ;

TryBlock: TOK_TRY CompoundStatement HandlerSeq { $$=550; }
        ;

HandlerSeq: Handler { $$=551; }
          | Handler HandlerSeq { $$=552; }
          ;

Handler: TOK_CATCH TOK_LPAREN HandlerParameter TOK_RPAREN CompoundStatement { $$=553; }
       | TOK_CATCH TOK_LPAREN TOK_ELLIPSIS TOK_RPAREN CompoundStatement { $$=554; }
       ;

HandlerParameter: TypeSpecifier UnqualifiedDeclarator { $$=555; }
                | TypeSpecifier AbstractDeclaratorOpt { $$=556; }
                ;

UnqualifiedDeclarator: Declarator { $$=557; }
                     ;

ThrowExpression: TOK_THROW { $$=558; }
               | TOK_THROW AssignmentExpression { $$=559; }
               ;

ExceptionSpecificationOpt: /* empty */ { $$=560; }
                         | TOK_THROW TOK_LPAREN TOK_RPAREN { $$=561; }
                         | TOK_THROW TOK_LPAREN TypeIdList TOK_RPAREN { $$=562; }
                         ;

TypeIdList: TypeId { $$=563; }
          | TypeId TOK_COMMA TypeIdList { $$=564; }
          ;

NamespaceDefinition: TOK_NAMESPACE IdentifierOpt TOK_LBRACE TranslationUnit TOK_RBRACE { $$=565; }
                   | TOK_NAMESPACE IdentifierOpt AttributeSpecifierList TOK_LBRACE TranslationUnit TOK_RBRACE { $$=566; }
                   ;

NamespaceDecl: TOK_NAMESPACE Identifier TOK_EQUAL IdExpression TOK_SEMICOLON { $$=567; }
             | TOK_USING IdExpression TOK_SEMICOLON { $$=568; }
             | TOK_USING TOK_NAMESPACE IdExpression TOK_SEMICOLON { $$=569; }
             | TOK_USING TOK_NAMESPACE IdExpression AttributeSpecifierList TOK_SEMICOLON { $$=570; }
             ;

AnnotationOpt: /* empty */ { $$=571; }
             | AnnotationList { $$=572; }
             ;

AnnotationList: TOK_ANNOTATION { $$=573; }
              | TOK_ANNOTATION AnnotationList { $$=574; }
              ;

NamesAfterDot: NameAfterDot { $$=575; }
             | NamesAfterDot TOK_DOT NameAfterDot { $$=576; }
             | NamesAfterDot TOK_LBRACKET Expression TOK_RBRACKET { $$=577; }
             ;

ParenthesizedExpression: TOK_LPAREN Expression TOK_RPAREN { $$=578; }
                       ;

LabeledEmptyStatementList: LabelAndColon LabeledEmptyStatementListOpt { $$=579; }
                         | TOK_CASE ConstantExpression TOK_COLON LabeledEmptyStatementListOpt { $$=580; }
                         | TOK_CASE ConstantExpression TOK_ELLIPSIS ConstantExpression TOK_COLON LabeledEmptyStatementListOpt { $$=581; }
                         | TOK_DEFAULT TOK_COLON LabeledEmptyStatementListOpt { $$=582; }
                         ;

LabeledEmptyStatementListOpt: /* empty */ { $$=583; }
                            | LabeledEmptyStatementList { $$=584; }
                            ;

TypeofTypeSpecifier: TypeofExpr %merge<merge> { $$=585; }
                   | TypeofType %merge<merge> { $$=586; }
                   ;

TypeofExpr: TOK___TYPEOF__ TOK_LPAREN Expression TOK_RPAREN { $$=587; }
          ;

TypeofType: TOK___TYPEOF__ TOK_LPAREN TypeId TOK_RPAREN { $$=588; }
          ;

BracketedWordOpt: /* empty */ { $$=589; }
                | TOK_LBRACKET Identifier TOK_RBRACKET { $$=590; }
                ;

ParenthesizedExpressionOpt: /* empty */ { $$=591; }
                          | TOK_LPAREN Expression TOK_RPAREN { $$=592; }
                          ;

OpConstraint: BracketedWordOpt StringLiteral ParenthesizedExpressionOpt { $$=593; }
            ;

OpConstraintList: /* empty */ { $$=594; }
                | OpConstraint { $$=595; }
                | OpConstraint TOK_COMMA OpConstraintList { $$=596; }
                ;

OpConstraints: /* empty */ { $$=597; }
             | NonemptyOpConstraints { $$=598; }
             ;

NonemptyOpConstraints: OpConstraints TOK_COLON OpConstraintList { $$=599; }
                     | OpConstraints TOK_COLONCOLON OpConstraintList %prec TOK_COLONCOLON { $$=600; }
                     ;

DesignatorList: Designator { $$=601; }
              | Designator DesignatorList { $$=602; }
              ;

Designator: TOK_DOT Identifier { $$=603; }
          | TOK_LBRACKET ConstantExpression TOK_RBRACKET { $$=604; }
          | TOK_LBRACKET ConstantExpression TOK_ELLIPSIS ConstantExpression TOK_RBRACKET { $$=605; }
          ;

BuggyGccTypeModifier: TOK_LONG { $$=606; }
                    | TOK_SHORT { $$=607; }
                    | TOK_SIGNED { $$=608; }
                    | TOK_UNSIGNED { $$=609; }
                    | TOK_LONG BuggyGccTypeModifier { $$=610; }
                    | TOK_SHORT BuggyGccTypeModifier { $$=611; }
                    | TOK_SIGNED BuggyGccTypeModifier { $$=612; }
                    | TOK_UNSIGNED BuggyGccTypeModifier { $$=613; }
                    ;

CVQualAttrSeqOpt: /* empty */ { $$=614; }
                | CVQualAttrSeq { $$=615; }
                ;

CVQualAttrSeq: CVQualAttr { $$=616; }
             | CVQualAttr CVQualAttrSeq { $$=617; }
             ;

CVQualAttr: CVQualifier { $$=618; }
          | AttributeSpecifier { $$=619; }
          ;

AttributeWord: TOK_NAME { $$=620; }
             | TOK_ASM { $$=621; }
             | TOK_AUTO { $$=622; }
             | TOK_BREAK { $$=623; }
             | TOK_BOOL { $$=624; }
             | TOK_CASE { $$=625; }
             | TOK_CATCH { $$=626; }
             | TOK_CDECL { $$=627; }
             | TOK_CHAR { $$=628; }
             | TOK_CLASS { $$=629; }
             | TOK_CONST %prec TOK_CONST { $$=630; }
             | TOK_CONST_CAST { $$=631; }
             | TOK_CONTINUE { $$=632; }
             | TOK_DEFAULT { $$=633; }
             | TOK_DELETE { $$=634; }
             | TOK_DO { $$=635; }
             | TOK_DOUBLE { $$=636; }
             | TOK_DYNAMIC_CAST { $$=637; }
             | TOK_ELSE %prec TOK_CONST { $$=638; }
             | TOK_ENUM { $$=639; }
             | TOK_EXPLICIT { $$=640; }
             | TOK_EXPORT { $$=641; }
             | TOK_EXTERN { $$=642; }
             | TOK_FALSE { $$=643; }
             | TOK_FLOAT { $$=644; }
             | TOK_FOR { $$=645; }
             | TOK_FRIEND { $$=646; }
             | TOK_GOTO { $$=647; }
             | TOK_IF { $$=648; }
             | TOK_INLINE { $$=649; }
             | TOK_INT { $$=650; }
             | TOK_LONG { $$=651; }
             | TOK_MUTABLE { $$=652; }
             | TOK_NAMESPACE { $$=653; }
             | TOK_NEW { $$=654; }
             | TOK_OPERATOR { $$=655; }
             | TOK_PASCAL { $$=656; }
             | TOK_PRIVATE { $$=657; }
             | TOK_PROTECTED { $$=658; }
             | TOK_PUBLIC { $$=659; }
             | TOK_REGISTER { $$=660; }
             | TOK_REINTERPRET_CAST { $$=661; }
             | TOK_RETURN { $$=662; }
             | TOK_SHORT { $$=663; }
             | TOK_SIGNED { $$=664; }
             | TOK_SIZEOF { $$=665; }
             | TOK_STATIC { $$=666; }
             | TOK_STATIC_CAST { $$=667; }
             | TOK_STRUCT { $$=668; }
             | TOK_SWITCH { $$=669; }
             | TOK_TEMPLATE { $$=670; }
             | TOK_THIS { $$=671; }
             | TOK_THROW { $$=672; }
             | TOK_TRUE { $$=673; }
             | TOK_TRY { $$=674; }
             | TOK_TYPEDEF { $$=675; }
             | TOK_TYPEID { $$=676; }
             | TOK_TYPENAME { $$=677; }
             | TOK_UNION { $$=678; }
             | TOK_UNSIGNED { $$=679; }
             | TOK_USING { $$=680; }
             | TOK_VIRTUAL { $$=681; }
             | TOK_VOID { $$=682; }
             | TOK_VOLATILE %prec TOK_CONST { $$=683; }
             | TOK_WCHAR_T { $$=684; }
             | TOK_WHILE { $$=685; }
             ;

CommaSepExpressionListOpt: /* empty */ { $$=686; }
                         | ExpressionList { $$=687; }
                         ;

AttributeParameters: CommaSepExpressionListOpt { $$=688; }
                   ;

Attribute: /* empty */ { $$=689; }
         | AttributeWord { $$=690; }
         | AttributeWord TOK_LPAREN AttributeParameters TOK_RPAREN { $$=691; }
         ;

AttributeList: Attribute { $$=692; }
             | Attribute TOK_COMMA AttributeList { $$=693; }
             ;

AttributeSpecifier: TOK___ATTRIBUTE__ TOK_LPAREN TOK_LPAREN AttributeList TOK_RPAREN TOK_RPAREN { $$=694; }
                  ;

AttributeSpecifierList: AttributeSpecifier { $$=695; }
                      | AttributeSpecifier AttributeSpecifierList { $$=696; }
                      ;

KandRFunctionDefinition: DeclSpecifier KandRDeclarator KandRSimpleDeclarationSeq FunctionBody { $$=697; }
                       ;

KandRFunctionDefinition_implInt: KandRDeclarator KandRSimpleDeclarationSeq FunctionBody { $$=698; }
                               | UberModifierSeq KandRDeclarator KandRSimpleDeclarationSeq FunctionBody { $$=699; }
                               ;

KandRSimpleDeclarationSeq: KandRSimpleDeclaration { $$=700; }
                         | KandRSimpleDeclarationSeq KandRSimpleDeclaration { $$=701; }
                         ;

KandRSimpleDeclaration: DeclSpecifier KandRInitDeclaratorList TOK_SEMICOLON { $$=702; }
                      | TOK_REGISTER KandRInitDeclaratorList TOK_SEMICOLON { $$=703; }
                      ;

KandRInitDeclaratorList: KandRInitDeclarator { $$=704; }
                       | KandRInitDeclarator TOK_COMMA KandRInitDeclaratorList { $$=705; }
                       ;

KandRInitDeclarator: Declarator { $$=706; }
                   ;

KandRDeclarator: TOK_STAR CVQualifierSeqOpt KandRDeclarator %prec TOK_STAR { $$=707; }
               | KandRDirectDeclarator { $$=708; }
               ;

KandRDirectDeclarator: KandRIdExpression TOK_LPAREN KandRIdentifierList TOK_RPAREN { $$=709; }
                     | TOK_LPAREN KandRIdExpression TOK_RPAREN TOK_LPAREN KandRIdentifierList TOK_RPAREN { $$=710; }
                     | KandRDirectDeclarator TOK_LPAREN ParameterDeclarationClause TOK_RPAREN { $$=711; }
                     | KandRDirectDeclarator TOK_LBRACKET ConstantExpressionOpt TOK_RBRACKET { $$=712; }
                     | TOK_LPAREN KandRDeclarator TOK_RPAREN { $$=713; }
                     ;

KandRIdExpression: KandRIdentifier { $$=714; }
                 ;

KandRIdentifierList: KandRIdentifier { $$=715; }
                   | KandRIdentifier TOK_COMMA KandRIdentifierList { $$=716; }
                   ;

KandRIdentifier: Identifier { $$=717; }
               ;

CFuncModifier_no_inline: TOK_STATIC { $$=718; }
                       | TOK_EXTERN { $$=719; }
                       | TOK_CONST %prec TOK_CONST { $$=720; }
                       | TOK_VOLATILE %prec TOK_CONST { $$=721; }
                       ;

CFuncModifier: CFuncModifier_no_inline { $$=722; }
             | TOK_INLINE { $$=723; }
             ;

CFuncModifierSeq: CFuncModifier { $$=724; }
                | CFuncModifierSeq CFuncModifier { $$=725; }
                ;

CFuncModifierSeqOpt: /* empty */ { $$=726; }
                   | CFuncModifier { $$=727; }
                   ;

ImplIntFunctionDefinition: CFuncModifier_no_inline Declarator FunctionBody { $$=728; }
                         | CFuncModifier_no_inline CFuncModifierSeq Declarator FunctionBody { $$=729; }
                         | TOK_INLINE CFuncModifier_no_inline CFuncModifierSeqOpt Declarator FunctionBody { $$=730; }
                         ;

ImplicitIntTypeSpecifier: UberCVQualifierSeq { $$=731; }
                        ;

ImplicitIntTypeId: ImplicitIntTypeSpecifier { $$=732; }
                 ;

