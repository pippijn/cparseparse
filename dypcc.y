%token TOK_EOF
%token TOK_NAME
%token TOK_TYPE_NAME
%token TOK_VARIABLE_NAME
%token TOK_INT_LITERAL
%token TOK_FLOAT_LITERAL
%token TOK_STRING_LITERAL
%token TOK_CHAR_LITERAL
%token TOK_ASM
%token TOK_AUTO
%token TOK_BREAK
%token TOK_BOOL
%token TOK_CASE
%token TOK_CATCH
%token TOK_CDECL
%token TOK_CHAR
%token TOK_CLASS
%token TOK_CONST
%token TOK_CONST_CAST
%token TOK_CONTINUE
%token TOK_DEFAULT
%token TOK_DELETE
%token TOK_DO
%token TOK_DOUBLE
%token TOK_DYNAMIC_CAST
%token TOK_ELSE
%token TOK_ENUM
%token TOK_EXPLICIT
%token TOK_EXPORT
%token TOK_EXTERN
%token TOK_FALSE
%token TOK_FLOAT
%token TOK_FOR
%token TOK_FRIEND
%token TOK_GOTO
%token TOK_IF
%token TOK_INLINE
%token TOK_INT
%token TOK_LONG
%token TOK_MUTABLE
%token TOK_NAMESPACE
%token TOK_NEW
%token TOK_OPERATOR
%token TOK_PASCAL
%token TOK_PRIVATE
%token TOK_PROTECTED
%token TOK_PUBLIC
%token TOK_REGISTER
%token TOK_REINTERPRET_CAST
%token TOK_RETURN
%token TOK_SHORT
%token TOK_SIGNED
%token TOK_SIZEOF
%token TOK_STATIC
%token TOK_STATIC_CAST
%token TOK_STRUCT
%token TOK_SWITCH
%token TOK_TEMPLATE
%token TOK_THIS
%token TOK_THROW
%token TOK_TRUE
%token TOK_TRY
%token TOK_TYPEDEF
%token TOK_TYPEID
%token TOK_TYPENAME
%token TOK_UNION
%token TOK_UNSIGNED
%token TOK_USING
%token TOK_VIRTUAL
%token TOK_VOID
%token TOK_VOLATILE
%token TOK_WCHAR_T
%token TOK_WHILE
%token TOK_LPAREN
%token TOK_RPAREN
%token TOK_LBRACKET
%token TOK_RBRACKET
%token TOK_ARROW
%token TOK_COLONCOLON
%token TOK_DOT
%token TOK_BANG
%token TOK_TILDE
%token TOK_PLUS
%token TOK_MINUS
%token TOK_PLUSPLUS
%token TOK_MINUSMINUS
%token TOK_AND
%token TOK_STAR
%token TOK_DOTSTAR
%token TOK_ARROWSTAR
%token TOK_SLASH
%token TOK_PERCENT
%token TOK_LEFTSHIFT
%token TOK_RIGHTSHIFT
%token TOK_LESSTHAN
%token TOK_LESSEQ
%token TOK_GREATERTHAN
%token TOK_GREATEREQ
%token TOK_EQUALEQUAL
%token TOK_NOTEQUAL
%token TOK_XOR
%token TOK_OR
%token TOK_ANDAND
%token TOK_OROR
%token TOK_QUESTION
%token TOK_COLON
%token TOK_EQUAL
%token TOK_STAREQUAL
%token TOK_SLASHEQUAL
%token TOK_PERCENTEQUAL
%token TOK_PLUSEQUAL
%token TOK_MINUSEQUAL
%token TOK_ANDEQUAL
%token TOK_XOREQUAL
%token TOK_OREQUAL
%token TOK_LEFTSHIFTEQUAL
%token TOK_RIGHTSHIFTEQUAL
%token TOK_COMMA
%token TOK_ELLIPSIS
%token TOK_SEMICOLON
%token TOK_LBRACE
%token TOK_RBRACE
%token TOK_ANNOTATION
%token TOK_PREFER_REDUCE
%token TOK_PREFER_SHIFT
%token TOK_BUILTIN_CONSTANT_P
%token TOK___ALIGNOF__
%token TOK___OFFSETOF__
%token TOK___BUILTIN_OFFSETOF
%token TOK___ATTRIBUTE__
%token TOK___FUNCTION__
%token TOK___LABEL__
%token TOK___PRETTY_FUNCTION__
%token TOK___TYPEOF__
%token TOK___EXTENSION__
%token TOK___BUILTIN_EXPECT
%token TOK___BUILTIN_VA_ARG
%token TOK_MIN_OP
%token TOK_MAX_OP
%token TOK_REAL
%token TOK_IMAG
%token TOK_RESTRICT
%token TOK_COMPLEX
%token TOK_IMAGINARY


%start file

%%

file: translation_unit TOK_EOF { 1; }


identifier: TOK_NAME { 2; }


translation_unit: /* empty */ { 3; }
               | translation_unit declaration { 4; }
               | translation_unit TOK_SEMICOLON { 5; }


primaryExpression: literal { 6; }
                 | TOK_THIS { 7; }
                 | TOK_LPAREN expression TOK_RPAREN { 8; }
                 | idExpression { 9; }
                 | TOK_LPAREN compoundStatement TOK_RPAREN { 10; }
                 | TOK_LPAREN typeId TOK_RPAREN compoundInitializer { 11; }
                 | TOK_REAL primaryExpression { 12; }
                 | TOK_IMAG primaryExpression { 13; }


literal: TOK_INT_LITERAL { 14; }
       | TOK_FLOAT_LITERAL { 15; }
       | stringLiteral { 16; }
       | TOK_CHAR_LITERAL { 17; }
       | TOK_TRUE { 18; }
       | TOK_FALSE { 19; }


preprocString: TOK_STRING_LITERAL { 20; }
             | TOK___FUNCTION__ { 21; }
             | TOK___PRETTY_FUNCTION__ { 22; }


stringLiteral: preprocString { 23; }
             | preprocString stringLiteral { 24; }


idExpression: p_qualifiedId { 25; }
            | TOK_COLONCOLON p_qualifiedId { 26; }


unqualifiedId: identifier { 27; }
             | operatorFunctionId { 28; }
             | conversionFunctionId { 29; }
             | templateId { 30; }


p_qualifiedId: unqualifiedId { 31; }
            | identifier TOK_COLONCOLON p_qualifiedId { 32; }
            | identifier TOK_LESSTHAN templateArgumentListOpt TOK_GREATERTHAN TOK_COLONCOLON p_qualifiedId { 33; }
            | TOK_TEMPLATE identifier TOK_LESSTHAN templateArgumentListOpt TOK_GREATERTHAN TOK_COLONCOLON p_qualifiedId { 34; }


argumentList: TOK_LPAREN expressionListOpt TOK_RPAREN { 35; }


postfixExpression: primaryExpression { 36; }
                 | postfixExpression TOK_LBRACKET expression TOK_RBRACKET { 37; }
                 | postfixExpression argumentList { 38; }
                 | TOK_TYPENAME idExpression argumentList { 39; }
                 | ctorExpressionType argumentList { 40; }
                 | postfixExpression TOK_DOT nameAfterDot { 41; }
                 | postfixExpression TOK_ARROW nameAfterDot { 42; }
                 | postfixExpression TOK_PLUSPLUS { 43; }
                 | postfixExpression TOK_MINUSMINUS { 44; }
                 | castKeyword TOK_LESSTHAN typeId TOK_GREATERTHAN TOK_LPAREN expression TOK_RPAREN { 45; }
                 | TOK_TYPEID TOK_LPAREN expression TOK_RPAREN { 46; }
                 | TOK_TYPEID TOK_LPAREN typeId TOK_RPAREN { 47; }
                 | TOK_BUILTIN_CONSTANT_P parenthesizedExpression { 48; }
                 | TOK___ALIGNOF__ TOK_LPAREN typeId TOK_RPAREN { 49; }
                 | TOK___ALIGNOF__ TOK_LPAREN expression TOK_RPAREN { 50; }
                 | TOK___BUILTIN_OFFSETOF TOK_LPAREN typeId TOK_COMMA namesAfterDot TOK_RPAREN { 51; }
                 | TOK___OFFSETOF__ TOK_LPAREN expression TOK_RPAREN { 52; }
                 | TOK___BUILTIN_EXPECT TOK_LPAREN expression TOK_COMMA expression TOK_RPAREN { 53; }
                 | TOK___BUILTIN_VA_ARG TOK_LPAREN expression TOK_COMMA typeId TOK_RPAREN { 54; }


ctorExpressionType: pq_type_name { 55; }
                  | TOK_CHAR { 56; }
                  | TOK_WCHAR_T { 57; }
                  | TOK_BOOL { 58; }
                  | TOK_SHORT { 59; }
                  | TOK_INT { 60; }
                  | TOK_LONG { 61; }
                  | TOK_SIGNED { 62; }
                  | TOK_UNSIGNED { 63; }
                  | TOK_FLOAT { 64; }
                  | TOK_DOUBLE { 65; }
                  | TOK_VOID { 66; }


castKeyword: TOK_DYNAMIC_CAST { 67; }
           | TOK_STATIC_CAST { 68; }
           | TOK_REINTERPRET_CAST { 69; }
           | TOK_CONST_CAST { 70; }


expressionList: assignmentExpression { 71; }
              | assignmentExpression TOK_COMMA expressionList { 72; }


expressionListOpt: /* empty */ { 73; }
                 | expressionList { 74; }


unaryExpression: postfixExpression { 75; }
               | TOK_PLUSPLUS castExpression { 76; }
               | TOK_MINUSMINUS castExpression { 77; }
               | TOK_SIZEOF unaryExpression { 78; }
               | deleteExpression { 79; }
               | TOK_STAR castExpression { 80; }
               | TOK_AND castExpression { 81; }
               | TOK_PLUS castExpression { 82; }
               | TOK_MINUS castExpression { 83; }
               | TOK_BANG castExpression { 84; }
               | TOK_TILDE castExpression { 85; }
               | TOK_SIZEOF TOK_LPAREN typeId TOK_RPAREN { 86; }
               | newExpression { 87; }
               | TOK_ANDAND identifier { 88; }


colonColonOpt: /* empty */ { 89; }
             | TOK_COLONCOLON { 90; }


newExpression: colonColonOpt TOK_NEW newPlacementOpt newTypeId newInitializerOpt { 91; }
             | colonColonOpt TOK_NEW newPlacementOpt TOK_LPAREN typeId TOK_RPAREN newInitializerOpt { 92; }


newPlacementOpt: /* empty */ { 93; }
               | TOK_LPAREN expressionList TOK_RPAREN { 94; }


newTypeId: typeSpecifier newDeclaratorOpt { 95; }


newDeclaratorOpt: /* empty */ { 96; }
                | TOK_STAR cv_qualifierSeqOpt newDeclaratorOpt { 97; }
                | ptrToMemberName TOK_STAR cv_qualifierSeqOpt newDeclaratorOpt { 98; }
                | directNewDeclarator { 99; }


directNewDeclarator: TOK_LBRACKET expression TOK_RBRACKET { 100; }
                   | directNewDeclarator TOK_LBRACKET constantExpression TOK_RBRACKET { 101; }


newInitializerOpt: /* empty */ { 102; }
                 | TOK_LPAREN expressionListOpt TOK_RPAREN { 103; }


deleteExpression: colonColonOpt TOK_DELETE castExpression { 104; }
                | colonColonOpt TOK_DELETE TOK_LBRACKET TOK_RBRACKET castExpression { 105; }


nameAfterDot: nad1 { 106; }
            | TOK_COLONCOLON nad2 { 107; }


nad1: nad2 { 108; }
    | TOK_TEMPLATE identifier TOK_LESSTHAN templateArgumentListOpt TOK_GREATERTHAN { 109; }
    | TOK_TILDE identifier { 110; }
    | TOK_TILDE identifier TOK_LESSTHAN templateArgumentListOpt TOK_GREATERTHAN { 111; }
    | conversionFunctionId { 112; }
    | TOK_TEMPLATE identifier TOK_LESSTHAN templateArgumentListOpt TOK_GREATERTHAN TOK_COLONCOLON nad1 { 113; }


nad2: identifier TOK_LESSTHAN templateArgumentListOpt TOK_GREATERTHAN { 114; }
    | identifier { 115; }
    | operatorFunctionId { 116; }
    | operatorFunctionId TOK_LESSTHAN templateArgumentListOpt TOK_GREATERTHAN { 117; }
    | TOK_TEMPLATE operatorFunctionId TOK_LESSTHAN templateArgumentListOpt TOK_GREATERTHAN { 118; }
    | identifier TOK_LESSTHAN templateArgumentListOpt TOK_GREATERTHAN TOK_COLONCOLON nad1 { 119; }
    | identifier TOK_COLONCOLON nad1 { 120; }


castExpression: unaryExpression { 121; }
              | TOK_LPAREN typeId TOK_RPAREN castExpression { 122; }
              | TOK_LPAREN implicitIntTypeId TOK_RPAREN castExpression { 123; }


binExp_high: castExpression { 124; }
           | binExp_high TOK_DOTSTAR binExp_high { 125; }
           | binExp_high TOK_ARROWSTAR binExp_high { 126; }
           | binExp_high TOK_STAR binExp_high { 127; }
           | binExp_high TOK_SLASH binExp_high { 128; }
           | binExp_high TOK_PERCENT binExp_high { 129; }
           | binExp_high TOK_PLUS binExp_high { 130; }
           | binExp_high TOK_MINUS binExp_high { 131; }
           | binExp_high TOK_LEFTSHIFT binExp_high { 132; }
           | binExp_high TOK_RIGHTSHIFT binExp_high { 133; }


binExp_mid: binExp_high { 134; }
          | binExp_mid TOK_LESSTHAN binExp_high { 135; }
          | binExp_mid TOK_GREATERTHAN binExp_high { 136; }
          | binExp_mid TOK_LESSEQ binExp_high { 137; }
          | binExp_mid TOK_GREATEREQ binExp_high { 138; }
          | binExp_mid TOK_MIN_OP binExp_high { 139; }
          | binExp_mid TOK_MAX_OP binExp_high { 140; }


binaryExpression: binExp_mid { 141; }
                | binaryExpression TOK_EQUALEQUAL binaryExpression { 142; }
                | binaryExpression TOK_NOTEQUAL binaryExpression { 143; }
                | binaryExpression TOK_AND binaryExpression { 144; }
                | binaryExpression TOK_XOR binaryExpression { 145; }
                | binaryExpression TOK_OR binaryExpression { 146; }
                | binaryExpression TOK_ANDAND binaryExpression { 147; }
                | binaryExpression TOK_OROR binaryExpression { 148; }


conditionalExpression: binaryExpression { 149; }
                     | binaryExpression TOK_QUESTION expression TOK_COLON assignmentExpression { 150; }
                     | binaryExpression TOK_QUESTION TOK_COLON assignmentExpression { 151; }


assignmentExpression: conditionalExpression { 152; }
                    | binaryExpression assignmentOperator assignmentExpression { 153; }
                    | throwExpression { 154; }


assignmentOperator: TOK_STAREQUAL { 155; }
                  | TOK_SLASHEQUAL { 156; }
                  | TOK_PERCENTEQUAL { 157; }
                  | TOK_PLUSEQUAL { 158; }
                  | TOK_MINUSEQUAL { 159; }
                  | TOK_RIGHTSHIFTEQUAL { 160; }
                  | TOK_LEFTSHIFTEQUAL { 161; }
                  | TOK_ANDEQUAL { 162; }
                  | TOK_XOREQUAL { 163; }
                  | TOK_OREQUAL { 164; }
                  | TOK_EQUAL { 165; }


expression: assignmentExpression { 166; }
          | expression TOK_COMMA assignmentExpression { 167; }


expressionOpt: /* empty */ { 168; }
             | expression { 169; }


constantExpression: assignmentExpression { 170; }


constantExpressionOpt: /* empty */ { 171; }
                     | constantExpression { 172; }


labelAndColon: identifier TOK_COLON { 173; }
             | identifier TOK_COLON attributeSpecifierList { 174; }


statement: labelAndColon statement { 175; }
         | TOK_CASE constantExpression TOK_COLON statement { 176; }
         | TOK_DEFAULT TOK_COLON statement { 177; }
         | expressionStatement { 178; }
         | compoundStatement { 179; }
         | TOK_IF TOK_LPAREN condition TOK_RPAREN statement { 180; }
         | TOK_IF TOK_LPAREN condition TOK_RPAREN statement TOK_ELSE statement { 181; }
         | TOK_SWITCH TOK_LPAREN condition TOK_RPAREN statement { 182; }
         | TOK_WHILE TOK_LPAREN condition TOK_RPAREN statement { 183; }
         | TOK_DO statement TOK_WHILE TOK_LPAREN expression TOK_RPAREN TOK_SEMICOLON { 184; }
         | TOK_FOR TOK_LPAREN forInitStatement conditionOpt TOK_SEMICOLON expressionOpt TOK_RPAREN statement { 185; }
         | TOK_BREAK TOK_SEMICOLON { 186; }
         | TOK_CONTINUE TOK_SEMICOLON { 187; }
         | TOK_RETURN expression TOK_SEMICOLON { 188; }
         | TOK_RETURN TOK_SEMICOLON { 189; }
         | TOK_GOTO identifier TOK_SEMICOLON { 190; }
         | blockDeclaration { 191; }
         | tryBlock { 192; }
         | asmDefinition { 193; }
         | namespaceDecl { 194; }
         | functionDefinition { 195; }
         | kandRFunctionDefinition { 196; }
         | TOK_CASE constantExpression TOK_ELLIPSIS constantExpression TOK_COLON statement { 197; }
         | TOK_GOTO TOK_STAR expression TOK_SEMICOLON { 198; }
         | uberModifierSeq initDeclaratorList TOK_SEMICOLON { 199; }


expressionStatement: TOK_SEMICOLON { 200; }
                   | expression TOK_SEMICOLON { 201; }


compoundStatement: compoundStmtHelper TOK_RBRACE { 202; }
                 | compoundStmtHelper labeledEmptyStatementList TOK_RBRACE { 203; }


compoundStmtHelper: TOK_LBRACE { 204; }
                  | compoundStmtHelper statement annotationOpt { 205; }


condition: expression { 206; }
         | typeSpecifier declarator TOK_EQUAL assignmentExpression { 207; }


conditionOpt: /* empty */ { 208; }
            | condition { 209; }


forInitStatement: expressionStatement { 210; }
                | simpleDeclaration { 211; }


declaration: blockDeclaration { 212; }
           | functionDefinition { 213; }
           | templateDeclaration { 214; }
           | explicitInstantiation { 215; }
           | linkageSpecification { 216; }
           | asmDefinition { 217; }
           | namespaceDefinition { 218; }
           | namespaceDecl { 219; }
           | kandRFunctionDefinition { 220; }
           | kandRFunctionDefinition_implInt { 221; }
           | implIntFunctionDefinition { 222; }
           | uberModifierSeqOpt initDeclaratorList TOK_SEMICOLON { 223; }


blockDeclaration: simpleDeclaration { 224; }


simpleDeclaration: declSpecifier initDeclaratorList TOK_SEMICOLON { 225; }
                 | declSpecifier TOK_SEMICOLON { 226; }


declSpecifier: pq_type_name uberModifierSeqOpt { 227; }
             | uberModifierSeq pq_type_name uberModifierSeqOpt { 228; }
             | uberTypeKeyword uberTypeAndModifierSeqOpt { 229; }
             | uberModifierSeq uberTypeKeyword uberTypeAndModifierSeqOpt { 230; }
             | elaboratedOrSpecifier uberModifierSeqOpt { 231; }
             | uberModifierSeq elaboratedOrSpecifier uberModifierSeqOpt { 232; }
             | typeofTypeSpecifier uberModifierSeqOpt { 233; }
             | uberModifierSeq typeofTypeSpecifier uberModifierSeqOpt { 234; }
             | pq_type_name buggyGccTypeModifier uberModifierSeqOpt { 235; }
             | uberModifierSeq pq_type_name buggyGccTypeModifier uberModifierSeqOpt { 236; }


elaboratedOrSpecifier: elaboratedTypeSpecifier { 237; }
                     | classSpecifier { 238; }
                     | enumSpecifier { 239; }


uberModifierSeq: uberModifier { 240; }
               | uberModifierSeq uberModifier { 241; }


uberModifierSeqOpt: /* empty */ { 242; }
                  | uberModifierSeq { 243; }


uberTypeAndModifierSeqOpt: /* empty */ { 244; }
                         | uberTypeAndModifierSeqOpt uberModifier { 245; }
                         | uberTypeAndModifierSeqOpt uberTypeKeyword { 246; }


ubercv_qualifierSeq: ubercv_qualifier { 247; }
                  | ubercv_qualifierSeq ubercv_qualifier { 248; }


ubercv_qualifierSeqOpt: /* empty */ { 249; }
                     | ubercv_qualifierSeq { 250; }


uberTypeAndcv_qualifierSeqOpt: /* empty */ { 251; }
                            | uberTypeAndcv_qualifierSeqOpt ubercv_qualifier { 252; }
                            | uberTypeAndcv_qualifierSeqOpt uberTypeKeyword { 253; }


uberModifier: TOK_AUTO { 254; }
            | TOK_REGISTER { 255; }
            | TOK_STATIC { 256; }
            | TOK_EXTERN { 257; }
            | TOK_MUTABLE { 258; }
            | TOK_INLINE { 259; }
            | TOK_VIRTUAL { 260; }
            | TOK_FRIEND { 261; }
            | TOK_TYPEDEF { 262; }
            | TOK_CONST { 263; }
            | TOK_VOLATILE { 264; }
            | TOK_RESTRICT { 265; }
            | attributeSpecifier { 266; }


ubercv_qualifier: TOK_CONST { 267; }
               | TOK_VOLATILE { 268; }
               | TOK_RESTRICT { 269; }
               | attributeSpecifier { 270; }


uberTypeKeyword: TOK_CHAR { 271; }
               | TOK_WCHAR_T { 272; }
               | TOK_BOOL { 273; }
               | TOK_SHORT { 274; }
               | TOK_INT { 275; }
               | TOK_LONG { 276; }
               | TOK_SIGNED { 277; }
               | TOK_UNSIGNED { 278; }
               | TOK_FLOAT { 279; }
               | TOK_DOUBLE { 280; }
               | TOK_VOID { 281; }
               | TOK_COMPLEX { 282; }
               | TOK_IMAGINARY { 283; }


elaboratedTypeSpecifier: classKey pq_type_name { 284; }
                       | TOK_ENUM pq_type_name { 285; }
                       | TOK_TYPENAME pq_type_name { 286; }
                       | classKey attributeSpecifier pq_type_name { 287; }
                       | TOK_ENUM attributeSpecifierList pq_type_name { 288; }


typeSpecifier: pq_type_name ubercv_qualifierSeqOpt { 289; }
             | ubercv_qualifierSeq pq_type_name ubercv_qualifierSeqOpt { 290; }
             | uberTypeKeyword uberTypeAndcv_qualifierSeqOpt { 291; }
             | ubercv_qualifierSeq uberTypeKeyword uberTypeAndcv_qualifierSeqOpt { 292; }
             | elaboratedOrSpecifier ubercv_qualifierSeqOpt { 293; }
             | ubercv_qualifierSeq elaboratedOrSpecifier ubercv_qualifierSeqOpt { 294; }
             | typeofTypeSpecifier ubercv_qualifierSeqOpt { 295; }
             | ubercv_qualifierSeq typeofTypeSpecifier ubercv_qualifierSeqOpt { 296; }


pq_type_name: pq_type_name_ncc { 297; }
          | TOK_COLONCOLON pq_type_name_ncc { 298; }


pq_type_name_ncc: identifier { 299; }
              | templateId { 300; }
              | identifier TOK_COLONCOLON pq_type_name_notfirst { 301; }
              | identifier TOK_LESSTHAN templateArgumentListOpt TOK_GREATERTHAN TOK_COLONCOLON pq_type_name_notfirst { 302; }


pq_type_name_notfirst: pq_type_name_ncc { 303; }
                   | TOK_TEMPLATE templateId { 304; }
                   | TOK_TEMPLATE identifier TOK_LESSTHAN templateArgumentListOpt TOK_GREATERTHAN TOK_COLONCOLON pq_type_name_notfirst { 305; }


enumSpecifier: TOK_ENUM TOK_LBRACE enumeratorListOpt TOK_RBRACE { 306; }
             | TOK_ENUM identifier TOK_LBRACE enumeratorListOpt TOK_RBRACE { 307; }
             | TOK_ENUM attributeSpecifierList TOK_LBRACE enumeratorListOpt TOK_RBRACE { 308; }
             | TOK_ENUM attributeSpecifierList pq_type_name TOK_LBRACE enumeratorListOpt TOK_RBRACE { 309; }


enumeratorListOpt: /* empty */ { 310; }
                 | enumeratorDefinition { 311; }
                 | enumeratorDefinition TOK_COMMA enumeratorListOpt { 312; }


enumeratorDefinition: identifier { 313; }
                    | identifier TOK_EQUAL constantExpression { 314; }


asmDefinition: TOK_ASM TOK_LPAREN stringLiteral TOK_RPAREN TOK_SEMICOLON { 315; }
             | TOK_ASM cv_qualifierSeq TOK_LPAREN stringLiteral TOK_RPAREN TOK_SEMICOLON { 316; }
             | TOK_ASM cv_qualifierSeq TOK_LPAREN stringLiteral nonemptyOpConstraints TOK_RPAREN TOK_SEMICOLON { 317; }
             | TOK_ASM TOK_LPAREN stringLiteral nonemptyOpConstraints TOK_RPAREN TOK_SEMICOLON { 318; }


linkageSpecification: TOK_EXTERN TOK_STRING_LITERAL TOK_LBRACE translation_unit TOK_RBRACE { 319; }
                    | TOK_EXTERN TOK_STRING_LITERAL declaration { 320; }


initDeclaratorList: initDeclarator { 321; }
                  | initDeclarator TOK_COMMA initDeclaratorList { 322; }
                  | initDeclarator TOK_COMMA attributeSpecifierList initDeclaratorList { 323; }


initDeclarator: declarator { 324; }
              | declarator initializer { 325; }


initializer: TOK_EQUAL simpleInitializerClause { 326; }
           | TOK_LPAREN expressionList TOK_RPAREN { 327; }


simpleInitializerClause: assignmentExpression { 328; }
                       | compoundInitializer { 329; }


initializerClause: simpleInitializerClause { 330; }
                 | identifier TOK_COLON simpleInitializerClause { 331; }
                 | designatorList TOK_EQUAL simpleInitializerClause { 332; }
                 | designatorList simpleInitializerClause { 333; }


compoundInitializer: TOK_LBRACE initializerList commaOpt TOK_RBRACE { 334; }
                   | TOK_LBRACE TOK_RBRACE { 335; }


commaOpt: /* empty */ { 336; }
        | TOK_COMMA { 337; }


initializerList: initializerClause { 338; }
               | initializerList TOK_COMMA initializerClause { 339; }


declarator: TOK_STAR cv_qualifierSeqOpt declarator { 340; }
          | TOK_AND cv_qualifierSeqOpt declarator { 341; }
          | ptrToMemberName TOK_STAR cv_qualifierSeqOpt declarator { 342; }
          | directDeclarator { 343; }
          | directDeclarator TOK_ASM TOK_LPAREN stringLiteral TOK_RPAREN { 344; }
          | directDeclarator TOK_ASM TOK_LPAREN stringLiteral TOK_RPAREN attributeSpecifierList { 345; }
          | TOK_STAR cv_qualifierSeqOpt attributeSpecifier cv_qualAttrSeqOpt declarator { 346; }
          | directDeclarator attributeSpecifierList { 347; }


directDeclarator: idExpression { 348; }
                | pq_dtorName { 349; }
                | directDeclarator TOK_LPAREN parameterDeclarationClause TOK_RPAREN cv_qualifierSeqOpt exceptionSpecificationOpt { 350; }
                | directDeclarator TOK_LBRACKET constantExpressionOpt TOK_RBRACKET { 351; }
                | TOK_LPAREN declarator TOK_RPAREN { 352; }
                | directDeclarator TOK_LBRACKET cv_qualifierSeq TOK_RBRACKET { 353; }
                | TOK_LPAREN attributeSpecifierList declarator TOK_RPAREN { 354; }


pq_dtorName: TOK_TILDE identifier { 355; }
          | TOK_TILDE identifier TOK_LESSTHAN templateArgumentListOpt TOK_GREATERTHAN { 356; }
          | identifier TOK_COLONCOLON pq_dtorName { 357; }
          | identifier TOK_LESSTHAN templateArgumentListOpt TOK_GREATERTHAN TOK_COLONCOLON pq_dtorName { 358; }
          | TOK_TEMPLATE identifier TOK_LESSTHAN templateArgumentListOpt TOK_GREATERTHAN TOK_COLONCOLON pq_dtorName { 359; }


ptrToMemberName: idExpression TOK_COLONCOLON { 360; }


cv_qualifierSeqOpt: /* empty */ { 361; }
                 | cv_qualifierSeq { 362; }


cv_qualifierSeq: cv_qualifier { 363; }
              | cv_qualifier cv_qualifierSeq { 364; }


cv_qualifier: TOK_CONST { 365; }
           | TOK_VOLATILE { 366; }
           | TOK_RESTRICT { 367; }


typeId: typeSpecifier abstractDeclaratorOpt { 368; }


abstractDeclaratorOpt: /* empty */ { 369; }
                     | abstractDeclarator { 370; }


abstractDeclarator: TOK_STAR cv_qualifierSeqOpt abstractDeclaratorOpt { 371; }
                  | TOK_AND cv_qualifierSeqOpt abstractDeclaratorOpt { 372; }
                  | ptrToMemberName TOK_STAR cv_qualifierSeqOpt abstractDeclaratorOpt { 373; }
                  | directAbstractDeclarator { 374; }
                  | TOK_STAR cv_qualifierSeqOpt attributeSpecifier cv_qualAttrSeqOpt abstractDeclaratorOpt { 375; }
                  | directAbstractDeclarator attributeSpecifierList { 376; }


directAbstractDeclaratorOpt: /* empty */ { 377; }
                           | directAbstractDeclarator { 378; }


directAbstractDeclarator: directAbstractDeclaratorOpt TOK_LPAREN parameterDeclarationClause TOK_RPAREN cv_qualifierSeqOpt exceptionSpecificationOpt { 379; }
                        | directAbstractDeclaratorOpt TOK_LBRACKET constantExpressionOpt TOK_RBRACKET { 380; }
                        | TOK_LPAREN abstractDeclarator TOK_RPAREN { 381; }
                        | directAbstractDeclaratorOpt TOK_LBRACKET cv_qualifierSeq TOK_RBRACKET { 382; }
                        | TOK_LPAREN attributeSpecifierList abstractDeclarator TOK_RPAREN { 383; }


parameterDeclarationClause: parameterDeclarationList { 384; }
                          | /* empty */ { 385; }


parameterDeclarationList: TOK_ELLIPSIS { 386; }
                        | parameterDeclaration TOK_ELLIPSIS { 387; }
                        | parameterDeclaration { 388; }
                        | parameterDeclaration TOK_COMMA parameterDeclarationList { 389; }


parameterDeclaration: typeSpecifier parameterDeclarator { 390; }
                    | TOK_REGISTER typeSpecifier parameterDeclarator { 391; }
                    | typeSpecifier TOK_REGISTER parameterDeclarator { 392; }
                    | unqualifiedDeclarator { 393; }
                    | TOK_REGISTER unqualifiedDeclarator { 394; }


parameterDeclarator: unqualifiedDeclarator { 395; }
                   | unqualifiedDeclarator TOK_EQUAL assignmentExpression { 396; }
                   | abstractDeclaratorOpt { 397; }
                   | abstractDeclaratorOpt TOK_EQUAL assignmentExpression { 398; }


functionDefinition: declSpecifier fd_declarator functionBody { 399; }
                  | declSpecifier fd_declarator TOK_TRY functionBody handlerSeq { 400; }
                  | cdtorModifierSeq fd_declarator ctorInitializerOpt functionBody { 401; }
                  | fd_declarator ctorInitializerOpt functionBody { 402; }
                  | cdtorModifierSeq fd_declarator TOK_TRY ctorInitializerOpt functionBody handlerSeq { 403; }
                  | fd_declarator TOK_TRY ctorInitializerOpt functionBody handlerSeq { 404; }


fd_declarator: declarator { 405; }


functionBody: compoundStatement { 406; }


ctorInitializerOpt: /* empty */ { 407; }
                  | TOK_COLON memInitializerList { 408; }


classSpecifier: classKey classHeadNameOpt baseClauseOpt TOK_LBRACE memberDeclarationSeqOpt TOK_RBRACE { 409; }
              | classKey attributeSpecifierList classHeadNameOpt baseClauseOpt TOK_LBRACE memberDeclarationSeqOpt TOK_RBRACE { 410; }


classHeadNameOpt: /* empty */ { 411; }
                | classHeadName { 412; }


classHeadName: identifier { 413; }
             | templateId { 414; }
             | identifier TOK_COLONCOLON classHeadName { 415; }
             | identifier TOK_LESSTHAN templateArgumentListOpt TOK_GREATERTHAN TOK_COLONCOLON classHeadName { 416; }
             | TOK_TEMPLATE identifier TOK_LESSTHAN templateArgumentListOpt TOK_GREATERTHAN TOK_COLONCOLON classHeadName { 417; }


classKey: TOK_CLASS { 418; }
        | TOK_STRUCT { 419; }
        | TOK_UNION { 420; }


memberDeclarationSeqOpt: /* empty */ { 421; }
                       | memberDeclarationSeqOpt TOK_SEMICOLON { 422; }
                       | memberDeclarationSeqOpt memberDeclaration { 423; }
                       | memberDeclarationSeqOpt accessSpecifier TOK_COLON { 424; }


accessSpecifier: TOK_PUBLIC { 425; }
               | TOK_PRIVATE { 426; }
               | TOK_PROTECTED { 427; }


memberDeclaration: declSpecifier memberDeclaratorList TOK_SEMICOLON { 428; }
                 | declSpecifier TOK_SEMICOLON { 429; }
                 | p_qualifiedId TOK_SEMICOLON { 430; }
                 | TOK_USING idExpression TOK_SEMICOLON { 431; }
                 | functionDefinition { 432; }
                 | cdtorProtoDecl { 433; }
                 | templateDeclaration { 434; }


cdtorProtoDecl: cdtorModifierSeq memberDeclarator TOK_SEMICOLON { 435; }
              | memberDeclarator TOK_SEMICOLON { 436; }


memberDeclaratorList: memberDeclarator { 437; }
                    | memberDeclarator TOK_COMMA memberDeclaratorList { 438; }


memberDeclarator: declarator { 439; }
                | declarator TOK_EQUAL constantExpression { 440; }
                | identifierOpt TOK_COLON constantExpression { 441; }
                | identifierOpt TOK_COLON constantExpression attributeSpecifierList { 442; }


identifierOpt: /* empty */ { 443; }
             | identifier { 444; }


cdtorModifier: TOK_EXPLICIT { 445; }
             | TOK_VIRTUAL { 446; }
             | TOK_INLINE { 447; }
             | TOK_FRIEND { 448; }
             | attributeSpecifier { 449; }


cdtorModifierSeq: cdtorModifier { 450; }
                | cdtorModifierSeq cdtorModifier { 451; }


baseClauseOpt: /* empty */ { 452; }
             | TOK_COLON baseSpecifierList { 453; }


baseSpecifierList: baseSpecifier { 454; }
                 | baseSpecifier TOK_COMMA baseSpecifierList { 455; }


baseSpecifier: pq_class_name { 456; }
             | TOK_VIRTUAL accessSpecifierOpt pq_class_name { 457; }
             | accessSpecifier virtualOpt pq_class_name { 458; }


virtualOpt: /* empty */ { 459; }
          | TOK_VIRTUAL { 460; }


accessSpecifierOpt: /* empty */ { 461; }
                  | accessSpecifier { 462; }


pq_class_name: pq_type_name { 463; }


conversionFunctionId: TOK_OPERATOR conversionTypeId { 464; }


conversionTypeId: typeSpecifier conversionDeclaratorOpt { 465; }


conversionDeclaratorOpt: /* empty */ { 466; }
                       | TOK_STAR cv_qualifierSeqOpt conversionDeclaratorOpt { 467; }
                       | TOK_AND cv_qualifierSeqOpt conversionDeclaratorOpt { 468; }
                       | ptrToMemberName TOK_STAR cv_qualifierSeqOpt conversionDeclaratorOpt { 469; }


memInitializerList: memInitializer { 470; }
                  | memInitializer TOK_COMMA memInitializerList { 471; }


memInitializer: memInitializerId TOK_LPAREN expressionListOpt TOK_RPAREN { 472; }


memInitializerId: pq_type_name { 473; }


operatorFunctionId: TOK_OPERATOR operator { 474; }


operator: TOK_NEW { 475; }
        | TOK_DELETE { 476; }
        | TOK_NEW TOK_LBRACKET TOK_RBRACKET { 477; }
        | TOK_DELETE TOK_LBRACKET TOK_RBRACKET { 478; }
        | TOK_BANG { 479; }
        | TOK_TILDE { 480; }
        | TOK_PLUSPLUS { 481; }
        | TOK_MINUSMINUS { 482; }
        | TOK_PLUS { 483; }
        | TOK_MINUS { 484; }
        | TOK_STAR { 485; }
        | TOK_SLASH { 486; }
        | TOK_PERCENT { 487; }
        | TOK_LEFTSHIFT { 488; }
        | TOK_RIGHTSHIFT { 489; }
        | TOK_AND { 490; }
        | TOK_XOR { 491; }
        | TOK_OR { 492; }
        | TOK_EQUAL { 493; }
        | TOK_PLUSEQUAL { 494; }
        | TOK_MINUSEQUAL { 495; }
        | TOK_STAREQUAL { 496; }
        | TOK_SLASHEQUAL { 497; }
        | TOK_PERCENTEQUAL { 498; }
        | TOK_LEFTSHIFTEQUAL { 499; }
        | TOK_RIGHTSHIFTEQUAL { 500; }
        | TOK_ANDEQUAL { 501; }
        | TOK_XOREQUAL { 502; }
        | TOK_OREQUAL { 503; }
        | TOK_EQUALEQUAL { 504; }
        | TOK_NOTEQUAL { 505; }
        | TOK_LESSTHAN { 506; }
        | TOK_GREATERTHAN { 507; }
        | TOK_LESSEQ { 508; }
        | TOK_GREATEREQ { 509; }
        | TOK_ANDAND { 510; }
        | TOK_OROR { 511; }
        | TOK_ARROW { 512; }
        | TOK_ARROWSTAR { 513; }
        | TOK_LBRACKET TOK_RBRACKET { 514; }
        | TOK_LPAREN TOK_RPAREN { 515; }
        | TOK_COMMA { 516; }
        | TOK_MIN_OP { 517; }
        | TOK_MAX_OP { 518; }


templateDeclaration: templatePreamble functionDefinition { 519; }
                   | templatePreamble simpleDeclaration { 520; }
                   | templatePreamble templateDeclaration { 521; }
                   | templatePreamble cdtorProtoDecl { 522; }


templatePreamble: TOK_TEMPLATE TOK_LESSTHAN templateParameterList TOK_GREATERTHAN { 523; }
                | TOK_EXPORT TOK_TEMPLATE TOK_LESSTHAN templateParameterList TOK_GREATERTHAN { 524; }
                | TOK_TEMPLATE TOK_LESSTHAN TOK_GREATERTHAN { 525; }
                | TOK_EXPORT TOK_TEMPLATE TOK_LESSTHAN TOK_GREATERTHAN { 526; }


templateParameterList: classOrTypename identifierOpt defaultTypeOpt templateParameterListContinuation { 527; }
                     | parameterDeclaration templateParameterListContinuation { 528; }
                     | TOK_TEMPLATE TOK_LESSTHAN templateParameterList TOK_GREATERTHAN TOK_CLASS identifierOpt defaultTemplateOpt templateParameterListContinuation { 529; }


templateParameterListContinuation: /* empty */ { 530; }
                                 | TOK_COMMA templateParameterList { 531; }


classOrTypename: TOK_CLASS { 532; }
               | TOK_TYPENAME { 533; }


defaultTypeOpt: /* empty */ { 534; }
              | TOK_EQUAL typeId { 535; }


defaultTemplateOpt: /* empty */ { 536; }
                  | TOK_EQUAL idExpression { 537; }


templateArgumentListOpt: /* empty */ { 538; }
                       | templateArgumentList { 539; }


templateId: identifier TOK_LESSTHAN templateArgumentListOpt TOK_GREATERTHAN { 540; }
          | operatorFunctionId TOK_LESSTHAN templateArgumentListOpt TOK_GREATERTHAN { 541; }


templateArgumentList: templateArgument { 542; }


templateArgumentListTailOpt: /* empty */ { 543; }
                           | TOK_COMMA templateArgument { 544; }


templateArgument: typeId templateArgumentListTailOpt { 545; }
                | assignmentExpression templateArgumentListTailOpt { 546; }


explicitInstantiation: TOK_TEMPLATE blockDeclaration { 547; }
                     | TOK_INLINE TOK_TEMPLATE blockDeclaration { 548; }
                     | TOK_EXTERN TOK_TEMPLATE blockDeclaration { 549; }


tryBlock: TOK_TRY compoundStatement handlerSeq { 550; }


handlerSeq: handler { 551; }
          | handler handlerSeq { 552; }


handler: TOK_CATCH TOK_LPAREN handlerParameter TOK_RPAREN compoundStatement { 553; }
       | TOK_CATCH TOK_LPAREN TOK_ELLIPSIS TOK_RPAREN compoundStatement { 554; }


handlerParameter: typeSpecifier unqualifiedDeclarator { 555; }
                | typeSpecifier abstractDeclaratorOpt { 556; }


unqualifiedDeclarator: declarator { 557; }


throwExpression: TOK_THROW { 558; }
               | TOK_THROW assignmentExpression { 559; }


exceptionSpecificationOpt: /* empty */ { 560; }
                         | TOK_THROW TOK_LPAREN TOK_RPAREN { 561; }
                         | TOK_THROW TOK_LPAREN typeIdList TOK_RPAREN { 562; }


typeIdList: typeId { 563; }
          | typeId TOK_COMMA typeIdList { 564; }


namespaceDefinition: TOK_NAMESPACE identifierOpt TOK_LBRACE translation_unit TOK_RBRACE { 565; }
                   | TOK_NAMESPACE identifierOpt attributeSpecifierList TOK_LBRACE translation_unit TOK_RBRACE { 566; }


namespaceDecl: TOK_NAMESPACE identifier TOK_EQUAL idExpression TOK_SEMICOLON { 567; }
             | TOK_USING idExpression TOK_SEMICOLON { 568; }
             | TOK_USING TOK_NAMESPACE idExpression TOK_SEMICOLON { 569; }
             | TOK_USING TOK_NAMESPACE idExpression attributeSpecifierList TOK_SEMICOLON { 570; }


annotationOpt: /* empty */ { 571; }
             | annotationList { 572; }


annotationList: TOK_ANNOTATION { 573; }
              | TOK_ANNOTATION annotationList { 574; }


namesAfterDot: nameAfterDot { 575; }
             | namesAfterDot TOK_DOT nameAfterDot { 576; }
             | namesAfterDot TOK_LBRACKET expression TOK_RBRACKET { 577; }


parenthesizedExpression: TOK_LPAREN expression TOK_RPAREN { 578; }


labeledEmptyStatementList: labelAndColon labeledEmptyStatementListOpt { 579; }
                         | TOK_CASE constantExpression TOK_COLON labeledEmptyStatementListOpt { 580; }
                         | TOK_CASE constantExpression TOK_ELLIPSIS constantExpression TOK_COLON labeledEmptyStatementListOpt { 581; }
                         | TOK_DEFAULT TOK_COLON labeledEmptyStatementListOpt { 582; }


labeledEmptyStatementListOpt: /* empty */ { 583; }
                            | labeledEmptyStatementList { 584; }


typeofTypeSpecifier: typeofExpr { 585; }
                   | typeofType { 586; }


typeofExpr: TOK___TYPEOF__ TOK_LPAREN expression TOK_RPAREN { 587; }


typeofType: TOK___TYPEOF__ TOK_LPAREN typeId TOK_RPAREN { 588; }


bracketedWordOpt: /* empty */ { 589; }
                | TOK_LBRACKET identifier TOK_RBRACKET { 590; }


parenthesizedExpressionOpt: /* empty */ { 591; }
                          | TOK_LPAREN expression TOK_RPAREN { 592; }


opConstraint: bracketedWordOpt stringLiteral parenthesizedExpressionOpt { 593; }


opConstraintList: /* empty */ { 594; }
                | opConstraint { 595; }
                | opConstraint TOK_COMMA opConstraintList { 596; }


opConstraints: /* empty */ { 597; }
             | nonemptyOpConstraints { 598; }


nonemptyOpConstraints: opConstraints TOK_COLON opConstraintList { 599; }
                     | opConstraints TOK_COLONCOLON opConstraintList { 600; }


designatorList: designator { 601; }
              | designator designatorList { 602; }


designator: TOK_DOT identifier { 603; }
          | TOK_LBRACKET constantExpression TOK_RBRACKET { 604; }
          | TOK_LBRACKET constantExpression TOK_ELLIPSIS constantExpression TOK_RBRACKET { 605; }


buggyGccTypeModifier: TOK_LONG { 606; }
                    | TOK_SHORT { 607; }
                    | TOK_SIGNED { 608; }
                    | TOK_UNSIGNED { 609; }
                    | TOK_LONG buggyGccTypeModifier { 610; }
                    | TOK_SHORT buggyGccTypeModifier { 611; }
                    | TOK_SIGNED buggyGccTypeModifier { 612; }
                    | TOK_UNSIGNED buggyGccTypeModifier { 613; }


cv_qualAttrSeqOpt: /* empty */ { 614; }
                | cv_qualAttrSeq { 615; }


cv_qualAttrSeq: cv_qualAttr { 616; }
             | cv_qualAttr cv_qualAttrSeq { 617; }


cv_qualAttr: cv_qualifier { 618; }
          | attributeSpecifier { 619; }


attributeWord: TOK_NAME { 620; }
             | TOK_ASM { 621; }
             | TOK_AUTO { 622; }
             | TOK_BREAK { 623; }
             | TOK_BOOL { 624; }
             | TOK_CASE { 625; }
             | TOK_CATCH { 626; }
             | TOK_CDECL { 627; }
             | TOK_CHAR { 628; }
             | TOK_CLASS { 629; }
             | TOK_CONST { 630; }
             | TOK_CONST_CAST { 631; }
             | TOK_CONTINUE { 632; }
             | TOK_DEFAULT { 633; }
             | TOK_DELETE { 634; }
             | TOK_DO { 635; }
             | TOK_DOUBLE { 636; }
             | TOK_DYNAMIC_CAST { 637; }
             | TOK_ELSE { 638; }
             | TOK_ENUM { 639; }
             | TOK_EXPLICIT { 640; }
             | TOK_EXPORT { 641; }
             | TOK_EXTERN { 642; }
             | TOK_FALSE { 643; }
             | TOK_FLOAT { 644; }
             | TOK_FOR { 645; }
             | TOK_FRIEND { 646; }
             | TOK_GOTO { 647; }
             | TOK_IF { 648; }
             | TOK_INLINE { 649; }
             | TOK_INT { 650; }
             | TOK_LONG { 651; }
             | TOK_MUTABLE { 652; }
             | TOK_NAMESPACE { 653; }
             | TOK_NEW { 654; }
             | TOK_OPERATOR { 655; }
             | TOK_PASCAL { 656; }
             | TOK_PRIVATE { 657; }
             | TOK_PROTECTED { 658; }
             | TOK_PUBLIC { 659; }
             | TOK_REGISTER { 660; }
             | TOK_REINTERPRET_CAST { 661; }
             | TOK_RETURN { 662; }
             | TOK_SHORT { 663; }
             | TOK_SIGNED { 664; }
             | TOK_SIZEOF { 665; }
             | TOK_STATIC { 666; }
             | TOK_STATIC_CAST { 667; }
             | TOK_STRUCT { 668; }
             | TOK_SWITCH { 669; }
             | TOK_TEMPLATE { 670; }
             | TOK_THIS { 671; }
             | TOK_THROW { 672; }
             | TOK_TRUE { 673; }
             | TOK_TRY { 674; }
             | TOK_TYPEDEF { 675; }
             | TOK_TYPEID { 676; }
             | TOK_TYPENAME { 677; }
             | TOK_UNION { 678; }
             | TOK_UNSIGNED { 679; }
             | TOK_USING { 680; }
             | TOK_VIRTUAL { 681; }
             | TOK_VOID { 682; }
             | TOK_VOLATILE { 683; }
             | TOK_WCHAR_T { 684; }
             | TOK_WHILE { 685; }


commaSepExpressionListOpt: /* empty */ { 686; }
                         | expressionList { 687; }


attributeParameters: commaSepExpressionListOpt { 688; }


attribute: /* empty */ { 689; }
         | attributeWord { 690; }
         | attributeWord TOK_LPAREN attributeParameters TOK_RPAREN { 691; }


attributeList: attribute { 692; }
             | attribute TOK_COMMA attributeList { 693; }


attributeSpecifier: TOK___ATTRIBUTE__ TOK_LPAREN TOK_LPAREN attributeList TOK_RPAREN TOK_RPAREN { 694; }


attributeSpecifierList: attributeSpecifier { 695; }
                      | attributeSpecifier attributeSpecifierList { 696; }


kandRFunctionDefinition: declSpecifier kandRDeclarator kandRSimpleDeclarationSeq functionBody { 697; }


kandRFunctionDefinition_implInt: kandRDeclarator kandRSimpleDeclarationSeq functionBody { 698; }
                               | uberModifierSeq kandRDeclarator kandRSimpleDeclarationSeq functionBody { 699; }


kandRSimpleDeclarationSeq: kandRSimpleDeclaration { 700; }
                         | kandRSimpleDeclarationSeq kandRSimpleDeclaration { 701; }


kandRSimpleDeclaration: declSpecifier kandRInitDeclaratorList TOK_SEMICOLON { 702; }
                      | TOK_REGISTER kandRInitDeclaratorList TOK_SEMICOLON { 703; }


kandRInitDeclaratorList: kandRInitDeclarator { 704; }
                       | kandRInitDeclarator TOK_COMMA kandRInitDeclaratorList { 705; }


kandRInitDeclarator: declarator { 706; }


kandRDeclarator: TOK_STAR cv_qualifierSeqOpt kandRDeclarator { 707; }
               | kandRDirectDeclarator { 708; }


kandRDirectDeclarator: kandRIdExpression TOK_LPAREN kandRIdentifierList TOK_RPAREN { 709; }
                     | TOK_LPAREN kandRIdExpression TOK_RPAREN TOK_LPAREN kandRIdentifierList TOK_RPAREN { 710; }
                     | kandRDirectDeclarator TOK_LPAREN parameterDeclarationClause TOK_RPAREN { 711; }
                     | kandRDirectDeclarator TOK_LBRACKET constantExpressionOpt TOK_RBRACKET { 712; }
                     | TOK_LPAREN kandRDeclarator TOK_RPAREN { 713; }


kandRIdExpression: kandRIdentifier { 714; }


kandRIdentifierList: kandRIdentifier { 715; }
                   | kandRIdentifier TOK_COMMA kandRIdentifierList { 716; }


kandRIdentifier: identifier { 717; }


c_func_modifier_no_inline: TOK_STATIC { 718; }
                       | TOK_EXTERN { 719; }
                       | TOK_CONST { 720; }
                       | TOK_VOLATILE { 721; }


c_func_modifier: c_func_modifier_no_inline { 722; }
             | TOK_INLINE { 723; }


c_func_modifierSeq: c_func_modifier { 724; }
                | c_func_modifierSeq c_func_modifier { 725; }


c_func_modifierSeqOpt: /* empty */ { 726; }
                   | c_func_modifier { 727; }


implIntFunctionDefinition: c_func_modifier_no_inline declarator functionBody { 728; }
                         | c_func_modifier_no_inline c_func_modifierSeq declarator functionBody { 729; }
                         | TOK_INLINE c_func_modifier_no_inline c_func_modifierSeqOpt declarator functionBody { 730; }


implicitIntTypeSpecifier: ubercv_qualifierSeq { 731; }


implicitIntTypeId: implicitIntTypeSpecifier { 732; }


