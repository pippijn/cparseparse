type decl =
  | BlockDeclaration
  | AmbiguousDeclaration of decl * decl


type uber_modifier =
  | UM_CHAR
  | UM_WCHAR_T
  | UM_BOOL
  | UM_SHORT
  | UM_INT
  | UM_LONG
  | UM_SIGNED
  | UM_UNSIGNED
  | UM_FLOAT
  | UM_DOUBLE
  | UM_VOID
