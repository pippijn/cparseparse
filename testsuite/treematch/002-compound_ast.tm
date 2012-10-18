ast Program {
Program:     | Program [Definition]
             | End
Definition:  | Function Function
             | Global Global
Function:    | Function Identifer [Identifier] Expression
Expression:  | Let Identifer Expression Expression
             | Operator Operator Expression Expression
             | Call Name [Expression]
             | Variable Identifier
             | Constant Integer
Operator:    | Plus
             | Minus
             | Mult
Global:      | Global Identifer
Identifer: string
Integer: int
}
