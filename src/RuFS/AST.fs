module AST

type VName = Var of string

type Expression =
    | Num of int
    | Sum of Expression * Expression
    | Sub of Expression * Expression
    | Mul of Expression * Expression
    | Div of Expression * Expression

type Stmt =
    | Print of VName
    | VDecl of VName * Expression

type Program = List<Stmt>
