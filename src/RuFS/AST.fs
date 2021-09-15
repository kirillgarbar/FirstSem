module AST

type VName = Var of string

type Expression =
    | Num of BigInt.BigInt
    | NVar of VName
    | Sum of Expression * Expression
    | Sub of Expression * Expression
    | Mul of Expression * Expression
    | Div of Expression * Expression
    | Rem of Expression * Expression
    | Pow of Expression * Expression
    | Bin of Expression
    | Abs of Expression

type Stmt =
    | Print of VName
    | VDecl of VName * Expression

type Program = List<Stmt>
