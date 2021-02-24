module Interpreter

open System.Collections.Generic

let rec processExpr vDict expr =
    match expr with
    | AST.Num n -> n
    | AST.Sum (x, y) -> (processExpr vDict x) + (processExpr vDict y)
    | AST.Sub (x, y) -> (processExpr vDict x) - (processExpr vDict y)
    | AST.Mul (x, y) -> (processExpr vDict x) * (processExpr vDict y)
    | AST.Div (x, y) -> (processExpr vDict x) / (processExpr vDict y)

let processStmt (vDict:Dictionary<_,_>) stmt =
    match stmt with
    | AST.Print v ->
        let data =
            try
                vDict.[v]
            with
            | _ -> failwithf "Variable %A is not declared." v
        printfn "%A" data
    | AST.VDecl(v,e) ->
        if vDict.ContainsKey v
        then vDict.[v] <- processExpr vDict e
        else vDict.Add(v, processExpr vDict e)
    vDict

let run ast =
    let vDict = new Dictionary<_,_>()
    ast
    |> List.fold processStmt vDict
    |> ignore
