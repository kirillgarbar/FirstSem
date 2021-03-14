module Interpreter

open System.Collections.Generic
open BigInt

let rec processExpr (vDict:Dictionary<AST.VName,AST.Expression>) expr =
    match expr with
    | AST.Num n -> stringToBigInt n
    | AST.NVar nv ->
        try
            processExpr vDict vDict.[nv]
        with
        | _ -> failwithf "Variable %A is not declared." nv
    | AST.Sum (x, y) -> sum (processExpr vDict x) (processExpr vDict y)
    | AST.Sub (x, y) -> sub (processExpr vDict x) (processExpr vDict y)
    | AST.Mul (x, y) -> mul (processExpr vDict x) (processExpr vDict y)
    | AST.Div (x, y) -> div (processExpr vDict x) (processExpr vDict y)
    | AST.Rem (x, y) -> rem (processExpr vDict x) (processExpr vDict y)
    | AST.Pow (x, y) -> power (processExpr vDict x) (processExpr vDict y)
    | AST.Abs x -> abs (processExpr vDict x)
    | AST.Bin x -> toBinary (processExpr vDict x)

let processStmt (vDict:Dictionary<AST.VName,AST.Expression>) stmt =
    match stmt with
    | AST.Print v ->
        let data =
            try
                vDict.[v]
            with
            | _ -> failwithf "Variable %A is not declared." v
        match data with
        | AST.Num n -> printfn "%s" (if n.[0] = '+' then n.[1..] else n)
        | _ -> failwith "Num expected"
    | AST.VDecl(v,e) ->
        if vDict.ContainsKey v
        then vDict.[v] <- AST.Num ((processExpr vDict e) |> bigIntToString)
        else vDict.Add(v, AST.Num ((processExpr vDict e) |> bigIntToString))
    vDict

let run ast =
    let vDict = Dictionary<_,_>()
    ast |> List.fold processStmt vDict |> ignore

let calculate (ast:AST.Stmt list) =
    match ast.[0] with
    | AST.VDecl (_, e) -> processExpr (Dictionary<_,_>()) e
    | _ -> failwith "unexpected statement"

let rec drawTree stmt output =
    let getExpr =
        match stmt with
        | AST.VDecl (_, e) -> e
        | _ -> failwith "Unexpected statement"
    let getName label counter = (sprintf "\"x%A\"" counter) + (sprintf " [shape = ellipse, label = %A]; \n" label)
    let exprToDot res label lastOp c =
        if c = 1 then res + (getName label c)
        else res + (getName label c) + (sprintf "\"x%A\"" lastOp) + " -> " + (sprintf "\"x%A\"" c) + ";\n"
    let mutable c = 0
    let rec go expr res lastOp =
        c <- c + 1
        match expr with
        | AST.Num n -> res + (getName (string n) c) + (sprintf "\"x%A\"" lastOp) + " -> " + (sprintf "\"x%A\"" c) + ";\n"
        | AST.Sum (x, y) ->
            let newRes = exprToDot res "+" lastOp c
            let currC = c
            go y (newRes + go x "" currC) currC
        | AST.Sub (x, y) ->
            let newRes = exprToDot res "-" lastOp c
            let currC = c
            go y (newRes + go x "" currC) currC
        | AST.Mul (x, y) ->
            let newRes = exprToDot res "*" lastOp c
            let currC = c
            go y (newRes + go x "" currC) currC
        | AST.Div (x, y) ->
            let newRes = exprToDot res "/" lastOp c
            let currC = c
            go y (newRes + go x "" currC) currC
        | AST.Rem (x, y) ->
            let newRes = exprToDot res "%" lastOp c
            let currC = c
            go y (newRes + go x "" currC) currC
        | AST.Pow (x, y) ->
            let newRes = exprToDot res "^" lastOp c
            let currC = c
            go y (newRes + go x "" currC) currC
        | AST.Abs x ->
            let newRes = exprToDot res "| |" lastOp c
            go x newRes c
        | AST.Bin x ->
            let newRes = exprToDot res "~" lastOp c
            go x newRes c
        | AST.NVar _ -> "No variables allowed in this expression"

    System.IO.File.WriteAllText (output, ("digraph G {\n" + (go getExpr "" -1) + "}"))
    
