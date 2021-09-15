module DrawTree

open BigInt

let rec drawTree ast output =
    let getName label counter = (sprintf "\"x%A\"" counter) + (sprintf " [shape = ellipse, label = %A]; \n" label)

    let addLabel res label lastL c = res + (getName label c) + (sprintf "\"x%A\"" lastL) + " -> " + (sprintf "\"x%A\"" c) + ";\n"

    let mutable c = 0
    let rec expToDot expr res lastL =
        c <- c + 1
        match expr with
        | AST.Num n -> addLabel res (bigIntToString n) lastL c
        | AST.NVar (AST.Var v) ->
            let newRes = addLabel res ("Expression.NVar") lastL c
            c <- c + 1
            newRes + getName (sprintf "%s" v) c + (sprintf "\"x%A\"" (c - 1)) + " -> " + (sprintf "\"x%A\"" c) + ";\n"
        | AST.Sum (x, y) ->
            let newRes = addLabel res "Expression.Sum" lastL c  
            let currC = c
            expToDot y (newRes + expToDot x "" currC) currC
        | AST.Sub (x, y) ->
            let newRes = addLabel res "Expression.Sub" lastL c
            let currC = c
            expToDot y (newRes + expToDot x "" currC) currC
        | AST.Mul (x, y) ->
            let newRes = addLabel res "Expression.Mul" lastL c
            let currC = c
            expToDot y (newRes + expToDot x "" currC) currC
        | AST.Div (x, y) ->
            let newRes = addLabel res "Expression.Div" lastL c
            let currC = c
            expToDot y (newRes + expToDot x "" currC) currC
        | AST.Rem (x, y) ->
            let newRes = addLabel res "Expression.Rem" lastL c
            let currC = c
            expToDot y (newRes + expToDot x "" currC) currC
        | AST.Pow (x, y) ->
            let newRes = addLabel res "Expression.Pow" lastL c
            let currC = c
            expToDot y (newRes + expToDot x "" currC) currC
        | AST.Abs x ->
            let newRes = addLabel res "Expression.Abs" lastL c
            expToDot x newRes c
        | AST.Bin x ->
            let newRes = addLabel res "Expression.Bin" lastL c
            expToDot x newRes c

    let rec go ast res =
        c <- c + 1
        match ast with
        | [] -> res
        | head :: tail ->
            match head with
            | AST.VDecl (AST.Var v, e) ->
                let newRes = addLabel res "VDecl" 0 c
                c <- c + 1
                let newRes2 = addLabel newRes (sprintf "%s" v) (c - 1) c
                c <- c + 1
                let newRes3 = newRes2 + (expToDot e "" (c - 2))
                go tail newRes3
            | AST.Print (AST.Var v) ->
                let newRes = addLabel res "Print" 0 c
                c <- c + 1
                let newRes2 = addLabel newRes (sprintf "%s" v) (c - 1) c
                go tail newRes2

    let startNode = getName "list<Stmt>" 0

    System.IO.File.WriteAllText (output, ("digraph G {\n" + go ast startNode + "}"))
