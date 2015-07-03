module MIT_6034.Assignment1

open Prelude

open System

type Atom = string
type Constant = string
type Identifier = string

type Var = 
    | Var   of Identifier
    | Const of Constant

type Datum = Atom * Constant list
type Statement = Atom * Var list

type Predicate =
    | Not  of Predicate
    | And  of Predicate list
    | Or   of Predicate list
    | Bool of bool
    | Stmt of Statement

type Consequent =
    | Then of Statement
    | Delete of Statement

type Rule = 
    {
        Antecedent : Predicate
        Consequent : Consequent list
    }

let siblings : Rule = {
    Antecedent = 
        And [ 
            Stmt ("parent", [ Var "x" ; Var "y" ])
            Stmt ("parent", [ Var "x" ; Var "z" ]) 
        ]
    Consequent = [ Then ("sibling", [ Var "y" ; Var "z" ]) ]
}

let data : Set<Datum> = 
    [
        "parent", [ "marge" ; "lisa" ]
        "parent", [ "marge" ; "bart" ]
    ] |> set

///Takes a statement and a datum, and tries to provide an instantiation of the statement with the provided datum
let bind ((atom, vars) : Statement) ((atom', consts) : Datum) = maybe {
    if atom = atom' then
        if vars.Length <> consts.Length then 
            forceAttention "rule and datum refer to same atom but with different number of args" 
            return! None
        else
            let varids = List.zip vars consts
            let! result = 
                (Some [], varids) |> List.foldBackPair (fun (v, c) s -> maybe {
                    let! s = s
                    match v with
                    | Const c' -> if c = c' then return s
                    | Var v -> return (v, c) :: s
                })
            return map result
    }

let rec instantiate' (acc : Set<Map<Identifier, Constant>>) (data : Set<Datum>) = function
    | Stmt stmt -> 
        data 
        |> Set.choose (bind stmt)
        |> Some
    | Bool true -> Some acc
    | Bool false -> None
    | Not p ->
        match instantiate' acc data p with
        | None -> Some Set.empty
        | Some m -> None
    | And l -> 
        l
        |> List.fold (fun s p -> maybe {
            let! s = s
            let! s' = instantiate' s data p
            let p = Set.product s s'
            return Set.choose (uncurry Map.union) p
        }) (Some acc)
    | Or l -> 
        l |> List.fold (fun s p ->
            match s with
            | None -> instantiate' acc data p
            | Some s ->
                maybe {
                    let! s' = instantiate' s data p
                    let p = Set.product s s'
                    return Set.choose (uncurry Map.union) p
                }
                |> Option.defaultsTo s
                |> Some
        ) None

///Find possible instantiations of the given predicate using the provided set of data
let instantiate = instantiate' Set.empty

let forwardChain (allRules : Rule list) : Set<Datum> -> Set<Datum> = 
    let rec fwd = 
        function
        | [], data -> data
        | (rule : Rule) :: rest, data ->
            let instantiations = instantiate data rule.Antecedent
            let newdata =
                instantiations 
                |> Option.map TODO
                |> Option.defaultsTo data

            if newdata <> data then 
                fwd (allRules, newdata)
            else 
                fwd (rest, data)

    curry fwd allRules

let rec simplify = function
    | Not (Not p) -> simplify p
    | Not (Bool b) -> Bool (not b)
    | Not p -> Not (simplify p)

    | And [] -> Bool true
    | And (And h :: t) -> simplify (And (h @ t))
    | And (Bool false :: _) -> Bool false
    | And (Bool true :: t) -> simplify (And t)
    | And (h :: t) ->
        match simplify h with
        | h' when h = h' ->
            match simplify (And t) with
            | And t' when t = t' -> And (h :: t)
            | p -> simplify (And [h ; p])
        | h' -> simplify (And (h' :: t))

    | Or [] -> Bool false
    | Or (Or h :: t) -> simplify (Or (h @ t))
    | Or (Bool true :: _) -> Bool true
    | Or (Bool false :: t) -> simplify (Or t)
    | Or (h :: t) ->
        match simplify h with
        | h' when h = h' ->
            match simplify (Or t) with
            | And t' when t = t' -> Or (h :: t)
            | p -> simplify (Or [h ; p])
        | h' -> simplify (Or (h' :: t))

    | Stmt s -> Stmt s
    | Bool b -> Bool b
