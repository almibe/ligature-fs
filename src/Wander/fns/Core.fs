// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Fns.Core

open Wander.Model
open Wander.Interpreter
open Ligature.Model

let idFn: Fn =
    Fn.Fn(
        { doc = "Return the value passed."
          examples = []
          args = "Any"
          result = "Any" },
        fun _ _ application ->
            match application.arguments with
            | [ value ] -> Ok value
            | _ -> failwith "Unexpected value passed to id."
    )

let fnFn: Fn =
    Fn.Macro(
        { doc = "Create a lambda."
          examples = [ "Fn.Fn(test)"; "Fn.Fn($arg -> count($arg))" ]
          args = "Any"
          result = "Any" },
        fun actions variables application ->
            let readArrow, variables, (body: Option<Script>) =
                List.fold
                    (fun (readArrow, variables, body) value ->
                        if readArrow then
                            match value with
                            | Expression.Term(Term "->") -> failwith "Error multiple -> in fn definition."
                            | ex ->
                                match body with
                                | None -> readArrow, variables, Some [ None, ex ]
                                | Some body -> readArrow, variables, Some(List.append body [ None, ex ])
                        else
                            match value with
                            | Expression.Term(Term "->") -> true, variables, body
                            | Expression.Variable variable ->
                                match variables with
                                | None -> readArrow, Some [ variable ], body
                                | Some variables -> readArrow, Some(List.append variables [ variable ]), body
                            | _ -> failwith "Error expected variable or ->.")
                    (false, None, None)
                    application.arguments

            let variables =
                match variables with
                | Some values -> values
                | None -> []

            let body =
                match body with
                | Some values -> values
                | None -> []

            Ok(Expression.Lambda(variables, body))
    )

let doFn: Fn =
    Fn.Fn(
        { doc = "Create a new scope and execute a series of expressions."
          examples = [ "do (test)" ]
          args = "Any"
          result = "Any" },
        fun actions variables application ->
            List.fold
                (fun state expression -> executeExpression actions variables expression)
                (Ok(Expression.Term(Term "")))
                application.arguments
    )

let pipeFn: Fn =
    Fn.Fn(
        { doc =
            "Execute a sequence of function calls passing the result of the first as the last argument to the next and returning the final result."
          examples = [ "pipe(assertions(triple(a b c)) count())" ]
          args = "Any..."
          result = "Any" },
        fun actions variables application ->
            List.fold
                (fun state expression -> executeExpression actions variables expression)
                (Ok(Expression.Term(Term "")))
                application.arguments
    )

let seqFn: Fn =
    Fn.Fn(
        { doc = "Create a seq."
          examples = [ "seq(test)" ]
          args = "Any"
          result = "Seq" },
        fun _ _ application -> Ok(Expression.Seq application.arguments)
    )

let mapFn: Fn =
    Fn.Fn(
        { doc = "Map over a seq."
          examples = [ "map(id seq(1 2 3))" ]
          args = "Mapper Seq"
          result = "Seq" },
        fun fns variables application ->
            match application.arguments with
            | [ Expression.Term mapper; Expression.Seq seq ] ->
                match fns.TryFind mapper with
                | Some(Fn.Fn(_, fn)) ->
                    let values =
                        List.map
                            (fun value ->
                                match
                                    fn
                                        fns
                                        variables
                                        { name = Term ""
                                          attributes = Map.empty
                                          arguments = [ value ] }
                                with
                                | Ok res -> res
                                | _ -> failwith "TODO")
                            seq

                    Ok(Expression.Seq values)
                | _ -> failwith "TODO"
            | _ -> failwith "TODO"
    )

// let importCommand: Command =
//     { Eval =
//         fun networks local modules arguments ->
//             let mutable local = local

//             match arguments with
//             | [ Any.Term importName ] ->
//                 match modules.TryFind importName with
//                 | Some res ->
//                     Map.iter (fun name command -> local <- Map.add name command local) res
//                     Ok(None, networks, local, modules, variables)
//                 | None -> failwith "TODO"
//             | _ -> error "Illegal call to import." None }


// let readCommand: Command =
//     { Eval =
//         fun networks local modules arguments ->
//             match arguments with
//             | [ Any.Slot(name) ] ->
//                 if variables.ContainsKey name then
//                     Ok((Snetworks, local, modules))
//                 else
//                     error "Could not read variable" None
//             | _ -> error "Illegal call to read." None }

// let evalCommand: Fn =
//     { Eval =
//         fun networks local modules arguments ->
//             match arguments with
//             | [ Any.Tuple(tuple) ] -> evalTuple networks local modules tuple
//             | _ -> error "Illegal call to read." None }

// let foldCommand: Fn =
//     { Eval =
//         fun networks local modules arguments ->
//             match arguments with
//             | [ tuple; initialNetwork; resultSet ] ->
//                 let tuple =
//                     match tuple with
//                     | Any.Tuple q -> q
//                     | _ -> failwith "TODO"

//                 let initialNetwork =
//                     match initialNetwork with
//                     | Any.Network n -> n
//                     | _ -> failwith "TODO"

//                 let resultSet =
//                     match resultSet with
//                     | Any.ResultSet rs -> rs
//                     | Any.Tuple q ->
//                         match evalTuple networks local modules q with
//                         | Ok(_, _, _) -> failwith "TODO"
//                         | _ -> failwith "TODO"
//                     | _ -> failwith "TODO"

//                 let res =
//                     Set.fold
//                         (fun s x ->
//                             let variables = Map.add (Slot "?_") (Any.ValueSet x)

//                             match evalTuple networks local modules tuple with
//                             | Ok(_, _, _) -> failwith "TODO"
//                             | Error err -> failwith $"TODO - error in fold - {err.UserMessage}")
//                         initialNetwork
//                         resultSet

//                 Ok(networks, local, modules)
//             | _ -> error "Illegal call to fold." None }

// let ignoreCommand: Fn =
//     { Eval = fun networks local modules _ -> Ok(networks, local, modules) }

// let printSignature ((arguments, result): WanderType list * WanderType option) : Term =
//     Term($"{arguments} -> {result}")
// List.map
//     (fun t ->
//         match t with
//         | LigatureType.Bytes -> Identifier.Name(Name("Bytes"))
//         | LigatureType.Int -> Identifier.Name(Name("Int"))
//         | LigatureType.Name -> Identifier.Name(Name("Name"))
//         | LigatureType.Network -> Identifier.Name(Name("Network"))
//         | LigatureType.NetworkName -> Identifier.Name(Name("NetworkName"))
//         | LigatureType.Tuple -> Identifier.Name(Name("Tuple"))
//         | LigatureType.Slot -> Identifier.Name(Name("Slot"))
//         | LigatureType.String -> Identifier.Name(Name("String"))
//         | LigatureType.Expression -> Identifier.Name(Name("Expression"))
//         | LigatureType.Value -> Identifier.Name(Name("Value")))
//     signature

// let docsCommand: Fn =
//     { Eval = fun local modules variables _ -> failwith "TODO" }
// let mutable docs: Network = Set.empty

// Map.toList modules
// |> List.iter (fun (Term moduleName, m) ->
//     Map.toList m
//     |> List.iter (fun (Term name, command) ->
//         docs <-
//             Set.add
//                 (TermPattern.Term(Term $"{moduleName}.{name}"),
//                  TermPattern.Term(Term("docString")),
//                  Value.Literal(command.Doc))
//                 docs

//         ()))

// Ok((Some(Any.Network docs), local, modules, variables)) }

// let containsCommand: Command =
//     { Name = Term "contains"
//       Doc = "Test if one network contains another."
//       Eval =
//         fun _ _ arguments ->
//             match arguments with
//             | [ Any.Network test; Any.Network data ] ->
//                 if contains test data then
//                     Ok(Some(Any.Term(Term "true")))
//                 else
//                     Ok(Some(Any.Term(Term "false")))
//             | _ -> error "Illegal call to contains" None }

// let resultSetCommand: Fn =
//     { Eval =
//         fun actions network ->
//             let mutable resultSet = Set.empty

//             List.iter
//                 (fun arg ->
//                     match arg with
//                     | Any.Tuple q ->
//                         let mutable variables = Map.empty

//                         List.iter
//                             (fun chunk ->
//                                 match chunk with
//                                 | [ Any.Slot v; Any.Term e ] ->
//                                     variables <- Map.add v (Value.Term e) variables
//                                 | [ Any.Slot v; Any.Literal l ] ->
//                                     variables <- Map.add v (Value.Literal l) variables
//                                 | _ -> failwith "TODO")
//                             (q |> List.chunkBySize 2)

//                         resultSet <- Set.add variables resultSet
//                     | _ -> failwith "TODO")
//                 arguments

//             Ok(networks, local, modules) }
