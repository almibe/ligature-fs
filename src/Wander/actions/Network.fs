// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Fns.Network

open Ligature.Model
open Wander.Model
open Ligature.Core
open Wander.Interpreter

let rec recordToNetwork (record: Record) : Result<Assertions, LigatureError> =
    match Map.tryFind (Any.Term(Term "@")) record with
    | Some(Any.Term id) ->
        Seq.fold
            (fun state (key, value) ->
                if key = Any.Term(Term "@") then
                    state
                else
                    let role: Term =
                        match key with
                        | Any.Term term -> term
                        | _ -> failwith "TODO"

                    match value with
                    | Any.Literal literal -> Set.add (Assertion.Triple(id, role, Value.Literal literal)) state
                    | Any.Term term -> Set.add (Assertion.Triple(id, role, Value.Term term)) state
                    | Any.Tuple tuple ->
                        List.fold
                            (fun state value ->
                                match value with
                                | Any.Literal literal ->
                                    Set.add (Assertion.Triple(id, role, Value.Literal literal)) state
                                | Any.Term term -> Set.add (Assertion.Triple(id, role, Value.Term term)) state
                                | _ -> failwith "TODO")
                            state
                            tuple
                    | Any.Record record ->
                        let state =
                            match record.TryFind(Any.Term(Term "@")) with
                            | Some(Any.Term value) -> Set.add (Assertion.Triple(id, role, Value.Term value)) state
                            | _ -> failwith "TODO"

                        match recordToNetwork record with
                        | Ok network -> state + network
                        | _ -> failwith "TODO"
                    | _ -> failwith "TODO")
            Set.empty
            (Map.toSeq record)
        |> Ok
    | _ -> error "Record requires valid @ entry." None

let rec recordToPattern (record: Record) : Result<Pattern, LigatureError> = failwith "TODO"
// match Map.tryFind (Any.Term(Term "@")) record with
// | Some id ->
//     let id =
//         match id with
//         | Any.Term term -> TermPattern.Term term
//         | Any.Slot slot -> TermPattern.Slot slot
//         | _ -> failwith "TODO"

//     Seq.fold
//         (fun state (key, value) ->
//             if key = Any.Term(Term "@") then
//                 state
//             else
//                 let role: TermPattern =
//                     match key with
//                     | Any.Term term -> TermPattern.Term term
//                     | Any.Slot slot -> TermPattern.Slot slot
//                     | _ -> failwith "TODO"

//                 match value with
//                 | Any.Literal literal -> Set.add (id, role, ValuePattern.Literal literal) state
//                 | Any.Term term -> Set.add (id, role, ValuePattern.Term term) state
//                 | Any.Slot slot -> Set.add (id, role, ValuePattern.Slot slot) state
//                 | Any.Tuple tuple ->
//                     List.fold
//                         (fun state value ->
//                             match value with
//                             | Any.Literal literal -> Set.add (id, role, ValuePattern.Literal literal) state
//                             | Any.Term term -> Set.add (id, role, ValuePattern.Term term) state
//                             | Any.Slot slot -> Set.add (id, role, ValuePattern.Slot slot) state
//                             | _ -> failwith "TODO")
//                         state
//                         tuple
//                 | Any.Record record ->
//                     let state =
//                         match record.TryFind(Any.Term(Term "@")) with
//                         | Some(Any.Term value) -> Set.add (id, role, ValuePattern.Term value) state
//                         | Some(Any.Slot slot) -> Set.add (id, role, ValuePattern.Slot slot) state
//                         | _ -> failwith "TODO"

//                     match recordToPattern record with
//                     | Ok pattern -> state + pattern
//                     | _ -> failwith "TODO"
//                 | _ -> failwith "TODO")
//         Set.empty
//         (Map.toSeq record)
//     |> Ok
// | _ -> error "Record requires valid @ entry." None

let assertionsFn =
    Fn(
        { doc = "Create an ABox."
          examples = [ "(assertions (isa betty Cat))" ]
          args = "Assertion..."
          result = "Assertions" },
        fun _ _ _ arguments ->
            let mutable res: Assertions = Set.empty

            List.iter
                (fun arg ->
                    match arg with
                    | Any.Assertion assertion -> res <- Set.add assertion res
                    | Any.Tuple [ e; a; v ] ->
                        let e =
                            match e with
                            | Any.Term t -> t
                            | _ -> failwith "Invalid call to assertions."

                        let a =
                            match a with
                            | Any.Term t -> t
                            | _ -> failwith "Invalid call to assertions."

                        let v =
                            match v with
                            | Any.Term t -> Value.Term t
                            | Any.Literal l -> Value.Literal l
                            | _ -> failwith "Invalid call to assertions."

                        res <- Set.add (Assertion.Triple(e, a, v)) res
                    | Any.Record record ->
                        match recordToNetwork record with
                        | Ok network -> res <- res + network
                        | _ -> failwith "TODO"
                    | _ -> failwith "Invalid call to assertions.")
                arguments

            Ok(Any.Assertions res)
    )

// let patternFn =
//     Fn(
//         { doc = "Create a Pattern from triples."
//           examples = [ "pattern [?a b c] [?a e f]" ]
//           args = "Tuple..."
//           result = "Pattern" },
//         fun _ _ _ arguments ->
//             let mutable res: Pattern = Set.empty

//             List.iter
//                 (fun arg ->
//                     match arg with
//                     | Any.Tuple [ e; a; v ] ->
//                         let e =
//                             match e with
//                             | Any.Term t -> TermPattern.Term t
//                             | Any.Slot s -> TermPattern.Slot s
//                             | _ -> failwith "Invalid call to pattern."

//                         let a =
//                             match a with
//                             | Any.Term t -> TermPattern.Term t
//                             | Any.Slot s -> TermPattern.Slot s
//                             | _ -> failwith "Invalid call to pattern."

//                         let v =
//                             match v with
//                             | Any.Term t -> ValuePattern.Term t
//                             | Any.Slot s -> ValuePattern.Slot s
//                             | Any.Literal l -> ValuePattern.Literal l
//                             | _ -> failwith "Invalid call to pattern."

//                         res <- Set.add (e, a, v) res
//                     | Any.Record record ->
//                         match recordToPattern record with
//                         | Ok pattern -> res <- res + pattern
//                         | _ -> failwith "TODO"
//                     | _ -> failwith "Invalid call to pattern.")
//                 arguments

//             Ok(Any.Pattern res)
//     )

let unionFn =
    Fn(
        { doc = "Combine the top two Networks on the Stack and push the resulting Network."
          examples = [ "{a b c} {d e f} union\n{a b c, d e f} assert-equal" ]
          args = "Network Network"
          result = "Network" },
        fun _ _ _ arguments ->
            match arguments with
            | [ Any.Assertions left; Any.Assertions right ] ->
                // let left =
                //     match left with
                //     | Any.Network n -> n
                //     | Any.Slot v ->
                //         match Map.tryFind v variables with
                //         | Some(Any.Network res) -> res
                //         | _ -> failwith "TODO"
                //     | _ -> failwith "TODO"

                // let right =
                //     match right with
                //     | Any.Network n -> n
                //     | Any.Slot v ->
                //         match Map.tryFind v variables with
                //         | Some(Any.Network res) -> res
                //         | _ -> failwith "TODO"
                //     | Any.Tuple tuple ->
                //         match evalTuple networks local modules variables tuple with
                //         | Ok((Some(Any.Network network), _, _, _, _)) -> network
                //         | _ -> failwith "TODO"
                //     | _ -> failwith "TODO"
                let result = Set.union left right |> Any.Assertions
                Ok result
            | _ -> failwith $"Calls to union requires two Networks on the stack."
    )

let countFn =
    Fn(
        { doc = "Take a Network from the top of the Stack and push its size."
          examples = [ "{} count 0 assert-equal" ]
          args = "Network"
          result = "Literal" },
        fun _ _ _ arguments ->
            match arguments with
            | [ Any.Assertions n ] -> Ok(Any.Literal(Literal((Set.count n).ToString())))
            | _ -> error "Network on stack required to call count." None
    )
// match arguments with
// // | [ Any.Slot variable ] ->
// //     match variables.TryFind variable with
// //     | Some(Any.Network network) ->
// //         Ok(
// //             (Some(Any.Term(Term((Set.count network).ToString()))), networks, local, modules, variables)
// //         )
// //     | _ -> failwith "TODO"
// | [ Any.Network network ] ->
//     Ok((networks, local, modules))
// | [ Any.Tuple tuple ] ->
//     match evalTuple networks local modules tuple with
//     | Ok(networks, local, modules) ->
//         Ok(networks, local, modules)
//     | Ok(_, _, _) -> error "Error in count, expected value." None
//     | Error err -> error $"Error in count, {err.UserMessage}" None
// | args -> failwith $"TODO - {args}" }

// let minusCommand =
//     { Eval =
//         fun networks local modules (arguments: Arguments) ->
//             match arguments with
//             | [ Any.Network(left); Any.Network(right) ] ->
//                 let result = Set.difference left right |> Any.Network
//                 Ok(networks, local, modules)
//             | _ -> failwith "TODO" }

let queryFn =
    Fn(
        { doc = "Query a network. This Fn requires three Networks on the stack."
          examples = []
          args = "Pattern Network"
          result = "ResultSet" },
        fun _ _ _ arguments ->
            match arguments with
            | [ Any.Pattern pattern; Any.Assertions source ] ->
                let results = query pattern source
                Ok(Any.ResultSet results)
            | _ -> error "Invalid call to query" None
    )

// let matchCommand =
//     { Eval =
//         fun networks local modules arguments ->
//             match arguments with
//             | [ Any.Tuple [ e; a; v ]; Any.Network network ] ->
//                 let element =
//                     match e with
//                     | Any.Term e -> TermPattern.Term e
//                     | Any.Slot v -> TermPattern.Slot v
//                     | _ -> failwith "TODO"

//                 let attribute =
//                     match a with
//                     | Any.Term e -> TermPattern.Term e
//                     | Any.Slot v -> TermPattern.Slot v
//                     | _ -> failwith "TODO"

//                 let value =
//                     match v with
//                     | Any.Term e -> Value.Term e
//                     | Any.Slot v -> Value.Slot v
//                     | Any.Literal l -> Value.Literal l
//                     | _ -> failwith "TODO"

//                 Ok(
//                     (Some(Any.ResultSet(singleMatch (element, attribute, value) network)),
//                      networks,
//                      local,
//                      modules,
//                      variables)
//                 )
//             | [ pattern; network ] ->
//                 let pattern =
//                     match pattern with
//                     | Any.Network n -> n
//                     | Any.Tuple q ->
//                         match evalTuple networks local modules variables q with
//                         | Ok((Some(Any.Network n), networks, local, modules, variables)) -> n
//                         | _ -> failwith "TODO"
//                     | _ -> failwith "TODO"

//                 let network =
//                     match network with
//                     | Any.Network n -> n
//                     | Any.Tuple q ->
//                         match evalTuple networks local modules variables q with
//                         | Ok((Some(Any.Network n), networks, local, modules, variables)) -> n
//                         | _ -> failwith "TODO"
//                     | _ -> failwith "TODO"

//                 Ok(Some(Any.ResultSet(networkMatch pattern network)), networks, local, modules, variables)

//             | _ -> failwith "TODO" }

// let applyCommand =
//     { Eval =
//         fun networks local modules variables arguments ->
//             match arguments with
//             | [ Any.Network network; Any.Tuple q ] ->
//                 match evalTuple networks local modules variables q with
//                 | Ok((Some(Any.ResultSet res), networks, local, modules, variables)) ->
//                     let res = apply network res
//                     Ok((Some(Any.Network res), networks, local, modules, variables))
//                 | Ok((Some(Any.ValueSet res), networks, local, modules, variables)) ->
//                     let res = applyValueSet network res
//                     Ok((Some(Any.Network res), networks, local, modules, variables))
//                 | Ok _ -> failwith "TODO"
//                 | Error err -> error $"Error in apply. {err.UserMessage}" None
//             | [ Any.Network network; Any.Slot v ] ->
//                 match Map.tryFind v variables with
//                 | Some(Any.ResultSet res) ->
//                     let res = apply network res
//                     Ok((Some(Any.Network res), networks, local, modules, variables))
//                 | Some(Any.ValueSet res) ->
//                     let res = applyValueSet network res
//                     Ok((Some(Any.Network res), networks, local, modules, variables))
//                 | Some _ -> failwith "TODO"
//                 | None -> failwith "TODO"
//             | args -> failwith $"TODO - unexpected args {args}" }


let filterFn =
    Fn(
        { doc = "Accepts two Networks. First a Pattern and then a Network to search. Pushes the matching Network."
          examples = []
          args = "Pattern Network"
          result = "Network" },
        fun _ _ _ arguments ->
            match arguments with
            | [ Any.Pattern pattern; Any.Assertions source ] ->
                // let pattern =
                //     match pattern with
                //     | Any.Network n -> n
                //     | Any.Slot v ->
                //         if variables.ContainsKey v then
                //             match variables[v] with
                //             | Any.Network n -> n
                //             | _ -> failwith "TODO"
                //         else
                //             failwith "TODO"
                //     | Any.Tuple tuple ->
                //         match evalTuple networks local modules variables tuple with
                //         | Ok((Some(Any.Network n), networks, local, modules)) -> n
                //         | _ -> failwith "TODO"
                //     | _ -> failwith "TODO"

                // let source =
                //     match source with
                //     | Any.Network n -> n
                //     | Any.Slot v ->
                //         if variables.ContainsKey v then
                //             match variables[v] with
                //             | Any.Network n -> n
                //             | _ -> failwith "TODO"
                //         else
                //             failwith "TODO"
                //     | Any.Tuple tuple ->
                //         match evalTuple networks local modules variables tuple with
                //         | Ok((Some(Any.Network n), networks, local, modules)) -> n
                //         | _ -> failwith "TODO"
                //     | _ -> failwith "TODO"

                let results = filter pattern source
                Ok(Any.Assertions results)
            | _ -> error "Invalid call to filter" None
    )

let ifEmptyFn =
    Fn(
        { doc = "Takes three Terms..."
          examples = []
          args = ""
          result = "" },
        fun actions variables arguments -> failwith "TODO"
    // match stack with
    // | elseCase :: emptyCase :: Any.Network cond :: tail ->
    //     if cond = Set.empty then
    //         Ok(emptyCase :: tail)
    //     else
    //         Ok(elseCase :: tail)
    // | _ -> error "Invalid call to if-empty" None
    )

let isEmptyFn =
    Fn(
        { doc =
            "Takes a Network or Tuple off the top of the Stack and pushes \"true\" if it is empty or \"false\" if not."
          examples = []
          args = ""
          result = "" },
        fun actions variables arguments -> failwith "TODO"
    // match stack with
    // | Any.Network cond :: tail ->
    //     if cond = Set.empty then
    //         Ok(Any.Term(Term "true") :: tail)
    //     else
    //         Ok(Any.Term(Term "false") :: tail)
    // | Any.Tuple q :: tail ->
    //     if q.IsEmpty then
    //         Ok(Any.Term(Term "true") :: tail)
    //     else
    //         Ok(Any.Term(Term "false") :: tail)
    // | _ -> error "Invalid call to is-empty" None
    )
