// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Fns.Network

open Ligature.Model
open Wander.Model
open Ligature.Core
open Wander.Interpreter

let unionFn =
    Fn(
        { doc = "Combine the top two Networks on the Stack and push the resulting Network."
          examples = [ "{a b c} {d e f} union\n{a b c, d e f} assert-equal" ]
          pre = "Network Network"
          post = "Network" },
        fun actions variables arguments -> failwith "TODO"
    // match stack with
    // | Any.Network left :: Any.Network right :: tail ->
    //     // let left =
    //     //     match left with
    //     //     | Any.Network n -> n
    //     //     | Any.Slot v ->
    //     //         match Map.tryFind v variables with
    //     //         | Some(Any.Network res) -> res
    //     //         | _ -> failwith "TODO"
    //     //     | _ -> failwith "TODO"

    //     // let right =
    //     //     match right with
    //     //     | Any.Network n -> n
    //     //     | Any.Slot v ->
    //     //         match Map.tryFind v variables with
    //     //         | Some(Any.Network res) -> res
    //     //         | _ -> failwith "TODO"
    //     //     | Any.Quote quote ->
    //     //         match evalQuote networks local modules variables quote with
    //     //         | Ok((Some(Any.Network network), _, _, _, _)) -> network
    //     //         | _ -> failwith "TODO"
    //     //     | _ -> failwith "TODO"
    //     let result = Set.union left right |> Any.Network
    //     Ok(result :: tail)
    // | _ -> failwith $"Calls to union requires two Networks on the stack."
    )

let countFn =
    Fn(
        { doc = "Take a Network from the top of the Stack and push its size."
          examples = [ "{} count 0 assert-equal" ]
          pre = "Network"
          post = "Literal" },
        fun actions variables arguments -> failwith "TODO"
    // match stack with
    // | [ Any.Network n ] -> Ok([ Any.Term(Term((Set.count n).ToString())) ])
    // | Any.Network n :: tail -> Ok(Any.Term(Term((Set.count n).ToString())) :: tail)
    // | _ -> error "Network on stack required to call count." None
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
// | [ Any.Quote quote ] ->
//     match evalQuote networks local modules quote with
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
          pre = "Template Pattern Network"
          post = "TemplateResult" },
        fun actions variables arguments -> failwith "TODO"
    // match stack with
    // | Any.Network template :: Any.Network pattern :: Any.Network source :: tail ->
    //     let results =
    //         query pattern template source
    //         |> Seq.map (fun network -> Any.Network network)
    //         |> Seq.toList

    //     Ok(Any.Quote results :: tail)
    // // | Any.Quote template :: Any.Network pattern :: Any.Network source :: tail ->
    // //     let results =
    // //         queryQuoteTemplate pattern template source
    // //         |> Seq.map (fun quote -> Any.Quote quote)
    // //         |> Seq.toList

    // //     Ok(Any.Quote results :: tail)
    // | _ -> error "Invalid call to query" None
    )

// let matchCommand =
//     { Eval =
//         fun networks local modules arguments ->
//             match arguments with
//             | [ Any.Quote [ e; a; v ]; Any.Network network ] ->
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
//                     | Any.Quote q ->
//                         match evalQuote networks local modules variables q with
//                         | Ok((Some(Any.Network n), networks, local, modules, variables)) -> n
//                         | _ -> failwith "TODO"
//                     | _ -> failwith "TODO"

//                 let network =
//                     match network with
//                     | Any.Network n -> n
//                     | Any.Quote q ->
//                         match evalQuote networks local modules variables q with
//                         | Ok((Some(Any.Network n), networks, local, modules, variables)) -> n
//                         | _ -> failwith "TODO"
//                     | _ -> failwith "TODO"

//                 Ok(Some(Any.ResultSet(networkMatch pattern network)), networks, local, modules, variables)

//             | _ -> failwith "TODO" }

// let applyCommand =
//     { Eval =
//         fun networks local modules variables arguments ->
//             match arguments with
//             | [ Any.Network network; Any.Quote q ] ->
//                 match evalQuote networks local modules variables q with
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
          pre = "Pattern Network"
          post = "Network" },
        fun actions variables arguments -> failwith "TODO"
    // match stack with
    // | Any.Network pattern :: Any.Network source :: tail ->
    //     // let pattern =
    //     //     match pattern with
    //     //     | Any.Network n -> n
    //     //     | Any.Slot v ->
    //     //         if variables.ContainsKey v then
    //     //             match variables[v] with
    //     //             | Any.Network n -> n
    //     //             | _ -> failwith "TODO"
    //     //         else
    //     //             failwith "TODO"
    //     //     | Any.Quote quote ->
    //     //         match evalQuote networks local modules variables quote with
    //     //         | Ok((Some(Any.Network n), networks, local, modules)) -> n
    //     //         | _ -> failwith "TODO"
    //     //     | _ -> failwith "TODO"

    //     // let source =
    //     //     match source with
    //     //     | Any.Network n -> n
    //     //     | Any.Slot v ->
    //     //         if variables.ContainsKey v then
    //     //             match variables[v] with
    //     //             | Any.Network n -> n
    //     //             | _ -> failwith "TODO"
    //     //         else
    //     //             failwith "TODO"
    //     //     | Any.Quote quote ->
    //     //         match evalQuote networks local modules variables quote with
    //     //         | Ok((Some(Any.Network n), networks, local, modules)) -> n
    //     //         | _ -> failwith "TODO"
    //     //     | _ -> failwith "TODO"

    //     let results = filter pattern source
    //     Ok(Any.Network results :: tail)
    // | _ -> error "Invalid call to filter" None
    )

let ifEmptyFn =
    Fn(
        { doc = "Takes three Terms..."
          examples = []
          pre = ""
          post = "" },
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
            "Takes a Network or Quote off the top of the Stack and pushes \"true\" if it is empty or \"false\" if not."
          examples = []
          pre = ""
          post = "" },
        fun actions variables arguments -> failwith "TODO"
    // match stack with
    // | Any.Network cond :: tail ->
    //     if cond = Set.empty then
    //         Ok(Any.Term(Term "true") :: tail)
    //     else
    //         Ok(Any.Term(Term "false") :: tail)
    // | Any.Quote q :: tail ->
    //     if q.IsEmpty then
    //         Ok(Any.Term(Term "true") :: tail)
    //     else
    //         Ok(Any.Term(Term "false") :: tail)
    // | _ -> error "Invalid call to is-empty" None
    )
