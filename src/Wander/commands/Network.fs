// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Commands.Network

open Ligature.Model
open Wander.Model
open Ligature.Core
open Wander.Interpreter

let chompCommand =
    { Name = Element("chomp")
      Doc = "Merge the passed Network into named Network."
      Eval =
        fun _ networks (arguments: Arguments) ->
            match arguments with
            | [ Any.Network(input); Any.Element(name) ] ->
                // let currentNetwork = currentNetwork networks
                // let newNetwork = Set.union input currentNetwork
                // let newNetworks = Map.add selected newNetwork networks
                // Ok(selected, newNetworks, None)
                failwith "TODO"
            | _ -> error "Bad call to chomp." None }

// let isConsistentCommand =
//     { Name = Element("is-consistent?")
//       Doc = "Determine if a Network is consistent."
//       Eval =
//         fun _ store (arguments: Arguments) ->
//             match arguments with
//             | [ Value.Element(Element(networkName)) ] ->
//                 let value = store.IsConsistent networkName
//                 Ok(Some(Value.Element(value.ToString().ToLower() |> Element)))
//             | [ Value.Network(network) ] ->
//                 match isConsistent network with
//                 | value -> Ok(Some(Value.Element(value.ToString().ToLower() |> Element)))
//             //| Error err -> error "Bad call to is-consistent." None
//             | _ -> error "Bad call to is-consistent." None }

// let isCompleteCommand =
//     { Name = Element("is-complete?")
//       Doc = "Determine if a Network is complete."
//       Eval =
//         fun _ store (arguments: Arguments) ->
//             match arguments with
//             | [ Value.Element(Element(networkName)) ] ->
//                 let value = store.IsComplete networkName
//                 Ok(Some(Value.Element(value.ToString().ToLower() |> Element)))
//             | [ Value.Network(network) ] ->
//                 let value = isComplete network
//                 Ok(Some(Value.Element(value.ToString().ToLower() |> Element)))
//             | _ -> error "Bad call to is-complete." None }

let unionCommand =
    { Name = Element("union")
      Doc = "Find the union of two Networks."
      Eval =
        fun _ networks (arguments: Arguments) ->
            match arguments with
            | [ Any.Network(left); Any.Network(right) ] ->
                let result = Set.union left right |> Any.Network
                Ok(Some(result))
            | _ -> failwith "TODO" }

// let countCommand =
//     { Name = Element("count")
//       Doc = "Count the number of assertions in a Network."
//       Eval =
//         fun _ store (arguments: Arguments) ->
//             match arguments with
//             | [ Value.Element(Element name) ] ->
//                 match store.ReadNetwork name with
//                 | Ok network -> Ok(Some(Value.Element(Element(network.Count.ToString()))))
//                 | _ -> failwith "TODO"
//             | [ Value.Network network ] -> Ok(Some(Value.Element(Element(network.Count.ToString()))))
//             | _ -> failwith "TODO" }

let minusCommand =
    { Name = Element("minus")
      Doc = "Remove all Statements from the first Network that are in the second Networks."
      Eval =
        fun _ networks (arguments: Arguments) ->
            match arguments with
            | [ Any.Network(left); Any.Network(right) ] ->
                let result = Set.difference left right |> Any.Network
                Ok(Some(result))
            | _ -> failwith "TODO" }

// let applySingle (template: Network) (data: Map<Element, Element>) : Network =
//     Set.map
//         (fun entry ->
//             match entry with
//             | _ -> failwith "TODO"
//             // | Entry.Role { first = Element first
//             //                second = Element second
//             //                role = Element role } ->
//             //     let resFirst =
//             //         if first.StartsWith "?" then
//             //             match Map.tryFind (Element first) data with
//             //             | Some res -> res
//             //             | _ -> Element first
//             //         else
//             //             Element first

//             //     let resSecond =
//             //         if second.StartsWith "?" then
//             //             match Map.tryFind (Element second) data with
//             //             | Some res -> res
//             //             | _ -> Element second
//             //         else
//             //             Element second

//             //     let resRole =
//             //         if role.StartsWith "?" then
//             //             match Map.tryFind (Element role) data with
//             //             | Some res -> res
//             //             | _ -> Element role
//             //         else
//             //             Element role

//             //     Entry.Role
//             //         { first = resFirst
//             //           second = resSecond
//             //           role = resRole }
//             // | Entry.Extends _ -> failwith "TODO"
//             // | Entry.NotExtends _ -> failwith "TODO")
//         template

// let apply (template: Network) (data: Set<Map<Element, Element>>) : Network =
//     Set.fold (fun state value -> Set.union state (applySingle template value)) Set.empty data

// // let compareRole
// //     ({ first = Element pFirst
// //        second = Element pSecond
// //        role = Element pRole }: Role)
// //     ({ first = Element sFirst
// //        second = Element sSecond
// //        role = Element sRole }: Role)
// //     : Set<Map<Element, Element>> =
// //     let mutable fail = false
// //     let mutable result = Map.empty

// //     if pFirst.StartsWith "?" then
// //         if pFirst.Length > 1 then
// //             result <- Map.add (Element pFirst) (Element sFirst) result
// //     else
// //         fail <- pFirst <> sFirst

// //     if (not fail) && pSecond.StartsWith "?" then
// //         if pSecond.Length > 1 then
// //             result <- Map.add (Element pSecond) (Element sSecond) result
// //     else
// //         fail <- pSecond <> sSecond

// //     if (not fail) && pRole.StartsWith "?" then
// //         if pRole.Length > 1 then
// //             result <- Map.add (Element pRole) (Element sRole) result
// //     else
// //         fail <- pRole <> sRole

// //     if fail || result = Map.empty then
// //         Set.empty
// //     else
// //         Set.ofList [ result ]

// let compareExtension (pattern: Extends) (source: Extends) = failwith "TODO"

// let compareNonExtension (pattern: NotExtends) (source: NotExtends) = failwith "TODO"

// let findSingleEntry (pattern: Entry) (source: Entry) : Set<Map<Element, Element>> =
//     match pattern, source with
//     //    | Entry.Role pattern, Entry.Role source -> compareRole pattern source
//     | Entry.Extends pattern, Entry.Extends source -> compareExtension pattern source
//     | Entry.NotExtends pattern, Entry.NotExtends source -> compareNonExtension pattern source
//     | _ -> Set.empty

// let findEntry (pattern: Entry) (source: Network) : Set<Map<Element, Element>> =
//     Set.fold (fun state entry -> Set.union state (findSingleEntry pattern entry)) Set.empty source

// let find (pattern: Network) (source: Network) : Set<Map<Element, Element>> =
//     Set.fold (fun state part -> Set.union state (findEntry part source)) Set.empty pattern

// let queryCommand =
//     { Name = Element("query")
//       Doc = "arguments: pattern template data, returns network"
//       Eval =
//         fun commands variables arguments ->
//             match arguments with
//             | [ Any.Network pattern; Any.Network template; Any.Network source ] ->
//                 let results = find pattern source
//                 Ok(Some(Any.Network(apply template results)))
//             | _ -> error "Invalid call to query" None }

let matchCommand =
    { Name = Element "match"
      Doc = "Match a pattern against a network."
      Eval =
        fun commands variables arguments ->
            match arguments with
            | [ Any.Quote [ e; a; v ]; Any.Network network ] ->
                let element =
                    match e with
                    | Any.Element e -> ElementPattern.Element e
                    | Any.Variable v -> ElementPattern.Variable v
                    | _ -> failwith "TODO"

                let attribute =
                    match a with
                    | Any.Element e -> ElementPattern.Element e
                    | Any.Variable v -> ElementPattern.Variable v
                    | _ -> failwith "TODO"

                let value =
                    match v with
                    | Any.Element e -> ValuePattern.Element e
                    | Any.Variable v -> ValuePattern.Variable v
                    | Any.Literal l -> ValuePattern.Literal l
                    | _ -> failwith "TODO"

                Ok(Some(Any.ResultSet(networkMatch (element, attribute, value) network)))
            | _ -> failwith "TODO" }

let applyCommand =
    { Name = Element "apply"
      Doc = "Fill in Variables in a Pattern with values from a Result Set."
      Eval =
        fun commands variables arguments ->
            match arguments with
            | [ Any.Pattern pattern; Any.Quote q ] ->
                let resultSet =
                    match evalQuote commands variables q with
                    | Ok(Some(Any.ResultSet res)) -> res
                    | Ok _ -> failwith "TODO"
                    | Error err -> failwith "TODO"

                let res = apply pattern resultSet
                Ok(Some(Any.Pattern res))
            | [ Any.Network network; Any.Quote q ] ->
                let resultSet =
                    match evalQuote commands variables q with
                    | Ok(Some(Any.ResultSet res)) -> res
                    | Ok _ -> failwith "TODO"
                    | Error err -> failwith "TODO"

                let res = apply (networkToPattern network) resultSet
                Ok(Some(Any.Pattern res))
            | _ -> failwith "TODO" }

// let filterCommand =
//     { Name = Element("filter")
//       Doc = "arguments: pattern data, returns network"
//       Eval =
//         fun commands networks arguments ->
//             match arguments with
//             | [ Any.Pattern pattern; Any.Network source ] ->
//                 let results = filter pattern source
//                 Ok(Some(Any.Network results))
//             | _ -> error "Invalid call to query" None }

let networkCommands: Map<Element, Command> =
    (Map.ofList
        [ (applyCommand.Name, applyCommand)
          (chompCommand.Name, chompCommand)
          //   (countCommand.Name, countCommand)
          (minusCommand.Name, minusCommand)
          (matchCommand.Name, matchCommand)
          //   (queryCommand.Name, queryCommand)
          (unionCommand.Name, unionCommand)
          //(isCompleteCommand.Name, isCompleteCommand)
          //(isConsistentCommand.Name, isConsistentCommand)
          ])
