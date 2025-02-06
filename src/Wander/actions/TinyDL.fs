// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

module Wander.Actions.TinyDL

open Ligature.Model
open Wander.Model
open Wander.Interpreter

[<RequireQualifiedAccess>]
type JsonViewValue =
    | Literal of string
    | Element of JsonView

and JsonView = {
    Id: string
    Attrs: Map<string, Set<JsonViewValue>>
}

let rec writeValues (values: Set<JsonViewValue>): string =
    (Set.fold (fun state value -> 
        let value = 
            match value with
            | JsonViewValue.Element element -> writeJsonView element
            | JsonViewValue.Literal literal -> encodeString literal
        if state = "[" then
            state + value
        else
            state + "," + value) "[" values) + "]"

and writeJsonView (view: JsonView): string =
    let mutable res = "{"
    
    res <- res + $"\"id\":{encodeString view.Id}"

    Map.iter (fun key values ->
        res <- res + $",{encodeString key}:{writeValues values}") view.Attrs

    res <- res + "}"
    res

let rec infer (tBox: Network) (aBox: Network) : Result<Network, LigatureError> =
    let mutable res = aBox

    Set.iter
        (fun tStatement ->
            Set.iter
                (fun aStatement ->
                    match (tStatement, aStatement) with
                    | (ElementPattern.Element subconcept,
                       ElementPattern.Element(Element "subconcept-of"),
                       Value.Element superconcept),
                      (ElementPattern.Element element, ElementPattern.Element(Element ":"), Value.Element concept) when
                        subconcept = concept
                        ->
                        res <-
                            Set.add
                                (ElementPattern.Element element,
                                 ElementPattern.Element(Element ":"),
                                 Value.Element superconcept)
                                res
                    | (ElementPattern.Element firstRole,
                       ElementPattern.Element(Element "tdl.inverse-of"),
                       Value.Element secondRole),
                      (ElementPattern.Element first, ElementPattern.Element role, Value.Element second) when
                        role = firstRole
                        ->
                        res <-
                            Set.add
                                (ElementPattern.Element second, ElementPattern.Element secondRole, Value.Element first)
                                res
                    | (ElementPattern.Element firstRole,
                       ElementPattern.Element(Element "tdl.inverse-of"),
                       Value.Element secondRole),
                      (ElementPattern.Element first, ElementPattern.Element role, Value.Element second) when
                        role = secondRole
                        ->
                        res <-
                            Set.add
                                (ElementPattern.Element second, ElementPattern.Element firstRole, Value.Element first)
                                res
                    | (ElementPattern.Element roleName,
                       ElementPattern.Element(Element ":"),
                       Value.Element(Element "tdl.Is-Symmetrical")),
                      (ElementPattern.Element first, ElementPattern.Element role, Value.Element second) when
                        role = roleName
                        ->
                        res <-
                            Set.add
                                (ElementPattern.Element second, ElementPattern.Element role, Value.Element first)
                                res
                    | _ -> ())
                aBox)
        tBox

    if aBox = res then Ok res else infer tBox res

let extract (source: Network) (id: Element): Network =
    let mutable checkedIds = Set.ofList [id]
    let mutable toCheck = [id]
    let mutable result = Set.empty
    while not toCheck.IsEmpty do
        let current = toCheck.Head
        toCheck <- toCheck.Tail
        let matched =
            Set.filter (fun (element, a, v) -> 
                match element with
                | ElementPattern.Element e -> 
                    if e = current then
                        match v with
                        | Value.Element e ->
                            if not (checkedIds.Contains(e)) then
                                toCheck <- e :: toCheck
                                checkedIds <- Set.add e checkedIds
                            else
                                ()
                        | _ -> ()
                        true
                    else
                        false
                | _ -> false) source
        result <- Set.union matched result
    result

let extractAction: Action =
    Action.Full(
        { doc = "..."; examples = []; pre = ""; post = "" },
        fun _ stack ->
            match stack with
            | Any.Quote ids :: Any.Network source :: tail ->
                let result: AnySet =
                    List.fold (fun state concept -> 
                        match concept with
                        | Any.Element concept -> 
                            Set.add (Any.Network (extract source concept)) state
                        | _ -> failwith "TODO") (Set.empty) ids
                Ok(Any.AnySet result :: tail)
            | _ -> failwith "TODO"
    )

let rec createJsonView (source: Network) (Element root): JsonView =
    let mutable attrs = Map.empty
    Set.iter (fun triple ->
        match triple with
        | (ElementPattern.Element element, ElementPattern.Element (Element attribute), value) ->
            if element = Element root then
                let value =
                    match value with
                    | Value.Literal l -> JsonViewValue.Literal l
                    | Value.Element e -> JsonViewValue.Element (createJsonView source e)
                    | _ -> failwith "TODO"
                if attrs.ContainsKey(attribute) then
                    failwith "TODO"
                else
                    let values = Set.ofList [ value ]
                    attrs <- Map.add attribute values attrs
        | _ -> failwith "TODO") source
    let view = { Id = root; Attrs = attrs }
    view

let extractJson (ids: Quote) (source: Network): string =
    let mutable result = "["
    List.iteri (fun index id -> 
        match id with
        | Any.Element id ->
            if (index > 0) then
                result <- result + ","
            result <- result + (createJsonView (extract source id) id |> writeJsonView)
        | _ -> failwith "TODO") ids
    result <- result + "]"
    result

let extractJsonAction: Action =
    Action.Full(
        { doc = "..."; examples = []; pre = ""; post = "" },
        fun _ stack ->
            match stack with
            | Any.Quote ids :: Any.Network source :: tail ->
                let json = extractJson ids source
                Ok(Any.Literal json :: tail)
            | _ -> failwith "TODO"
    )

let instances (source: Network) (concept: Element): AnySet =
    Set.fold (fun state triple -> 
        match triple with
        | ElementPattern.Element element, ElementPattern.Element (Element ":"), Value.Element conceptToCheck ->
            if conceptToCheck = concept then
                Set.add (Any.Network (extract source element)) state
            else
                state
        | _ -> state) Set.empty source

let instancesAction: Action =
    Action.Full(
        { doc = "..."; examples = []; pre = ""; post = "" },
        fun _ stack ->
            match stack with
            | Any.Quote concepts :: Any.Network source :: tail ->
                let result: AnySet =
                    List.fold (fun state concept -> 
                        match concept with
                        | Any.Element concept -> 
                            instances source concept
                        | _ -> failwith "TODO") (Set.empty) concepts
                Ok(Any.AnySet result :: tail)
            | _ -> failwith "TODO"
    )

let instancesJsonAction: Action =
    Action.Full(
        { doc = "..."; examples = []; pre = ""; post = "" },
        fun _ stack ->
            match stack with
            | Any.Quote concepts :: Any.Network source :: tail ->
                let ids: Quote = 
                    List.fold (fun state concept -> 
                        let mutable state = state
                        match concept with
                        | Any.Element el -> 
                            Set.iter (fun triple ->
                                match triple with
                                | ElementPattern.Element e, ElementPattern.Element(Element ":"), Value.Element concept -> 
                                    if el = concept then
                                        state <- Any.Element e :: state
                                | _ -> ()) source
                            state
                        | _ -> failwith "TODO") List.empty concepts
                let json = extractJson ids source
                Ok(Any.Literal json :: tail)
            | _ -> failwith "TODO"
    )

let inferAction: Action =
    Action.Full(
        { doc = "..."; examples = []; pre = ""; post = "" },
        fun _ stack ->
            match stack with
            | description :: network :: tail ->
                let description =
                    match description with
                    | Any.Network n -> n
                    | _ -> failwith "TODO"

                let network =
                    match network with
                    | Any.Network n -> n
                    | _ -> failwith "TODO"

                match infer description network with
                | Ok res -> Ok(Any.Network res :: tail)
                | Error err -> error $"Error calling infer: {err}" None
            | _ -> error "Improper call to infer." None
    )
