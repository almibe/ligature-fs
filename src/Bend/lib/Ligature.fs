// module private Instance = 
//     let datasetsFunction (instance: ILigature) = Model.WanderValue.NativeFunction (
//         new Model.NativeFunction(fun _ _ ->
//             match instance.AllDatasets() with
//             | Ok(datasets) ->
//                 let datasets = List.map (fun d -> Model.WanderValue.String(datasetName d)) datasets
//                 Ok(WanderValue.Tuple(datasets))
//             | Error(err) -> Error(err)))

//     let addDataset (instance: ILigature) = WanderValue.NativeFunction (
//         new Model.NativeFunction(fun args _ ->
//             match args.Head with
//             | Expression.Value(WanderValue.String(name)) -> 
//                 let dataset = Dataset(name)
//                 match instance.CreateDataset dataset with
//                 | Ok(_) -> Ok(WanderValue.Nothing)
//                 | Error(err) -> Error(err)
//             | _ -> error "Could not add Dataset" None))

//     let removeDataset (instance: ILigature) = WanderValue.NativeFunction (
//         new Model.NativeFunction(fun args _ ->
//             match args.Head with
//             | Expression.Value(WanderValue.String(name)) -> 
//                 let dataset = Dataset(name)
//                 match instance.RemoveDataset dataset with
//                 | Ok(_) -> Ok(WanderValue.Nothing)
//                 | Error(err) -> Error(err)
//             | _ -> error "Could not remove Dataset" None))

//     let datasetExists (instance: ILigature) = WanderValue.NativeFunction (
//         new Model.NativeFunction(fun args _ ->
//             match args.Head with
//             | Expression.Value(WanderValue.String(name)) -> 
//                 let dataset = Dataset(name)
//                 match instance.DatasetExists dataset with
//                 | Ok(result) -> Ok(WanderValue.Boolean(result))
//                 | Error(err) -> Error(err)
//             | _ -> error "Could not check for Dataset" None))

//     let valueToWanderValue (value: Value): WanderValue =
//         match value with
//         | Identifier i -> WanderValue.Identifier i
//         | Integer i -> WanderValue.Integer i
//         | String s -> WanderValue.String s

//     let rec statementsToTuple (statements: Statement list) (results: Tuple<WanderValue>): Tuple<WanderValue> =
//         if List.isEmpty statements then
//             results
//         else
//             let statement = statements.Head
//             let entity = WanderValue.Identifier(statement.Entity)
//             let attribute = WanderValue.Identifier(statement.Attribute)
//             let value = valueToWanderValue statement.Value
//             statementsToTuple (statements.Tail) (List.append results [WanderValue.Tuple[entity; attribute; value]])

//     let allStatements (instance: ILigature) = WanderValue.NativeFunction (
//         new Model.NativeFunction(fun args _ ->
//             let datasetName = args.Head
//             match datasetName with
//             | Expression.Value(WanderValue.String(name)) ->
//                 let dataset = Dataset name
//                 instance.Query dataset (fun tx ->
//                     match tx.AllStatements () with
//                     | Ok(statements) ->
//                         Ok(WanderValue.Tuple(statementsToTuple statements []))
//                     | Error(err) -> Error(err)
//                 )
//             | _ -> error "Improper arguments could not run allStatements." None
//         ))

//     let matchStatements (query: IQueryTx) = WanderValue.NativeFunction (
//         new Model.NativeFunction(fun args bindings ->
//             error "todo - inside match" None
//         )
//     )

//     let query (instance: ILigature) = WanderValue.NativeFunction (
//         new Model.NativeFunction(fun args bindings ->
//             let datasetName = args.Head
//             let queryLambda = args.Tail.Head
//             match (datasetName, queryLambda) with
//             | (Expression.Value(WanderValue.String(name)), (Expression.Value(WanderValue.Lambda(_parameters, body)))) ->
//                 let dataset = Dataset(name)
//                 let res = instance.Query dataset (fun tx ->
//                     let bindings' = Bindings.bind "match" (matchStatements tx) bindings
                                        
//                     error "todo - inside query" None)
//                 res
//             | _ -> error "Improper arguments could not run query." None
//         ))

//     /// A NativeFunction that does a single match against a given Dataset.
//     /// Internally it starts a query transaction and then runs a single function in the tx.
//     let matchCommand (instance: ILigature) = WanderValue.NativeFunction (
//         new Model.NativeFunction(fun args bindings ->
//             let datasetName = args.Head
//             let entity = args.Tail.Head
//             let attribute = args.Tail.Tail.Head
//             let value = args.Tail.Tail.Tail.Head
//             match (datasetName, entity, attribute, value) with
//             | (Expression.Value(WanderValue.String(name)), 
//                 Expression.Value(entity), 
//                 Expression.Value(attribute),
//                 Expression.Value(value)) ->
//                     let dataset = Dataset(name)
//                     let entity =
//                         match entity with
//                         | WanderValue.Identifier(i) -> Ok (Some i)
//                         | WanderValue.Nothing -> Ok None
//                         | _ -> error "Invalid Entity passed to match." None
//                     let attribute =
//                         match attribute with
//                         | WanderValue.Identifier(i) -> Ok (Some i)
//                         | WanderValue.Nothing -> Ok None
//                         | _ -> error "Invalid Attribute passed to match." None
//                     let value = Ok None
//                     match (entity, attribute, value) with
//                     | (Ok(entity), Ok(attribute), Ok(value)) ->
//                         instance.Query dataset (fun tx ->
//                             match tx.MatchStatements entity attribute value with
//                             | Ok(results) -> Ok(WanderValue.Tuple(statementsToTuple results []))
//                             | Error(err) -> Error(err)
//                         )
//                     | _ -> error "Could not call match." None //TODO should return actual error
//             | _ -> error "Improper arguments passed to match." None
//         )
//     )

//     /// A Native Function that write Statements to a Dataset.
//     /// Example: addStatements("dataset" (<a> <b> <c>)(<a> <b> "Test") (<a> <b> 432))
//     let write (instance: ILigature) = WanderValue.NativeFunction (
//         new Model.NativeFunction(fun args _ ->
//             let datasetName = args.Head
//             let statements = args.Tail.Head
//             match (datasetName, statements) with
//             | (Expression.Value(WanderValue.String(name)), Expression.Value(WanderValue.Tuple(statements))) ->
//                 let dataset = Dataset(name)
//                 let writeRes = instance.Write dataset (fun tx ->
//                     let rec addStatement (statements: Tuple<WanderValue>) =
//                         if not (List.isEmpty statements) then
//                             let statement = statements.Head
//                             // match statements.Head with
//                             // | Tuple(statement) ->
//                             match statement with
//                             | WanderValue.Tuple(statement) ->
//                                 let entity = statement.Head
//                                 let attribute = statement.Tail.Head
//                                 let value = statement.Tail.Tail.Head
//                                 match (entity, attribute, value) with
//                                 | (WanderValue.Identifier(entity), WanderValue.Identifier(attribute), WanderValue.Identifier(value)) ->
//                                     match tx.AddStatement (Ligature.statement entity attribute (Value.Identifier(value))) with
//                                     | Ok _ -> addStatement statements.Tail
//                                     | Error err -> Error err
//                                 | _ -> error "Invalid Statement contents." None
//                             | _ -> error "Error Statements must be expressed as Tuples." None
//                         else
//                             Ok ()
//                     addStatement statements)
//                 match writeRes with
//                 | Ok _ -> Ok WanderValue.Nothing
//                 | Error err -> Error err
//             | _ -> error "Improper call to addStatements." None
//         ))
