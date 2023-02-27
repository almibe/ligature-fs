// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

namespace LigatureDesktop

open Ligature.Sqlite.Main

module Views =
    open Ligature
    open Avalonia.FuncUI
    open Avalonia.Controls
    open Avalonia.FuncUI.DSL
    open Avalonia.Layout
    open LigatureDesktop.Model
    
    let reportError (error: LigatureError) =
        printfn $"{error.userMessage}"

    let refreshDatasets (state: IWritable<UIModel>) =
        let datasets = state.Current.Ligature.AllDatasets ()
        match datasets with
        | Ok(datasets) ->
            let datasets = List.map (fun d -> readDataset d) datasets
            state.Set { state.Current with Datasets = datasets }
        | Error(err) -> reportError err

    let addDatasetForm (state: IWritable<UIModel>) =
        let addDataset () =
            match dataset state.Current.NewDataset with
            | Ok(dataset) ->
                match state.Current.Ligature.CreateDataset dataset with
                | Ok () -> refreshDatasets state
                | Error(err) -> reportError err
            | Error(err) -> reportError err
    
        StackPanel.create [
            StackPanel.children [
                TextBlock.create [
                    TextBlock.text "Dataset Name:"
                ]
                TextBox.create [
                    TextBox.onTextChanged (fun newDataset -> state.Set { state.Current with NewDataset = newDataset })
                ]
                Button.create [
                    Button.content "Add"
                    Button.onClick (fun _ -> addDataset ())
                ]
            ]
        ]

    let removeDatasetForm (state: IWritable<UIModel>) =
        let removeDataset () =
            let datasetName = state.Current.SelectedDataset
            printfn "remove"

        StackPanel.create [
        StackPanel.children [
            TextBlock.create [
                TextBlock.text "Remove?"
            ]
            Button.create [
                Button.content "Yes"
                Button.onClick (fun _ -> removeDataset ())
            ]
        ]
    ]

    let datasets (state: IWritable<UIModel>) =
        StackPanel.create [
            StackPanel.orientation Orientation.Horizontal
            StackPanel.children [
                TextBlock.create [
                    TextBlock.verticalAlignment VerticalAlignment.Center
                    TextBlock.margin (10,0,0,0)
                    TextBlock.text "Dataset:"
                ]
                ComboBox.create [
                    ComboBox.verticalAlignment VerticalAlignment.Center
                    ComboBox.margin (5, 0, 10, 0)
                    ComboBox.dataItems state.Current.Datasets //[ "No Datasets" ]
                    ComboBox.selectedIndex 0
                ]
                Button.create [
                    Button.verticalAlignment VerticalAlignment.Center
                    Button.margin (10, 0, 10, 0)
                    Button.content "Add Dataset"
                    Button.flyout (
                        Flyout.create [
                            Flyout.placement FlyoutPlacementMode.Bottom
                            Flyout.content (addDatasetForm state)
                        ]
                    )
                ]
                Button.create [
                    Button.verticalAlignment VerticalAlignment.Center
                    Button.margin (10, 0, 10, 0)
                    Button.content "Remove Dataset"
                    Button.flyout (
                        Flyout.create [
                            Flyout.placement FlyoutPlacementMode.Bottom
                            Flyout.content (removeDatasetForm state)
                        ]
                    )
                ]
                Button.create [
                    Button.verticalAlignment VerticalAlignment.Center
                    Button.margin (10, 0, 10, 0)
                    Button.content "Refresh Datasets"
                    Button.onClick (fun _ -> refreshDatasets state)
                ]
            ]
        ]

    let actions (state: IWritable<UIModel>) =
        StackPanel.create [
            StackPanel.orientation Orientation.Horizontal
            StackPanel.children [
                TextBlock.create [
                    TextBlock.verticalAlignment VerticalAlignment.Center
                    TextBlock.margin (10, 0, 0, 0)
                    TextBlock.text "Action:"
                ]
                ComboBox.create [
                    ComboBox.verticalAlignment VerticalAlignment.Center
                    ComboBox.margin (5, 0, 10, 0)
                    ComboBox.dataItems [
                        "Query"
                        "Add Statements"
                        "Remove Statements"
                    ]
                    ComboBox.selectedIndex 0
                ]
                Button.create [
                    Button.verticalAlignment VerticalAlignment.Center
                    Button.margin (10, 0, 10, 0)
                    Button.content "Run"
                    //Button.onClick (fun _ -> refreshDatasets ())
                ]
                Button.create [
                    Button.verticalAlignment VerticalAlignment.Center
                    Button.margin (10, 0, 10, 0)
                    Button.content "Clear"
                    Button.onClick (fun _ -> state.Set { state.Current with Output = "" })
                ]
            ]
        ]

    let topControls (state: IWritable<UIModel>) =
        StackPanel.create [
            StackPanel.orientation Orientation.Vertical
            DockPanel.dock Dock.Top
            StackPanel.children [
                datasets state
                actions state
            ]
        ]

    let editor (state: IWritable<UIModel>) =
        Grid.create [
            Grid.rowDefinitions "*, 1, *"
            Grid.children [
                TextBox.create [
                    Grid.row 0
                    TextBox.acceptsReturn true
                    TextBox.classes [ "code" ]
                ]
                GridSplitter.create [
                    Grid.row 1
                    GridSplitter.background "Gray"
                ]
                TextBox.create [
                    Grid.row 2
                    TextBox.acceptsReturn true
                    TextBox.classes [ "code" ]
                    TextBox.isReadOnly true
                    TextBox.text state.Current.Output
                ]
            ]
        ]

    let main () =
        let model = createModel ()
        Component (fun (ctx: IComponentContext) ->
            let state = ctx.useState (model)
            
            DockPanel.create [
                DockPanel.children [
                    topControls state
                    editor state
                ]
            ]
        )
