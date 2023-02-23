// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

namespace LigatureDesktop

open Ligature.InMemory

module Datasets =
    open Ligature
    open Avalonia.FuncUI
    open Avalonia.Controls
    open Avalonia.FuncUI.DSL
    open Avalonia.Layout
    
    type UIModel = {
        Ligature: Ligature
        Datasets: string list
        SelectedDataset: string
        Output: string
        NewDataset: string
    }

    let createModel () = { 
        Datasets = []
        Output = ""
        Ligature = new LigatureInMemory() :> Ligature
        NewDataset = ""
        SelectedDataset = ""
        }

    let view =
        Component.create("Datasets",fun (ctx: IComponentContext) ->
            let state = ctx.useState (createModel ())

            let addDataset () =
                let datasetName = state.Current.NewDataset
                let x = datasetName :: state.Current.Datasets
                printfn "%A" x
                state.Set { state.Current with Datasets = datasetName :: state.Current.Datasets }
                printfn "test"

            DockPanel.create [
                DockPanel.children [
                    StackPanel.create [
                        StackPanel.dock Dock.Top
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
                                        Flyout.content (
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
                                                        Button.onClick (fun x -> addDataset ())
                                                    ]
                                                ]
                                            ]
                                        )
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
                                        Flyout.content (
                                            TextBlock.create [
                                                TextBlock.text "Hi, I am flyout"
                                            ]
                                        )
                                    ]
                                )
                            ]
                            Button.create [
                                Button.verticalAlignment VerticalAlignment.Center
                                Button.margin (10, 0, 10, 0)
                                Button.content "Refresh Datasets"
                                //Button.onClick (fun _ -> refreshDatasets ())
                            ]
                        ]
                    ]
                    StackPanel.create [
                        StackPanel.orientation Orientation.Horizontal
                        DockPanel.dock Dock.Bottom
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
                ]
            ]
        )
