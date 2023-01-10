// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

namespace LigatureDesktop

module Datasets =
    open Avalonia.FuncUI
    open Avalonia.Controls
    open Avalonia.FuncUI.DSL
    open Avalonia.Layout
    
    let view =
        Component.create("Datasets",fun ctx ->
            let state = ctx.useState 0
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
                                ComboBox.dataItems [ "No Datasets" ]
                                ComboBox.selectedIndex 0
                            ]
                            Button.create [
                                Button.verticalAlignment VerticalAlignment.Center
                                Button.margin (10, 0, 10, 0)
                                Button.content "Add Dataset"
                                //Button.click call RefreshDataset
                            ]
                            Button.create [
                                Button.verticalAlignment VerticalAlignment.Center
                                Button.margin (10, 0, 10, 0)
                                Button.content "Remove Dataset"
                                //Button.click call RefreshDataset
                            ]
                            Button.create [
                                Button.verticalAlignment VerticalAlignment.Center
                                Button.margin (10, 0, 10, 0)
                                Button.content "Refresh Datasets"
                                //Button.onClick (fun _ -> refreshDatasets ())
                            ]
                        ]
                    ]
                    //
                    // <StackPanel Orientation="Horizontal" DockPanel.Dock="Bottom">
                    //     <TextBlock VerticalAlignment="Center" Margin="10,0,0,0">Action:</TextBlock>
                    //     <ComboBox VerticalAlignment="Center" Margin="5,0,10,0" SelectedIndex="0">
                    //         <ComboBoxItem>Query</ComboBoxItem>
                    //         <ComboBoxItem>Add Statements</ComboBoxItem>
                    //         <ComboBoxItem>Remove Statements</ComboBoxItem>
                    //     </ComboBox>
                    //     <Button VerticalAlignment="Center" Margin="10,0,10,0" Click="RunClick">Run</Button>
                    //     <Button VerticalAlignment="Center" Margin="10,0,10,0" Click="ClearClick">Clear</Button>
                    // </StackPanel>
                    // <Grid RowDefinitions="*, 1, *">
                    //     <TextBox Grid.Row="0" AcceptsReturn="True" TextWrapping="Wrap" Classes="code"></TextBox>
                    //     <GridSplitter Grid.Row="1" Background="Gray" ResizeDirection="Rows"/>
                    //     <TextBox Grid.Row="2" AcceptsReturn="True" TextWrapping="Wrap" Classes="code" IsReadOnly="true"></TextBox>
                    // </Grid>
                ]
            ]
        )
