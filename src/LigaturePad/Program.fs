// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

namespace LigaturePad

open Ligature.Bend.Main
open Ligature.Bend.Bindings
open Ligature.Bend.Lib.Preludes
open Ligature.InMemory
open Avalonia
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Themes.Fluent
open Avalonia.FuncUI.Hosts
open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.Layout

module Main =
    type EditorState = {
        EditorText: string;
        ResultText: string
    }

    let runScript script = 
        run script (instancePrelude (new LigatureInMemory ()))

    let view () =
        Component(fun ctx ->
            let state = ctx.useState ({ EditorText = ""; ResultText = "" })

            DockPanel.create [
                DockPanel.children [
                    StackPanel.create [
                        StackPanel.dock Dock.Top
                        StackPanel.orientation Orientation.Horizontal
                        StackPanel.children [
                            Button.create [
                                Button.onClick (fun _ ->
                                    let editorText = state.Current.EditorText
                                    let res = printResult (runScript editorText)
                                    state.Set({ ResultText = res; EditorText = editorText})) //state.Current.EditorText }))
                                Button.content "Run"
                                Button.horizontalAlignment HorizontalAlignment.Stretch
                                Button.horizontalContentAlignment HorizontalAlignment.Center
                            ]
                            Button.create [
                                Button.onClick (fun _ ->
                                    let editorText = state.Current.EditorText
                                    let res = string (introspect editorText)
                                    state.Set({ ResultText = res; EditorText = editorText})) //state.Current.EditorText }))
                                Button.content "Intro"
                                Button.horizontalAlignment HorizontalAlignment.Stretch
                                Button.horizontalContentAlignment HorizontalAlignment.Center
                            ]
                        ]
                    ]
                    Grid.create [
                        Grid.columnDefinitions "1*"
                        Grid.rowDefinitions "1*, 1, 1*"
                        Grid.children [
                            TextBox.create [
                                Grid.row 0
                                Grid.column 0
                                TextBox.dock Dock.Bottom
                                TextBox.fontFamily "Cascadia Mono, Courier New"
                                TextBox.acceptsReturn true
                                TextBox.textWrapping Media.TextWrapping.Wrap
                                TextBox.text ""
                                TextBox.onTextChanged (
                                    fun script ->
                                        state.Set { state.Current with EditorText = script }
                                    )
                            ]
                            GridSplitter.create [
                                Grid.row 1
                            ]
                            TextBox.create [
                                Grid.row 2
                                Grid.column 0
                                TextBox.dock Dock.Bottom
                                TextBox.fontFamily "Cascadia Mono, Courier New"
                                TextBox.acceptsReturn true
                                TextBox.textWrapping Media.TextWrapping.Wrap
                                TextBox.text (string (state.Current.ResultText))
                                TextBox.isReadOnly true
                            ]
                        ]
                    ]
                ]
            ]
        )

type MainWindow() =
    inherit HostWindow()
    do
        base.Title <- "LigaturePad"
        base.Content <- Main.view ()

type App() =
    inherit Application()

    override this.Initialize() =
        this.Styles.Add (FluentTheme())
        this.RequestedThemeVariant <- Styling.ThemeVariant.Dark

    override this.OnFrameworkInitializationCompleted() =
        match this.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktopLifetime ->
            desktopLifetime.MainWindow <- MainWindow()
        | _ -> ()

module Program =
    [<EntryPoint>]
    let main(args: string[]) =
        AppBuilder
            .Configure<App>()
            .UsePlatformDetect()
            .UseSkia()
            .StartWithClassicDesktopLifetime(args)
