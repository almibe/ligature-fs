﻿// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

namespace LigaturePad

open Avalonia
open Avalonia.Controls.ApplicationLifetimes
open Avalonia.Themes.Fluent
open Avalonia.FuncUI.Hosts
open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.Layout

module Main =
    open Ligature.LigatureStore.InMemoryStore
    open Ligature.Wander.Combinators
    open Ligature.Wander.Main
    let view () =
        Component(fun ctx ->
            let result = ctx.useState ""
            let script = ctx.useState ""

            DockPanel.create [
                DockPanel.children [
                    StackPanel.create [
                        StackPanel.dock Dock.Top
                        StackPanel.orientation Orientation.Horizontal
                        StackPanel.children [
                            Button.create [
                                Button.content "Run"
                                Button.onClick (fun _ ->
                                    match run stdCombinators emptyInMemoryStore (script.Current) with
                                    | Ok(Some(res)) -> result.Set $"{res}"
                                    | Ok _ -> result.Set ("--nothing--")
                                    | Error(err) -> result.Set (err.UserMessage))
                            ]
                        ]
                    ]
                    Grid.create [
                        Grid.dock Dock.Bottom
                        Grid.columnDefinitions "1*"
                        Grid.rowDefinitions "1*, 1*"
                        Grid.children [
                            TextBox.create [
                                TextBox.name "script"
                                Grid.row 0
                                Grid.column 0
                                TextBox.acceptsReturn true
                                TextBox.onTextChanged (fun e -> script.Set e)
                            ]
                            TextBox.create [
                                TextBox.name "result"
                                TextBox.text (result.Current)
                                Grid.row 1
                                Grid.column 0
                                TextBox.acceptsReturn true
                                TextBox.isEnabled false
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
