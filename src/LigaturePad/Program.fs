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
    open Ligature.InMemoryEngine
    open Wander.Commands
    open Wander.Main
    open Wander.Lib
    open Wander.Model

    let view () =
        Component(fun ctx ->
            let result = ctx.useState ""
            let script = ctx.useState ""

            DockPanel.create
                [ DockPanel.children
                      [ StackPanel.create
                            [ StackPanel.dock Dock.Top
                              StackPanel.orientation Orientation.Horizontal
                              StackPanel.children
                                  [ Button.create
                                        [ Button.content "Run"
                                          Button.onClick (fun _ ->
                                              match run stdCommands (emptyVariables ()) (script.Current) with
                                              | Ok(Some(res)) -> result.Set $"{(Wander.Model.prettyPrint res)}"
                                              | Ok _ -> result.Set("--nothing--")
                                              | Error(err) -> result.Set(err.UserMessage)) ] ] ]
                        Grid.create
                            [ Grid.dock Dock.Bottom
                              Grid.columnDefinitions "1*"
                              Grid.rowDefinitions "1*, 1*"
                              Grid.children
                                  [ TextBox.create
                                        [ Grid.row 0
                                          Grid.column 0
                                          TextBox.acceptsReturn true
                                          TextBox.fontFamily "Source Code Pro"
                                          TextBox.onTextChanged (fun e -> script.Set e) ]
                                    TextBox.create
                                        [ TextBox.isReadOnly true
                                          TextBox.text (result.Current)
                                          TextBox.fontFamily "Source Code Pro"
                                          Grid.row 1
                                          Grid.column 0
                                          TextBox.acceptsReturn true ] ] ] ] ])

type MainWindow() =
    inherit HostWindow()

    do
        base.Title <- "LigaturePad"
        base.Content <- Main.view ()

type App() =
    inherit Application()

    override this.Initialize() =
        this.Styles.Add(FluentTheme())
        this.RequestedThemeVariant <- Styling.ThemeVariant.Dark

    override this.OnFrameworkInitializationCompleted() =
        match this.ApplicationLifetime with
        | :? IClassicDesktopStyleApplicationLifetime as desktopLifetime -> desktopLifetime.MainWindow <- MainWindow()
        | _ -> ()

module Program =

    [<EntryPoint>]
    let main (args: string[]) =
        AppBuilder
            .Configure<App>()
            .UsePlatformDetect()
            .UseSkia()
            .StartWithClassicDesktopLifetime(args)
