// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

namespace LigaturePad;

extern alias Bend;
using _ = Bend::Ligature.Bend.Main;

using Avalonia.Interactivity;
using Avalonia.Controls;
using Avalonia;
using Avalonia.Controls.ApplicationLifetimes;

public partial class MainWindow : Window
{
    public MainWindow()
    {
        InitializeComponent();
    }

    public void RunAction(object sender, RoutedEventArgs args)
    {
        //TODO use instance prelude
        var res = _.printResult(_.run(ScriptText.Text, Bend.Ligature.Bend.Lib.Preludes.standardPrelude()));
        ResultText.Text = res;
    }

    public void IntrospectAction(object sender, RoutedEventArgs args)
    {
        // let editorText = state.Current.EditorText
        // let res = string (introspect editorText)
        // state.Set({ ResultText = res; EditorText = editorText})) //state.Current.EditorText }))
        source.Text = "Source: None";
    }

    public void NewAction(object sender, RoutedEventArgs args)
    {
        // let editorText = state.Current.EditorText
        // let res = string (introspect editorText)
        // state.Set({ ResultText = res; EditorText = editorText})) //state.Current.EditorText }))
        source.Text = "Source: None";
    }
    public void OpenAction(object sender, RoutedEventArgs args)
    {
        // let editorText = state.Current.EditorText
        // let res = string (introspect editorText)
        // state.Set({ ResultText = res; EditorText = editorText})) //state.Current.EditorText }))
        source.Text = "Source: None";
    }
    public void SaveAction(object sender, RoutedEventArgs args)
    {
        // let editorText = state.Current.EditorText
        // let res = string (introspect editorText)
        // state.Set({ ResultText = res; EditorText = editorText})) //state.Current.EditorText }))
        source.Text = "Source: None";
    }
    public void SaveAsAction(object sender, RoutedEventArgs args)
    {
        // let editorText = state.Current.EditorText
        // let res = string (introspect editorText)
        // state.Set({ ResultText = res; EditorText = editorText})) //state.Current.EditorText }))
        source.Text = "Source: None";
    }
    public void ExitAction(object sender, RoutedEventArgs args)
    {
        if (Application.Current?.ApplicationLifetime is IClassicDesktopStyleApplicationLifetime desktopApp)
        {
            desktopApp.Shutdown();
        }
    }
}
