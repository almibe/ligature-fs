// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

namespace LigaturePad;


extern alias LigatureInMemory;
extern alias Wander;
using _ = Wander::Ligature.Wander.Main;

using Avalonia.Interactivity;
using Avalonia.Controls;
using Avalonia;
using Avalonia.Controls.ApplicationLifetimes;
using Avalonia.Platform.Storage;
using System.IO;
using System;

public partial class MainWindow : Window
{
    private LigatureInMemory.Ligature.InMemory.LigatureInMemory instance = new();
    private string? sourcePath = null;

    public MainWindow()
    {
        InitializeComponent();
    }

    public void RunAction(object sender, RoutedEventArgs args)
    {
        ScriptText.Text ??= "";

        //TODO use instance prelude
        var res = _.printResult(_.run(ScriptText.Text, Wander.Ligature.Wander.Lib.Preludes.instancePrelude(this.instance)));
        ResultText.Text = res;
    }

    public void IntrospectAction(object sender, RoutedEventArgs args)
    {
        throw new NotSupportedException();
    }

    public void NewAction(object sender, RoutedEventArgs args)
    {
        instance = new();
        source.Text = "Source: None";
    }

    public async void OpenAction(object sender, RoutedEventArgs args)
    {
        var files = await this.StorageProvider.OpenFilePickerAsync(new FilePickerOpenOptions
        {
            Title = "Open Ligature File",
            AllowMultiple = false
        });

        if (files.Count == 1)
        {
            var file = files[0];
            source.Text = $"Source: {file.Path.AbsolutePath.ToString()}";
            sourcePath = file.Path.AbsolutePath.ToString();
            Wander.Ligature.Wander.Serialize.loadFromString(File.ReadAllLines(sourcePath), instance);
        }
    }

    public async void SaveAction(object sender, RoutedEventArgs args)
    {
        if (this.sourcePath != null)
        {
            var writer = new StreamWriter(this.sourcePath);
            Wander.Ligature.Wander.Serialize.write<object>(writer, instance);
            writer.Close();
        }
        else
        {
            var file = await this.StorageProvider.SaveFilePickerAsync(new FilePickerSaveOptions
            {
                Title = "Save Ligature File",
            });

            if (file != null)
            {
                var writer = new StreamWriter(file.Path.AbsolutePath.ToString());
                Wander.Ligature.Wander.Serialize.write<object>(writer, instance);
                writer.Close();
                this.sourcePath = file.Path.AbsolutePath.ToString();
                source.Text = $"Source: {file.Path.AbsolutePath.ToString()}";
            }
        }
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
