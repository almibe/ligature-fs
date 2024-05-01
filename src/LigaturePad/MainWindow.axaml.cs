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

public partial class MainWindow : Window
{
    private LigatureInMemory.Ligature.InMemory.Main.LigatureInMemory instance = new();
    private string? sourcePath = null;

    public MainWindow()
    {
        InitializeComponent();
    }

    public void RunAction(object sender, RoutedEventArgs args)
    {
        ScriptText.Text ??= "";
        var res = _.printResult(_.run(ScriptText.Text, Wander.Ligature.Wander.Lib.Preludes.instancePrelude(instance)));
        ResultText.Text = res;
    }

    public void IntrospectAction(object sender, RoutedEventArgs args)
    {
        ScriptText.Text ??= "";
        var res = _.introspect(ScriptText.Text);
        ResultText.Text = res.ToString();
    }

    public void NewAction(object sender, RoutedEventArgs args)
    {
        instance = new();
        source.Text = "Source: None";
    }

    public async void OpenAction(object sender, RoutedEventArgs args)
    {
        var files = await StorageProvider.OpenFilePickerAsync(new FilePickerOpenOptions
        {
            Title = "Open Ligature File",
            AllowMultiple = false
        });

        if (files.Count == 1)
        {
            var file = files[0];
            source.Text = $"Source: {file.Path.AbsolutePath}";
            sourcePath = file.Path.AbsolutePath.ToString();
            Wander.Ligature.Wander.Serialize.loadFromString(File.ReadAllLines(sourcePath), instance);
        }
    }

    public async void SaveAction(object sender, RoutedEventArgs args)
    {
        if (sourcePath != null)
        {
            var writer = new StreamWriter(sourcePath);
            Wander.Ligature.Wander.Serialize.write(writer, instance);
            writer.Close();
        }
        else
        {
            var file = await StorageProvider.SaveFilePickerAsync(new FilePickerSaveOptions
            {
                Title = "Save Ligature File",
            });

            if (file != null)
            {
                var writer = new StreamWriter(file.Path.AbsolutePath.ToString());
                Wander.Ligature.Wander.Serialize.write(writer, instance);
                writer.Close();
                sourcePath = file.Path.AbsolutePath.ToString();
                source.Text = $"Source: {file.Path.AbsolutePath}";
            }
        }
    }

    public async void SaveAsAction(object sender, RoutedEventArgs args)
    {
            var file = await StorageProvider.SaveFilePickerAsync(new FilePickerSaveOptions
            {
                Title = "Save Ligature File",
            });

            if (file != null)
            {
                var writer = new StreamWriter(file.Path.AbsolutePath.ToString());
                Wander.Ligature.Wander.Serialize.write(writer, instance);
                writer.Close();
                sourcePath = file.Path.AbsolutePath.ToString();
                source.Text = $"Source: {file.Path.AbsolutePath}";
            }
    }
    public void ExitAction(object sender, RoutedEventArgs args)
    {
        if (Application.Current?.ApplicationLifetime is IClassicDesktopStyleApplicationLifetime desktopApp)
        {
            desktopApp.Shutdown();
        }
    }
}
