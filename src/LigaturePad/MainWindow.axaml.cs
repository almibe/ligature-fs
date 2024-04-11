using Avalonia.Controls;

namespace LigaturePad;

using Avalonia.Interactivity;

public partial class MainWindow : Window
{
    public MainWindow()
    {
        InitializeComponent();
    }

    public void RunAction(object sender, RoutedEventArgs args)
    {
        // let editorText = state.Current.EditorText
        // let res = printResult (runScript editorText)
        // state.Set({ ResultText = res; EditorText = editorText})) //state.Current.EditorText }))

        source.Text = "Source: None";
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
        // let editorText = state.Current.EditorText
        // let res = string (introspect editorText)
        // state.Set({ ResultText = res; EditorText = editorText})) //state.Current.EditorText }))
        source.Text = "Source: None";
    }
}
