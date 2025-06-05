using Photino.NET;
using System.Drawing;

namespace LigatureDesktop
{
    //NOTE: To hide the console window, go to the project properties and change the Output Type to Windows Application.
    // Or edit the .csproj file and change the <OutputType> tag from "WinExe" to "Exe".
    class Program
    {
        [STAThread]
        static void Main(string[] args)
        {
            string windowTitle = "Ligature Desktop";

            var window = new PhotinoWindow()
                .SetTitle(windowTitle)
                .SetUseOsDefaultSize(false)
                .SetSize(new Size(1024, 800))
                .Center()
                .RegisterWebMessageReceivedHandler((object sender, string message) =>
                {
                    var window = (PhotinoWindow)sender;
                    string response = $"Received message: \"{message}\"";
                    window.SendWebMessage(response);
                })
                .Load(new Uri("localhost:5173"));

            window.WaitForClose();
        }
    }
}
