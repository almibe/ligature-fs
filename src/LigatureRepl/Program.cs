using System;
using System.Collections.Generic;
using System.IO;
using System.Threading;
using System.Threading.Tasks;
using Spectre.Console;

namespace RadLine.Examples
{
    public static class Program
    {
        public static async Task Main()
        {
            Console.WriteLine("Welcome to Ligature's REPL!\nEnter :q to quit and :c to clear.");

            var editor = new LineEditor()
            {
                MultiLine = true,
                Prompt = new LineEditorPrompt(">", "."),
            };

            while (true)
            {
                AnsiConsole.WriteLine();
                var source = await editor.ReadLine(CancellationToken.None);
                if (source.Equals(":q", StringComparison.OrdinalIgnoreCase))
                {
                    break;
                }
                else if (source.Equals(":c", StringComparison.OrdinalIgnoreCase))
                {
                    AnsiConsole.Console.Clear(true);
                    continue;
                }
                else
                {
                    editor.History.Add(source);
                    var res = Ligature.Wander.Main.printResult(Ligature.Wander.Main.run(source, Ligature.InMemoryNetwork.emptyNetwork));
                    Console.WriteLine(res);
                }
            }
        }
    }
}
