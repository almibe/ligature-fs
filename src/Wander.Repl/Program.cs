// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

extern alias LigatureInMemory;
extern alias Wander;
using _ = Wander::Ligature.Wander.Main;

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
            if (!LineEditor.IsSupported(AnsiConsole.Console))
            {
                AnsiConsole.MarkupLine("The terminal does not support ANSI codes, or it isn't a terminal.");
            }

            AnsiConsole.WriteLine("Welcome to Wander's REPL!");
            AnsiConsole.MarkupLine("Type [yellow]:q[/] to quit.");
            AnsiConsole.MarkupLine("Type [yellow]:c[/] to clear.");
            AnsiConsole.MarkupLine("Type [yellow]:i EXPRESSION[/] to introspect an Expression.");

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
                else if (source.StartsWith(":i")) {
                    source = source.Substring(2);
                    Introspect(source);
                }
                else {
                    Evaluate(source);
                }
            }
        }

        private static LigatureInMemory.Ligature.InMemory.Main.LigatureInMemory instance =
            new LigatureInMemory.Ligature.InMemory.Main.LigatureInMemory();

        private static void Evaluate(string script)
        {
            var res = _.printResult(_.run(script, 
                Wander.Ligature.Wander.Lib.Preludes.instancePrelude(instance)));
            AnsiConsole.WriteLine(res);
        }

        private static void Introspect(string script)
        {
            var instance = new LigatureInMemory.Ligature.InMemory.Main.LigatureInMemory();
            var res = _.introspect(script.Trim());
            AnsiConsole.WriteLine(res.ToString());
        }
    }
}
