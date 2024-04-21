// This Source Code Form is subject to the terms of the Mozilla Public
// License, v. 2.0. If a copy of the MPL was not distributed with this
// file, You can obtain one at http://mozilla.org/MPL/2.0/.

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

            AnsiConsole.Write("Welcome to Wander's REPL!");
            AnsiConsole.MarkupLine("Type [yellow]:q[/] to quit.");
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
                else if (source.Equals("clear", StringComparison.OrdinalIgnoreCase))
                {
                    AnsiConsole.Console.Clear(true);
                    continue;
                }

                Evaluate(source);
            }
        }

        private static void Evaluate(string source)
        {
            AnsiConsole.WriteLine("Evaluating - " + source);
        }
    }
}
