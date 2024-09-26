# ligature-fs

An F# based implementation of Ligature.
This is still very early in development and not ready for any sort of use yet.
Keep in mind that the docs below might not be up to date.
See https://ligature.dev for more information on Ligature.

## Use

This project assumes you have .NET Core setup.

See https://dotnet.microsoft.com/en-us/download to download the SDK for .NET Core.

After installing .NET Core the following command should run the test suite for Ligature.

```
dotnet run --project src/Ligature.Test
```

## Projects

This repo is made up of a couple of different projects, below is a description of each.
Some of these projects have a sibling project like Ligature.Test that simply contains a
test suite for the other project.

| Project            | Description                                                        |
| ------------------ | ------------------------------------------------------------------ |
| build              | Fake build script.                                                 |
| Gaze               | A parsing library.                                                 |
| Ligature           | Main code base including the main logic and types.                 |
| LigaturePad        | A simple desktop app for Ligature.                                 |
| LigatureWebSockets | A backend of Ligature using Suave and WebSockets.                  |
| tiny-dl            | An F# library supporting Description Logic.                        |
| Wander             | The Wander scripting language.                                     |

## Setup

After checking out this project run.

`dotnet tool restore`

This is will setup tools used by this project.

## Formatting

Examples running the formatter

`dotnet fantomas ./src/Ligature/`

`dotnet fantomas ./ -r`

## Linting

Examples running the linter

`dotnet fsharplint lint ./src/Ligature/Ligature.fs`

## LigatureWebSockets

LigatureWebSockets is a project that allows running Wander code against an instance of Ligature via WebSockets.

### Running LigatureWebSockets

`dotnet run --project src/LigatureWebSockets`

See https://learn.microsoft.com/en-us/dotnet/core/deploying/ for more information.
