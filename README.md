# ligature-fs
An experimental F# based implementation of Ligature.
This is still very early in development and not ready for any sort of use yet.
Keep in mind that the docs below might not be up to date.
See https://ligature.dev for more information on Ligature.

## Use
This project assumes you have .NET Core and npm setup.

See https://nodejs.org/en/download/ to download node/npm, the current LTS version is fine.

See https://dotnet.microsoft.com/en-us/download to download the SDK for .NET Core.

To run the application simply run

```
cd src/LigatureLab
dotnet run
```

in the root directory of this project and open http://localhost:5000.

## Projects

This repo is made up of a couple of different projects, below is a description of each.

| Project           | Description                                                        |
| ----------------- | ------------------------------------------------------------------ |
| build             | 
| Gaze              | A parsing library.                                                 |
| Lig               | Support for the Lig serialization language.                        |
| Ligature          | Main code base including the main logic and types.                 |
| LigatureInMemory  | An implementation of Ligature that uses in-memory data structures. |
| LigatureLab       | A web application for working with Ligature.                       |
| LigatureLMDB      | An implementation of Ligature that uses LMDB for storage.          |
| LigatureSqlite    | An implementation of Ligature that uses SQLite3 for storage.       |
| LigatureTestSuite | A common test suite for Ligature implementations.                  |
| Wander            | The Wander scripting language.                                     |

## Setup

After checking out this project run.

`dotnet tool restore`

This is will setup tools used by this project.

## Formatting

Example to run the formatter

`dotnet fantomas ./src/Ligature/`