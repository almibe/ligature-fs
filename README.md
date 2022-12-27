# ligature-fs
An experimental F# based implementation of Ligature.
This is still very early in development and not ready for any sort of use yet.
Keep in mind that the docs below might not be up to date.
See https://ligature.dev for more information on Ligature.

## Use
This project assumes you have .NET Core setup.
See https://dotnet.microsoft.com/en-us/download to download the SDK.
Version 6.x is required (7.x won't work currently since this project uses Fable 3).
Currently to run the application simply run

```
cd src/LigatureLab
dotnet run
```

in the root directory of this project and open http://localhost:5000.

## Projects

This repo is made up of a couple of different projects, below is a description of each.

| Project       | Description                                              |
| ------------- | -------------------------------------------------------- |
| Ligature      | Main code base including the main logic and types.       |
| Ligature.Test | Test code for Ligature.                                  |
| LigatureLab   | A server for working with Ligature.                      |
| LigatureLabJS | Front-end JS code (created using Fable) for LigatureLab. |
