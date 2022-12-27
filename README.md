# ligature-fs
Experimental F# based implementation of Ligature.
Not ready for any sort of use yet and the docs below might not be up to date.
See ligature.dev for more information on Ligature.

## Use
This project assumes you have .NET Core setup.
Currently to run the application simply run

```
cd src/LigatureLab
dotnet run
```

in the root directory of this project.

## Projects

This repo is made up of a couple of different projects below is a description of each.

| Repo          | Description                                              |
| ------------- | -------------------------------------------------------- |
| Ligature      | Main code base including the main logic and types.       |
| Ligature.Test | Test code for Ligature.                                  |
| LigatureLab   | A server for working with Ligature.                      |
| LigatureLabJS | Front-end JS code (created using Fable) for LigatureLab. |
