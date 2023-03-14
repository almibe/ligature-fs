echo Restoring dotnet tools...
dotnet tool restore

dotnet run --project ./src/build/build.fsproj -- -t %*
