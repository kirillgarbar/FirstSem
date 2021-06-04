paket config add-token "https://nuget.pkg.github.com/AndreiZaycev/index.json" %NUGET_AUTH_TOKEN%

echo Restoring dotnet tools...
dotnet tool restore

dotnet fake build -t %*
