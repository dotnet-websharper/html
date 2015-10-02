#load "tools/includes.fsx"
open IntelliFactory.Build

let bt =
    BuildTool().PackageId("WebSharper.Html")
        .VersionFrom("WebSharper")
        .WithFSharpVersion(FSharpVersion.FSharp31)
        .WithFramework(fun f -> f.Net40)

let client =
    bt.WebSharper.Library("WebSharper.Html.Client")
        .SourcesFromProject()
        .Embed([])
        .References(fun r -> [])

let server =
    bt.WebSharper.Library("WebSharper.Html.Server")
        .SourcesFromProject()
        .Embed([])
        .References(fun r ->
            [
                r.Assembly("System.Xml")
                r.Assembly("System.Xml.Linq")
            ])

let tests =
    bt.WebSharper.SiteletWebsite("WebSharper.Html.Tests")
        .SourcesFromProject()
        .Embed([])
        .References(fun r ->
            [
                r.Project(client)
                r.Project(server)
            ])

bt.Solution [
    server
    client
    tests

    bt.NuGet.CreatePackage()
        .Configure(fun c ->
            { c with
                Title = Some "WebSharper.Html"
                LicenseUrl = Some "http://websharper.com/licensing"
                ProjectUrl = Some "https://github.com/intellifactory/https://github.com/intellifactory/websharper.html"
                Description = "Client-side and server-side HTML Combinators for WebSharper (legacy)"
                RequiresLicenseAcceptance = true })
        .Add(client)
        .Add(server)
]
|> bt.Dispatch
