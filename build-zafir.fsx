#load "tools/includes.fsx"
open System.IO
open IntelliFactory.Build

let bt =
    BuildTool().PackageId("Zafir.Html")
        .VersionFrom("Zafir")
        .WithFSharpVersion(FSharpVersion.FSharp30)
        .WithFramework(fun f -> f.Net40)

let client =
    bt.Zafir.Library("WebSharper.Html.Client")
        .SourcesFromProject()
        .WithSourceMap()
        .Embed([])
        .References(fun r -> [])

let server =
    bt.Zafir.Library("WebSharper.Html.Server")
        .SourcesFromProject()
        .Embed([])
        .References(fun r ->
            [
                r.Assembly("System.Xml")
                r.Assembly("System.Xml.Linq")
                r.NuGet("IntelliFactory.Xml").ForceFoundVersion().Reference()
            ])

let tests =
    bt.Zafir.SiteletWebsite("WebSharper.Html.Tests")
        .SourcesFromProject()
        .WithSourceMap()
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
                Title = Some "Zafir.Html"
                LicenseUrl = Some "http://websharper.com/licensing"
                ProjectUrl = Some "https://github.com/intellifactory/https://github.com/intellifactory/websharper.html"
                Description = "Client-side and server-side HTML Combinators for WebSharper (legacy)"
                RequiresLicenseAcceptance = true })
        .Add(client)
        .Add(server)
]
|> bt.Dispatch
