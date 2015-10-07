namespace WebSharper.Html.Tests

open WebSharper
open WebSharper.Sitelets

[<JavaScript>]
module Client =
    open WebSharper.Html.Client
    open WebSharper.JavaScript

    let Elt (x: string) =
        Button [Text x]
        |>! OnClick (fun _ _ -> JS.Alert "Clicked!")

module Site =
    open WebSharper.Html.Server

    type T = { title: string; body: seq<Element> }

    let Template =
        Content.Template<T>("~/Main.html")
            .With("title", fun t -> t.title)
            .With("body", fun t -> t.body)

    let Body (ctx: Context<_>) =
        [
            (try
                Div (Content.Template<unit>("~/fragment.html").Run((), root = ctx.RootFolder))
             with e -> Div [Text (sprintf "%A" e)])
            Div [
                Text "TODO" :> Web.INode
                Span [] :> _
                ClientSide <@ Client.Elt "test" @> :> _
            ]
        ]

    [<Website>]
    let Main =
        Application.SinglePage (fun ctx ->
            Content.WithTemplate Template { title = "WebSharper.Html Tests"; body = Body ctx }
//            Content.Page(Title = "WebSharper.Html Tests", Body = Body)
        )
