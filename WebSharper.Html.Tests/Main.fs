// $begin{copyright}
//
// This file is part of WebSharper
//
// Copyright (c) 2008-2018 IntelliFactory
//
// Licensed under the Apache License, Version 2.0 (the "License"); you
// may not use this file except in compliance with the License.  You may
// obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or
// implied.  See the License for the specific language governing
// permissions and limitations under the License.
//
// $end{copyright}
namespace WebSharper.Html.Tests

open WebSharper
open WebSharper.Sitelets

[<JavaScript>]
module Client =
    open WebSharper.Html.Client
    open WebSharper.JavaScript

    let Elt (x: string) =
        let i = Input []
        Div [
            i
            Button [Text x]
            |>! OnClick (fun _ _ -> JS.Alert ("Clicked! Text: " + i.Value))
        ]

module Site =
    open WebSharper.Html.Server

#if NET461
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

#else

    [<Website>]
    let Main =
        Application.SinglePage (fun ctx ->
            Content.Page(
                Title = "WebSharper.Html Tests",
                Body =
                    [
                        Div [
                            Text "TODO" :> Web.INode
                            Span [] :> _
                            ClientSide <@ Client.Elt "test" @> :> _
                        ]
                    ]
            )
        )

#endif