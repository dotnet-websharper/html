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

namespace WebSharper.Html.Server.Web

#if NET461

open WebSharper
open WebSharper.Web
open WebSharper.Html.Server

[<AbstractClass>]
type Control() as this =
    inherit System.Web.UI.Control()

    // Necessary so that .Element is only called once
    // (instead of twice, once by OnLoad and once by Render)
    let elt = lazy (this.Element :> INode)

    abstract member Element : Html.Element

    override this.OnLoad _ =
        ScriptManager.Find(base.Page)
            .Register(None, elt.Value, Shared.Metadata, Shared.Json)
        |> ignore

    override this.Render writer =
        let ctx = Remoting.GetContext()
        elt.Value.Write(ctx,
            new WebSharper.Core.Resources.HtmlTextWriter(writer.InnerWriter))

#endif
