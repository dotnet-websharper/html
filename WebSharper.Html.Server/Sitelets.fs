namespace WebSharper.Sitelets

#nowarn "44" // Deprecated: Content.CustomContentAsync

open System.IO
open System.Web.UI
open WebSharper
open WebSharper.Html.Server

module Content =
    open System
    open System.Collections.Generic
    open System.Text.RegularExpressions
    open System.Web

    type private Func<'A,'B> = System.Func<'A,'B>
    type private Writer = HtmlTextWriter -> unit

    module H = WebSharper.Html.Server.Html
    module M = WebSharper.Core.Metadata
    module J = WebSharper.Core.Json
    module XS = IntelliFactory.Xml.SimpleXml
    module XT = IntelliFactory.Xml.Templating

    type HtmlElement = Html.Element

    type Hole<'T> =
        | SH of ('T -> Async<string>)
        | EH of ('T -> Async<seq<Web.INode>>)

    type Wrapper<'T> =
        {
            extra : IDictionary<string, XS.INode>
            value : 'T
        }

    [<Literal>]
    let SCRIPTS = "SCRIPTS"
    [<Literal>]
    let STYLES = "STYLES"
    [<Literal>]
    let META = "META"

    module Template =
        type LoadFrequency =
            | Once
            | PerRequest
            | WhenChanged

    [<Sealed>]
    type CustomXml private () =
        static let self = CustomXml()
        static member Instance = self
        interface XT.IXml<H.Element,Web.INode> with
            member this.Text x = H.TextContent x :> _
            member this.CData x = H.VerbatimContent x :> _
            member this.ElementNode x = x :> _
            member this.Element(name, attrs, children) =
                children
                |> Seq.append
                    (attrs
                    |> Seq.map (fun (KeyValue (k, v)) ->
                        H.NewAttr k.Local v :> Web.INode))
                |> H.NewTag name.Local

    /// Decides if an attribute should contain a URL by HTML rules.
    let isUrlAttribute : XS.Element -> XS.Name -> bool =
        let d =
            Dictionary
                (Map
                    ([
                        "a", "href"
                        "applet", "codebase"
                        "area", "href"
                        "audio", "src"
                        "base", "url"
                        "blockquote", "cite"
                        "body", "background"
                        "button", "formaction"
                        "command", "icon"
                        "del", "cite"
                        "embed", "src"
                        "form", "action"
                        "frame", "src"
                        "head", "profile"
                        "html", "manifest"
                        "iframe", "src"
                        "img", "src"
                        "input", "src"
                        "ins", "cite"
                        "link", "href"
                        "q", "cite"
                        "script", "src"
                        "source", "src"
                        "video", "src"
                    ]))
        fun elem name ->
            match d.TryGetValue(elem.Name.Local) with
            | true, v -> v = name.Local
            | _ -> false

    let joinWithSlash (a: string) (b: string) =
        let startsWithSlash (s: string) =
            s.Length > 0
            && s.[0] = '/'
        let endsWithSlash (s: string) =
            s.Length > 0
            && s.[s.Length - 1] = '/'
        match endsWithSlash a, startsWithSlash b with
        | true, true -> a + b.Substring(1)
        | false, false -> a + "/" + b
        | _ -> a + b

    /// Replaces `~` with `appPath` in URL positions.
    let postProcess (appPath: string) (element: XS.Element) =
        let rec n (node: XS.INode) : XS.INode =
            match node.Node with
            | XS.ElementNode x -> XS.ElementNode (e x) :> _
            | _ -> node
        and e (element: XS.Element) : XS.Element =
            let attributes =
                let key = Seq.tryFind (isUrlAttribute element) element.Attributes.Keys
                match key with
                | Some key ->
                    let value = element.Attributes.[key]
                    if value.StartsWith("~") then
                        let d = Dictionary(element.Attributes)
                        d.[key] <- joinWithSlash appPath (value.Substring(1))
                        d :> IDictionary<_,_>
                    else
                        element.Attributes
                | None ->
                    element.Attributes
            {
                Attributes = attributes
                Children = Seq.map n element.Children
                Name = element.Name
            }
        e element

    /// Watches a file for changes.
    let watchForChanges (path: string) (recompile: unit -> unit) =
        let dir = Path.GetDirectoryName(path)
        let file = Path.GetFileName(path)
        let watcher =
            new FileSystemWatcher(dir, file,
                EnableRaisingEvents = true,
                NotifyFilter = (NotifyFilters.LastWrite ||| NotifyFilters.Security))
        watcher.Changed.Add(fun _ -> recompile ())
        watcher :> System.IDisposable

    [<Sealed>]
    type Template<'T>(getBasicTemplate, getPageTemplate, holes: Map<string,Hole<'T>>, controls: Queue<_>) =

        static let basicTemplate holes =
            let t = ref (XT.CustomTemplate<H.Element,Web.INode,'T>(CustomXml.Instance))
            holes |> Seq.iter (fun (KeyValue (k, v)) ->
                match v with
                | SH f -> t := (!t).WithAsync(k, f)
                | EH f -> t := (!t).WithAsync(k, f))
            !t

        static let pageTemplate holes =
            let t = 
                (XT.Template<Wrapper<'T>>(), holes)
                ||> Seq.fold (fun t (KeyValue (k, v)) ->
                    match v with
                    | SH f -> t.WithAsync(k, fun x -> f x.value)
                    | EH f -> t.With(k, fun x -> [|x.extra.[k]|]))
            (t, [|SCRIPTS; STYLES; META|])
            ||> Array.fold (fun t name ->
                t.With(name, fun x -> [|x.extra.[name]|])
                 .With(name.ToLowerInvariant(), fun x -> [|x.extra.[name]|]))

        let getBasicTemplate' = lazy getBasicTemplate holes
        let getPageTemplate' = lazy getPageTemplate holes

        static let memoize f =
            let d = Dictionary()
            fun x ->
                match d.TryGetValue(x) with
                | true, y -> y
                | _ ->
                    let y = f x
                    d.[x] <- y
                    y

        static let getTemplate freq (path: string) (parse: string -> _) =
            match freq with
            | Template.Once ->
                let t = lazy parse path
                fun () -> t.Value
            | Template.PerRequest ->
                fun () -> parse path
            | Template.WhenChanged ->
                let cell = ref None
                let read () =
                    try Choice1Of2 (parse path)
                    with e -> Choice2Of2 e
                let rec load () =
                    let watcher =
                        match !cell with
                        | None ->
                            // NOTE: resource leak here, watcher
                            // does not get disposed. Not a problem if
                            // template object is static.
                            watchForChanges path reload
                        | Some (_, w) -> w
                    let v = read ()
                    cell := Some (v, watcher)
                    v
                and reload () = lock cell (load >> ignore)
                fun () ->
                    lock cell <| fun () ->
                        match !cell with
                        | Some (Choice1Of2 x, w) ->
                            x
                        | Some (Choice2Of2 _, _)
                        | None ->
                            match load() with
                            | Choice1Of2 x -> x
                            | Choice2Of2 exn -> raise exn

        new (pathSpec: string, freq: Template.LoadFrequency) =
            let path (root: string) =
                if pathSpec.StartsWith("~/") then
                    Path.Combine(root, pathSpec.Substring(2))
                else
                    pathSpec
            let getBasicTemplate holes =
                memoize (fun root ->
                    let t = basicTemplate holes
                    getTemplate freq (path root) (fun f -> t.ParseFragmentFile(f, true)))
            let getPageTemplate holes =
                memoize (fun root ->
                    let t = pageTemplate holes
                    getTemplate freq (path root) (fun f -> t.Parse(f, true)))
            Template(getBasicTemplate, getPageTemplate, Map.empty, Queue())

        new (path) = Template(path, Template.WhenChanged)

        member this.With(name: string, f: Func<'T,string>) =
            Template(getBasicTemplate, getPageTemplate, Map.add name (SH (async.Return << f.Invoke)) holes, controls)

        member this.With(name: string, f: Func<'T,#Web.INode>) =
            let h = EH (fun x -> async.Return <| Seq.singleton (f.Invoke(x) :> _))
            Template(getBasicTemplate, getPageTemplate, Map.add name h holes, controls)

        member this.With(name: string, f: Func<'T,#seq<#Web.INode>>) =
            let h = EH (fun x -> async.Return <| Seq.cast(f.Invoke(x)))
            Template(getBasicTemplate, getPageTemplate, Map.add name h holes, controls)

        member this.With(name: string, f: Func<'T,Async<string>>) =
            Template(getBasicTemplate, getPageTemplate, Map.add name (SH f.Invoke) holes, controls)

        member this.With(name: string, f: Func<'T,Async<#Web.INode>>) =
            let h = EH (fun x ->
                async {
                    let! result = f.Invoke(x)
                    return Seq.singleton (result :> _)
                })
            Template(getBasicTemplate, getPageTemplate, Map.add name h holes, controls)

        member this.With(name: string, f: Func<'T,Async<#seq<#Web.INode>>>) =
            let h = EH (fun x ->
                async {
                    let! result = f.Invoke(x)
                    return Seq.cast(result)
                })
            Template(getBasicTemplate, getPageTemplate, Map.add name h holes, controls)

        member this.Compile(root) =
            getBasicTemplate holes (defaultArg root ".")
            |> ignore
            this

        member this.Run(value: 'T, ?root: string) : seq<Web.INode> =
            let t = getBasicTemplate'.Value (defaultArg root ".")
            t().Run(value)

        member this.CheckPageTemplate(root: string) =
            ignore (getPageTemplate holes root ())

        member this.Run(env: Content.Env, x: Async<'T>, ?root: string) : Async<XS.Element> =
            let controls = Queue(controls)
            let extra = Dictionary()
            async {
            let! x = x
            for KeyValue (k, v) in holes do
                match v with
                | SH _ -> ()
                | EH es ->
                    let! children = es x
                    let div = H.NewTag "div" children
                    Seq.iter controls.Enqueue children
                    extra.[k] <-
                        use tw = new StringWriter()
                        use w = new HtmlTextWriter(tw, " ")
                        for c in children do
                            c.Write(env.Meta, w)
                        XS.CDataNode (tw.ToString()) :> XS.INode
            let res = env.GetSeparateResourcesAndScripts controls
            let tpl = getPageTemplate'.Value (defaultArg root ".") ()
            if tpl.Holes |> Seq.exists (fun h -> let h = h.ToUpperInvariant() in h = STYLES || h = META) then
                extra.[SCRIPTS] <- XS.CDataNode res.Scripts :> XS.INode
                extra.[STYLES] <- XS.CDataNode res.Styles :> XS.INode
                extra.[META] <- XS.CDataNode res.Meta :> XS.INode
            else
                let scripts = String.concat "" [|res.Styles; res.Meta; res.Scripts|]
                extra.[SCRIPTS] <- XS.CDataNode scripts :> XS.INode
            return tpl.Run {
                extra = extra
                value = x
            }
            |> postProcess env.AppPath
            }

    let WithTemplateAsync<'Action,'T>
        (template: Template<'T>)
        (content: Async<'T>) : Async<Content<'Action>> =
        CustomContentAsync (fun ctx ->
            async {
            template.CheckPageTemplate(ctx.RootFolder)
            let! xml = template.Run(Content.Env.Create ctx, content, ctx.RootFolder)
            return {
                Status = Http.Status.Ok
                Headers =
                    [
                        Http.Header.Custom "Content-Type"
                            "text/html; charset=utf-8"
                    ]
                WriteBody = fun s ->
                    use w = new System.IO.StreamWriter(s)
                    w.WriteLine("<!DOCTYPE html>")
                    XS.Node.RenderHtml w xml
            }})
        |> async.Return

    let WithTemplate<'Action,'T>
        (template: Template<'T>)
        (content: 'T) : Async<Content<'Action>> =
        WithTemplateAsync template (async.Return content)
