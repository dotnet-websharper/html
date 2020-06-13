// $begin{copyright}
//
// This file is confidential and proprietary.
//
// Copyright (c) IntelliFactory, 2004-2012.
//
// All rights reserved.  Reproduction or use in whole or in part is
// prohibited without the written consent of the copyright holder.
//-----------------------------------------------------------------
// $end{copyright}

module IntelliFactory.Xml.Templating

open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions
open System.Xml
open System.Xml.Linq

module X = SimpleXml
type Id = string
type Message = string

let idRegex =
    Regex(@"^[a-zA-Z_$][-.$a-zA-Z0-9_:]*$")

let isIdentifier (s: string) =
    match s with
    | null | "" -> false
    | _ -> idRegex.IsMatch(s)

let isNullOrWhite (s: string) =
    s = null || Seq.forall Char.IsWhiteSpace s

let isWhitespace (node: XNode) =
    node.NodeType = XmlNodeType.Text &&
        (node :?> XText).Value
        |> isNullOrWhite

let isRegularElement (e: XElement) =
    e.Nodes()
    |> Seq.forall (fun node ->
        node.NodeType <> XmlNodeType.Text
        || isWhitespace node)

let removeWhitespaceNodes (e: XElement) =
    let nodes = Seq.toArray (e.Nodes())
    for node in nodes do
        if isWhitespace node then
            node.Remove()

let rec normWhitespace (e: XElement) =
    if isRegularElement e then
        removeWhitespaceNodes e
    e.Elements()
    |> Seq.iter normWhitespace

[<Sealed>]
type Context() =

    let holes = new HashSet<string>()

    member this.Holes = holes :> seq<_>

    member this.AddHole(h: string) =
        holes.Add(h) |> ignore

type Hole<'Element,'Node,'T> =
    | ElementHole of ('T -> Async<'Element>)
    | NodesHole of ('T -> Async<seq<'Node>>)
    | TextHole of ('T -> Async<string>)

type TextT<'T> =
    | FixedTT of string
    | HoleTT of ('T -> Async<string>)
    | SeqTT of seq<TextT<'T>>

type AttrsT<'T> =
    | FixedAT of IDictionary<X.Name,string>
    | MixedAT of IDictionary<X.Name,TextT<'T>>

type ElementT<'Element,'Node,'T> =
    | FixedET of 'Element
    | HoleET of ('T -> Async<'Element>)
    | MixedET of X.Name * AttrsT<'T> * NodesT<'Element,'Node,'T>

and NodesT<'Element,'Node,'T> =
    | CDataNT of TextT<'T>
    | ElementNT of ElementT<'Element,'Node,'T>
    | FixedNT of seq<'Node>
    | HoleNT of ('T -> Async<seq<'Node>>)
    | SeqNT of seq<NodesT<'Element,'Node,'T>>
    | TextNT of TextT<'T>

type HoleKind =
    | DataHole of string
    | DataReplace of string

    member this.Name =
        match this with
        | DataHole x | DataReplace x -> x

type IXml<'Element,'Node> =
    abstract member CData : data: string -> 'Node

    abstract member Element :
        name: X.Name
        * attributes: #IDictionary<X.Name,string>
        * children: seq<'Node> -> 'Element

    abstract member ElementNode : 'Element -> 'Node
    abstract member Text : text: string -> 'Node

[<Sealed>]
type SXml() =
    static let instance = SXml()

    interface IXml<X.Element,X.INode> with
        member this.CData(data) = X.CDataNode(data) :> X.INode

        member this.Element(name, attrs, children) : X.Element =
            {
                Name = name
                Attributes = attrs
                Children = children
            }

        member this.ElementNode(el) = X.ElementNode(el) :> X.INode
        member this.Text(text) = X.TextNode(text) :> X.INode

    static member Instance = instance :> IXml<X.Element,X.INode>

[<Sealed>]
type Parser<'Element,'Node,'T>
    (
        report: Message -> unit,
        xml: IXml<'Element,'Node>,
        getHole: Id -> option<Hole<'Element,'Node,'T>>
    ) =

    let parseText (ctx: Context) (template: string) : TextT<'T> =
        let s = template
        let r = Queue<TextT<'T>>()
        let noHoles = ref true
        use sw = new StringWriter()
        let plain (text: string) =
            r.Enqueue(FixedTT text)
            sw.Write(text)
        let custom (f: 'T -> Async<string>) =
            r.Enqueue(HoleTT f)
            noHoles := false
        let rec loop i =
            if i < s.Length then
                match s.IndexOf('$', i) with
                | -1 -> plain (s.Substring(i))
                | k ->
                    if k - i > 0 then
                        plain (s.Substring(i, k - i))
                    if k + 1 < s.Length then
                        match s.[k + 1] with
                        | '{' ->
                            match s.IndexOf('}', k + 1) with
                            | -1 -> plain (s.Substring(k))
                            | l ->
                                let name = s.Substring(k + 2, l - k - 2)
                                if isIdentifier name then
                                    match getHole name with
                                    | Some (TextHole f) ->
                                        ctx.AddHole name
                                        custom f
                                    | Some _ ->
                                        String.Format("Not a text-valued \
                                            placeholder: {0}",
                                            name)
                                        |> report
                                    | None ->
                                        String.Format("Uninstantiated placeholder: {0}. \
                                            Use .With(\"{0}\", ...) in the \
                                            template definition",
                                            name)
                                        |> report
                                else
                                    String.Format("Invalid placeholder \
                                        name: {0}",
                                        name)
                                    |> report
                                loop (l + 1)
                        | '$' ->
                            plain "$"
                            loop (k + 2)
                        | c ->
                            plain "$"
                            plain (string c)
                            loop (k + 2)
                    else
                        plain "$"
        loop 0
        if !noHoles then FixedTT (sw.ToString()) else SeqTT (r.ToArray())

    let parseAttrs (ctx: Context) (attrs: IDictionary<X.Name,string>) =
        let r = Dictionary()
        let mutable mixed = false
        for KeyValue (k, v) in attrs do
            let pt = parseText ctx v
            match pt with
            | FixedTT _ -> ()
            | _ -> mixed <- true
            r.[k] <- pt
        if mixed then MixedAT r else FixedAT attrs

    let getHoleKind (e: X.Element) =
        let ( ! ) name =
            match e.Attributes.TryGetValue(X.Name.Create(name)) with
            | true, holeName -> Some holeName
            | _  -> None
        match !"data-hole", !"data-replace" with
        | Some hole, Some replace ->
            String.Format("Cannot have both data-hole={0} \
                and data-replace={1}", hole, replace)
            |> report
            None
        | None, None -> None
        | Some hole, None -> Some (DataHole hole)
        | None, Some replace -> Some (DataReplace replace)

    let makeHole (e: X.Element) : seq<'Node> -> 'Element =
        let attrs =
            let d = Dictionary()
            for (KeyValue (k, v)) in e.Attributes do
                if k.Local <> "data-hole" && k.Local <> "data-replace" then
                    d.[k] <- v
            d
        fun (children: seq<'Node>) ->
            xml.Element(e.Name, attrs, children)

    let rec convertElement (e: X.Element) : 'Element =
        let children = Seq.toArray (Seq.map convertNode e.Children)
        xml.Element(e.Name, e.Attributes, children)

    and convertNode (n: X.INode) : 'Node =
        match n.Node with
        | X.CDataNode x -> xml.CData(x)
        | X.ElementNode e -> xml.ElementNode(convertElement e)
        | X.TextNode x -> xml.Text(x)

    let rec parseElement (ctx: Context) (element: X.Element) : ElementT<'Element,'Node,'T> =
        let errorElement =
            FixedET (xml.Element(X.Name.Create("error"), Map.empty, Seq.empty))
        let fail msg =
            report msg
            errorElement
        match getHoleKind element with
        | Some holeKind ->
            let holeName = holeKind.Name
            match getHole holeName with
            | Some (ElementHole e) ->
                ctx.AddHole holeName
                match holeKind with
                | DataReplace _ -> HoleET e
                | DataHole _ ->
                    let h = makeHole element
                    HoleET (fun x ->
                        async {
                            let! r = e x
                            return h (Seq.singleton (xml.ElementNode r))
                        })
            | Some _ -> fail ("Not an element-valued placeholder: " + holeName)
            | None -> fail ("Not a placeholder: " + holeName)
        | None ->
            let aT = parseAttrs ctx element.Attributes
            let nT = parseNodes ctx element.Children
            match aT, nT with
            | FixedAT _, FixedNT _ -> FixedET (convertElement element)
            | _ -> MixedET (element.Name, aT, nT)

    and parseNodes (ctx: Context) (nodes: seq<X.INode>) : NodesT<'Element,'Node,'T> =
        let res =
            nodes
            |> Seq.map (fun n ->
                match n.Node with
                | X.CDataNode x ->
                    CDataNT (parseText ctx x)
                | X.TextNode x ->
                    TextNT (parseText ctx x)
                | X.ElementNode e ->
                    match getHoleKind e with
                    | None -> ElementNT (parseElement ctx e)
                    | Some holeKind ->
                        let holeName = holeKind.Name
                        ctx.AddHole holeName
                        match getHole holeName with
                        | Some (NodesHole h) ->
                            match holeKind with
                            | DataHole _ ->
                                let g = makeHole e
                                HoleNT (fun x ->
                                    async {
                                        let! r = h x
                                        return Seq.singleton (xml.ElementNode (g r))
                                    })
                            | DataReplace _ ->
                                HoleNT h
                        | _ ->
                            ElementNT (parseElement ctx e))
            |> Seq.toArray
        let isFixed = function
            | CDataNT (FixedTT _)
            | TextNT (FixedTT _)
            | ElementNT (FixedET _) -> true
            | _ -> false
        if Array.forall isFixed res
        then FixedNT (Seq.map convertNode nodes)
        else SeqNT res

    member this.ParseElement ctx e = parseElement ctx e
    member this.ParseNodes ctx n = parseNodes ctx n
    member this.ParseText ctx x = parseText ctx x

[<Sealed>]
type Expander<'Element,'Node,'T>(xml: IXml<'Element,'Node>, t: 'T) =

    member e.Attrs(x: AttrsT<'T>) : Async<IDictionary<X.Name,string>> =
        match x with
        | FixedAT x -> async.Return x
        | MixedAT xs ->
            async {
                let d = Dictionary()
                for (KeyValue (k, v)) in xs do
                    let! v = e.Text v
                    do d.[k] <- v
                return d :> _
            }

    member e.Element(x: ElementT<'Element,'Node,'T>) : Async<'Element> =
        match x with
        | FixedET x -> async.Return x
        | HoleET f -> async { return! f t }
        | MixedET (n, a, c) ->
            async {
                let! aF = e.Attrs a
                let! cF = e.Nodes c
                return xml.Element(n, aF, cF)
            }

    member e.Nodes(x: NodesT<'Element,'Node,'T>) =
        match x with
        | FixedNT x -> async.Return x
        | HoleNT f -> async { return! f t }
        | CDataNT x ->
            async {
                let! txt = e.Text x
                return Seq.singleton (xml.CData txt)
            }
        | TextNT x ->
            async {
                let! txt = e.Text x
                return Seq.singleton (xml.Text txt)
            }
        | ElementNT x ->
            async {
                let! el = e.Element x
                return Seq.singleton (xml.ElementNode el)
            }
        | SeqNT xs ->
            async {
                let r = ResizeArray()
                for x in xs do
                    let! ys = e.Nodes x
                    r.AddRange(ys)
                return r.ToArray() :> seq<_>
            }

    member e.Text(x: TextT<'T>) : Async<string> =
        match x with
        | FixedTT x -> async.Return x
        | HoleTT f -> async { return! f t }
        | SeqTT xs ->
            async {
                let builder = System.Text.StringBuilder()
                for x in xs do
                    let! r = e.Text x
                    do builder.Append(r) |> ignore
                return builder.ToString()
            }

[<Sealed>]
type Renderer<'Element,'Node>(xml: IXml<'Element,'Node>) =
    member this.Element(x) = fun t -> Expander<_,_,_>(xml, t).Element(x)
    member this.Nodes(x) = fun t -> Expander<_,_,_>(xml, t).Nodes(x)
    member this.Text(x) = fun t -> Expander<_,_,_>(xml, t).Text(x)

[<Sealed>]
type Formatter<'T1,'T2>(run: 'T1 -> Async<'T2>, holes: seq<string>) =
    member this.Run(x) = Async.RunSynchronously(run x)
    member this.RunAsync(x) = run x
    member this.Holes = holes

[<Sealed>]
type ParseException(messages: string []) =
    inherit Exception(String.Join(Environment.NewLine, messages))
    member this.Messages = Seq.ofArray messages

let add<'T> (key: string) (value: 'T) (map: Map<string,'T>) =
    if Map.containsKey key map then
        failwithf "Duplicate key: %s" key
    if not (isIdentifier key) then
        failwithf "Invalid placeholder name: %s" key
    Map.add key value map

[<Sealed>]
type CustomTemplate<'Element,'Node,'T>
    (
        xml: IXml<'Element,'Node>,
        holes: Map<string,Hole<'Element,'Node,'T>>
    ) =

    static let loadOptions = function
        | Some true -> LoadOptions.PreserveWhitespace
        | Some false | None -> LoadOptions.None

    let parse f =
        let messages = Queue()
        let getHole key = Map.tryFind key holes
        let parser = Parser(messages.Enqueue, xml, getHole)
        let r = f parser
        if messages.Count = 0 then r else
            raise (ParseException(messages.ToArray()))

    new (xml) = CustomTemplate(xml, Map.empty)

    member this.WithAsync(hole: string, def: 'T -> Async<string>) =
        CustomTemplate(xml, add hole (TextHole def) holes)

    member this.WithAsync(hole: string, def: 'T -> Async<'Element>) =
        CustomTemplate(xml, add hole (ElementHole def) holes)

    member this.WithAsync(hole: string, def: 'T -> Async<#seq<'Node>>) =
        CustomTemplate(xml, add hole (NodesHole (fun t -> async { let! r = def t in return r :> seq<_> })) holes)

    member this.With(hole: string, def: Func<'T,string>) =
        this.WithAsync(hole, fun x -> async { return def.Invoke(x) })

    member this.With(hole: string, def: Func<'T,'Element>) =
        this.WithAsync(hole, fun x -> async { return def.Invoke(x) })

    member this.With(hole: string, def: Func<'T,#seq<'Node>>) =
        this.WithAsync(hole, fun x -> async { return def.Invoke(x) })

    member this.ParseElement(e: X.Element) =
        let ctx = Context()
        let r = Renderer(xml)
        parse <| fun p ->
            let e = p.ParseElement ctx e
            Formatter<'T,'Element>(r.Element e, ctx.Holes)

    member this.ParseNodes(ns: #seq<#X.INode>) =
        let ctx = Context()
        let s = Seq.map (fun x -> x :> X.INode) ns
        let r = Renderer(xml)
        parse <| fun p ->
            let e = p.ParseNodes ctx s
            Formatter(r.Nodes e, ctx.Holes)

    member this.ParseString(t: string) =
        let ctx = Context()
        let r = Renderer(xml)
        parse <| fun p ->
            let e = p.ParseText ctx t
            Formatter(r.Text e, ctx.Holes)

    member this.Parse(fileName: string, ?preserveWhiteSpace) =
        XDocument.Load(fileName, loadOptions preserveWhiteSpace).Root
        |> X.Element.FromXElement
        |> this.ParseElement

    member this.ParseFragmentFile(fileName: string, ?preserveWhiteSpace) =
        let preserveWhiteSpace = defaultArg preserveWhiteSpace false
        let xrs = XmlReaderSettings()
        xrs.ConformanceLevel <- ConformanceLevel.Auto
        xrs.ValidationType <- ValidationType.None
        xrs.IgnoreWhitespace <- not preserveWhiteSpace
        #if NET35
        xrs.ProhibitDtd <- false
        #else
        xrs.DtdProcessing <- DtdProcessing.Ignore
        #endif
        use reader = File.OpenRead(fileName)
        use xmlReader = XmlReader.Create(reader, xrs)
        let root = XElement(XName.Get("root"))
        let doc = XDocument(root)
        let canSkip () =
            match xmlReader.NodeType with
            | XmlNodeType.XmlDeclaration
            | XmlNodeType.DocumentType -> true
            | _ -> false
        while xmlReader.Read() && canSkip() do ()
        while not xmlReader.EOF do
            XNode.ReadFrom(xmlReader)
            |> root.Add
        if not preserveWhiteSpace then normWhitespace root
        X.Element.FromXElement(root).Children
        |> this.ParseNodes

    member this.Convert<'S>(f: Func<'S,'T>) =
        let g _ v =
            match v with
            | ElementHole x -> ElementHole (x << f.Invoke)
            | NodesHole x -> NodesHole (x << f.Invoke)
            | TextHole x -> TextHole (x << f.Invoke)
        CustomTemplate(xml, Map.map g holes)

[<Sealed>]
type Template<'T>(t: CustomTemplate<X.Element,X.INode,'T>) =

    new () = Template(CustomTemplate(SXml(), Map.empty))

    member this.With(hole: string, def: Func<'T,string>) =
        Template(t.With(hole, def))

    member this.With(hole: string, def: Func<'T,X.Element>) =
        Template(t.With(hole, def))

    member this.With(hole: string, def: Func<'T,#seq<#X.INode>>) =
        Template(t.With(hole, fun x -> def.Invoke(x) |> Seq.cast))

    member this.WithAsync(hole: string, def: 'T -> Async<string>) =
        Template(t.WithAsync(hole, def))

    member this.WithAsync(hole: string, def: 'T -> Async<X.Element>) =
        Template(t.WithAsync(hole, def))

    member this.WithAsync(hole: string, def: 'T -> Async<#seq<#X.INode>>) =
        let def (x: 'T) : Async<seq<X.INode>> =
            async {
                let! r = def x
                return Seq.cast r
            }
        Template(t.WithAsync(hole, def))

    member this.With(hole: string, def: Func<'T,int>) =
        this.With(hole, def.Invoke >> string)

    member this.With(hole: string, def: Func<'T,float>) =
        this.With(hole, def.Invoke >> string)

    member this.With(hole: string, def: Func<'T,bool>) =
        this.With(hole, def.Invoke >> string)

    member this.ParseElement(e: X.Element) =
        t.ParseElement(e)

    member this.ParseNodes(ns: #seq<#X.INode>) =
        t.ParseNodes(ns)

    member this.ParseString(s: string) =
        t.ParseString(s)

    member this.Parse(fileName: string, ?preserveWhiteSpace) =
        t.Parse(fileName, ?preserveWhiteSpace = preserveWhiteSpace)

    member this.ParseFragmentFile(fileName: string, ?preserveWhiteSpace) =
        t.ParseFragmentFile(fileName, ?preserveWhiteSpace = preserveWhiteSpace)

    member this.Convert<'S>(f: Func<'S,'T>) : Template<'S> =
        Template(t.Convert(f))
