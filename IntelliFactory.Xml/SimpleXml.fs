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

#if INTERACTIVE
#r "System.Xml.Linq"
#else
module IntelliFactory.Xml.SimpleXml
#endif

open System
open System.Collections.Generic
open System.IO
open System.Web.UI
open System.Xml
open System.Xml.Linq

module SH = SimpleHtml

type Name =
    {
        local : string
        uri : string
    }

    member this.Local = this.local
    member this.Uri = this.uri

    static member Create(local: string) =
        { local = local; uri = "" }

    static member Create(local: string, uri: string) =
        match uri with
        | null -> Name.Create local
        | _ -> { local = local; uri = uri }

    static member FromXName(n: XName) =
        Name.Create(n.LocalName, n.NamespaceName)

    static member ToXName(name) =
        XName.Get(name.local, name.uri)

type Element =
    {
        Attributes : IDictionary<Name,string>
        Children : seq<INode>
        Name : Name
    }

    interface INode with
        member this.Node = ElementNode this

    static member Create(n) =
        {
            Attributes = Map.empty
            Children = Seq.empty
            Name = Name.Create(n)
        }

    static member Create(n, u) =
        {
            Attributes = Map.empty
            Children = Seq.empty
            Name = Name.Create(n, u)
        }

    static member FromXElement(elem: XElement) =
        let attrs = Dictionary()
        for attr in elem.Attributes() do
            attrs.[Name.FromXName attr.Name] <- attr.Value
        let children : INode [] =
            elem.Nodes()
            |> Seq.choose Node.FromXNode
            |> Seq.cast
            |> Seq.toArray
        {
            Name = Name.FromXName elem.Name
            Attributes = attrs
            Children = children
        }

    static member ToXElement(e: Element) =
        let nodes = Queue<obj>()
        for KeyValue (n, v) in e.Attributes do
            XAttribute(Name.ToXName n, v)
            |> nodes.Enqueue
        for c in e.Children do
            Node.ToXNode c
            |> nodes.Enqueue
        XElement(Name.ToXName e.Name, nodes)

    static member WithChildren (children: #seq<#INode>) (self: Element) =
        { self with Children = Seq.toArray (Seq.cast children) }

    static member ( - ) (self: Element, children: #seq<#INode>) =
        Element.WithChildren children self

    static member WithAttributes (attrs: #seq<string*string>) (self: Element) =
        let a = Dictionary()
        for (k, v) in attrs do
            a.[Name.Create k] <- v
        { self with Attributes = a }

    static member ( + ) (self: Element, attrs: #seq<string * string>) =
        Element.WithAttributes attrs self

    static member WithText (text: string) (self: Element) =
        { self with Children = Seq.singleton (TextNode text :> _) }

    static member ( / ) (self, text) =
        Element.WithText text self

and Node =
    | CDataNode of string
    | ElementNode of Element
    | TextNode of string

    interface INode with
        member this.Node = this

    static member ToXNode(n: INode) : XNode =
        match n.Node with
        | CDataNode text -> XCData(text) :> _
        | ElementNode e -> Element.ToXElement e :> _
        | TextNode text -> XText(text) :> _

    static member FromXNode(node: XNode) : option<Node> =
        match node.NodeType with
        | XmlNodeType.CDATA ->
            let node = node :?> XCData
            Some (CDataNode node.Value)
        | XmlNodeType.Element ->
            let node = node :?> XElement
            Some (ElementNode (Element.FromXElement node))
        | XmlNodeType.Text ->
            let node = node :?> XText
            Some (TextNode node.Value)
        | _ ->
            None

    static member WriteXml(x: XmlWriter)(this: INode) =
        match this.Node with
        | CDataNode text ->
            x.WriteCData(text)
        | ElementNode e ->
            x.WriteStartElement(e.Name.local, e.Name.uri)
            for KeyValue (n, v) in e.Attributes do
                match n.uri with
                | "" -> x.WriteAttributeString(n.Local, v)
                | _ -> x.WriteAttributeString(n.local, n.uri, v)
            for c in e.Children do
                Node.WriteXml x c
            x.WriteEndElement()
        | TextNode text ->
            x.WriteString(text)

    static member WriteHtml(h: HtmlTextWriter)(this: INode) =
        let rec convertNode (node: INode) =
            match node.Node with
            | CDataNode x -> SH.Verbatim x
            | ElementNode e ->
                let attrs =
                    e.Attributes
                    |> Seq.map (fun (KeyValue (n, v)) -> (n.Local, v))
                Seq.map convertNode e.Children
                |> SH.Element e.Name.Uri e.Name.Local attrs
            | TextNode x -> SH.Text x
        SH.Render h (convertNode this)

    static member RenderXml(w: TextWriter)(node: INode) =
        use x = new XmlTextWriter(w)
        Node.WriteXml x node

    static member RenderHtml(h: TextWriter)(node: INode) =
        use h = new HtmlTextWriter(h, " ")
        Node.WriteHtml h node

    static member ToXml(this: INode) =
        use m = new StringWriter()
        Node.RenderXml m this
        m.ToString()

    static member ToHtml(this: INode) =
        use m = new StringWriter()
        Node.RenderHtml m this
        m.ToString()

and INode =
    abstract member Node : Node

