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

module internal IntelliFactory.Xml.SimpleHtml

open System
open System.Collections.Generic
open WebSharper.Core.Resources

type Flags =
    | None = 0
    | NoChildren = 1
    | NoTextContent = 2

type ElementKind =
    | Foreign
    | Normal
    | RawText
    | RCDATA
    | Void

type Element =
    {
        Attrs : Dictionary<string,string>
        Children : Node []
        Flags : Flags
        Kind : ElementKind
        Name : string
        Namespace : string
    }

and Node =
    | C of string
    | E of Element
    | T of string
    | V of string

let Comment t = C t
let Text t = T t
let Verbatim t = V t

let getElementKind ns (name: string) =
    match name with
    | "area" | "base" | "br" | "col" | "command"
    | "embed" | "hr" | "img" | "input" | "keygen"
    | "link" | "meta" | "param" | "source" | "track" | "wbr" -> Void
    | "script" | "style" -> RawText
    | "textarea" | "title" -> RCDATA
    | _ ->
        match ns with
        | null | "" -> Normal
        | _ -> Foreign

let Element ns (name: string) (attrs: seq<string * string>) (content: seq<Node>) : Node =
    let name =
        match ns with
        | null | "" -> name.ToLower()
        | _ -> name
    let attrs =
        let d = Dictionary()
        for (k, v) in attrs do
            d.[k.ToLower()] <- v
        d
    let children = Seq.toArray content
    let kind = getElementKind ns name
    let flags =
        if children.Length = 0 then
            Flags.NoChildren
        elif Array.forall (function E _ -> true | _ -> false) children then
            Flags.NoTextContent
        else
            Flags.None
    E {
        Attrs = attrs
        Children = children
        Flags = flags
        Kind = kind
        Name = name
        Namespace = ns
    }

let rec renderNode ignoreNS (h: HtmlTextWriter) (node: Node) =
    match node with
    | C x -> h.Write("<!-- "); h.Write(x.Replace("--", "")); h.Write(" -->")
    | E x -> renderElement ignoreNS h x
    | T x -> h.WriteEncodedText(x)
    | V x -> h.Write(x)

and renderElement ignoreNS (h: HtmlTextWriter) (x: Element) =
    h.WriteBeginTag(x.Name)
    let ignoreNS =
        match x.Namespace with
        | null | "" -> ignoreNS
        | ns when not ignoreNS -> h.WriteAttribute("xmlns", ns, true); true
        | _ -> ignoreNS
    for KeyValue (k, v) in x.Attrs do
        h.WriteAttribute(k, v, true)
    match x.Kind, x.Flags with
    | Foreign, Flags.NoChildren -> h.Write("/>")
    | _ ->
        h.Write('>')
        match x.Kind with
        | Void -> ()
        | _ ->
            let indentChildren =
                match x.Flags, x.Kind with
                | Flags.NoTextContent, Normal
                | Flags.NoTextContent, Foreign -> true
                | _ -> false
            if indentChildren then
                // h.Indent <- h.Indent + 1
                h.WriteLine()
                for c in x.Children do
                    renderNode ignoreNS h c
                    h.WriteLine()
                // h.Indent <- h.Indent - 1
            else
                for c in x.Children do
                    renderNode ignoreNS h c
            h.WriteEndTag(x.Name)

let Render h n = renderNode false h n
