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
namespace WebSharper.Html.Server

open System
open System.Collections.Generic
open System.IO
open System.Xml
open WebSharper
open WebSharper.Web
type internal HtmlTextWriter = WebSharper.Core.Resources.HtmlTextWriter

/// Provides types for expressing HTML values.
[<AutoOpen>]
module Html =

    /// Represents HTML tags.
    type TagContent =
        {
            Name : string
            Attributes : list<INode>
            Contents : list<INode>
            Annotation : option<IRequiresResources>
        }

    /// Represents HTML attributes.
    and Attribute =
        {
            Name : string
            Value : string
            Annotation : option<IRequiresResources>
        }

        interface INode with

            member this.IsAttribute = true

            member this.Requires(meta) =
                match this.Annotation with
                | None -> Seq.empty
                | Some a -> a.Requires(meta)

            member this.Write(_, w) =
                w.WriteAttribute(this.Name, this.Value, true)

            member this.Encode(meta, json) =
                []

    /// Represents HTML/XML contents.
    and  Element =
        | TagContent of TagContent
        | TextContent of string
        | VerbatimContent of string
        | CommentContent of string
        | INodeContent of INode

        interface INode with

            member this.IsAttribute =
                match this with
                | INodeContent n -> n.IsAttribute
                | _ -> false

            member this.Requires(meta) =
                match this with
                | TagContent t ->
                    Seq.concat [|
                        (match t.Annotation with
                        | None -> Seq.empty
                        | Some a -> a.Requires(meta))
                        t.Contents |> Seq.collect (fun x -> x.Requires(meta))
                        t.Attributes |> Seq.collect (fun x -> x.Requires(meta))
                    |]
                | INodeContent n -> n.Requires(meta)
                | _ -> Seq.empty

            member this.Write(ctx, w) =
                let meta = ctx.Metadata

                // Renders an element without content and with self
                // closing tagE.g. <img class="c" />
                let renderElementWithSelfClosingTag name (attrs: list<INode>) =
                    w.WriteBeginTag(name)
                    for a in attrs do a.Write(ctx, w)
                    w.Write(HtmlTextWriter.SelfClosingTagEnd)

                // Renders tag with content.
                // E.g. <div class="c">...</div>
                let renderElementWithContent name
                        (attrs: list<INode>)
                        (contents : list<INode>) =

                    w.WriteBeginTag(name)
                    for a in attrs do
                        a.Write(ctx, w)
                    w.Write(HtmlTextWriter.TagRightChar)

                    // Render contents
                    for content in contents do
                        content.Write(ctx, w)
                    w.WriteEndTag(name)
                //////////////////////////////////////////////////////////
                // TODO: replace with content from res when appropriate //
                //////////////////////////////////////////////////////////
                match this with
                | TagContent element ->
                    match element.Contents with
                    | [] when HtmlTextWriter.IsSelfClosingTag element.Name ->
                        renderElementWithSelfClosingTag element.Name element.Attributes
                    | contents ->
                        renderElementWithContent element.Name element.Attributes contents
                | TextContent text ->
                    w.WriteEncodedText text
                | VerbatimContent text ->
                    w.Write text
                | CommentContent comment ->
                    w.Write "<!--"
                    comment.Replace("--", " - - ")
                    |> w.Write
                    w.WriteLine "-->"
                | INodeContent n -> n.Write(ctx, w)

            member this.Encode(meta, json) =
                match this with
                | TagContent element ->
                    element.Contents |> List.collect (fun e -> e.Encode(meta, json))
                | INodeContent n -> n.Encode(meta, json)
                | _ -> []

    /// Constructs a new HTML element.
    let NewTag (name: string) (elements: seq<#INode>) =
        let attrs, contents =
            elements
            |> Seq.cast<INode>
            |> List.ofSeq
            |> List.partition (fun x -> x.IsAttribute)
        TagContent {
            Name = name
            Attributes = attrs
            Contents = contents
            Annotation = None
        }

    let (-<) (el: Element) (elements: seq<#INode>) : Element =
        match el with
        | TagContent el ->
            let newAttrs, newContent =
                elements
                |> Seq.cast<INode>
                |> List.ofSeq
                |> List.partition (fun x -> x.IsAttribute)
            TagContent {
                Name = el.Name
                Attributes = el.Attributes @ newAttrs
                Contents = el.Contents @ newContent
                Annotation = el.Annotation
            }
        | INodeContent _
        | VerbatimContent _
        | TextContent _
        | CommentContent _ ->
            failwith "Invalid HTML operation - can't append to a non-tag node"

    /// Constructs a new attribute value.
    let NewAttr name value =
        {
            Name = name
            Value = value
            Annotation = None
        }

    /// Sets an annotation on the element.
    /// If the element is a non-tag element, this operation is a no-op.
    let Annotate (x: IRequiresResources) (e: Element) =
        match e with
        | TagContent e ->
            { e with Annotation = Some x }
            |> TagContent
        | INodeContent _
        | VerbatimContent _
        | TextContent _
        | CommentContent _ -> e
