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

/// Defines data structures and operations for handling
/// a simplified XML subset and formatting it to HTML and XML.
/// As a convenience for generating HTML, CDATA nodes are interpreted
/// as "raw" HTML during HTML generation.
module IntelliFactory.Xml.SimpleXml

open System.Collections.Generic
open System.IO
open System.Xml
open System.Xml.Linq
open System.Web.UI

/// Represents qualified names.
[<Sealed>]
type Name =
    interface System.IComparable

    /// The local part of the name.
    member Local : string

    /// The URI part of the name, or an empty string.
    member Uri : string

    /// Creates a new Name without the URI part.
    static member Create : local: string -> Name

    /// Creates a new Name with a given URI part.
    static member Create : local: string * uri: string -> Name

/// Represents simple XML elements.
type Element =
    {
        /// The attribute collection.
        Attributes : IDictionary<Name,string>

        /// The children collection.
        Children : seq<INode>

        /// The qualified name.
        Name : Name
    }

    interface INode

    /// Constructs an new empty Element.
    static member Create : name: string -> Element

    /// Constructs an new empty Element.
    static member Create : name: string * uri: string -> Element

    /// Constructs an element from an XElement.
    static member FromXElement : element: XElement -> Element

    /// Converts the element to an XElement.
    static member ToXElement : element: Element -> XElement

    /// Replaces the children.
    static member WithChildren : children: #seq<#INode> -> self: Element -> Element

    /// Replaces the children.
    static member ( - ) : self: Element * children: #seq<#INode> -> Element

    /// Replaces the attributes.
    static member WithAttributes : attrs: #seq<string*string> -> self: Element -> Element

    /// Replaces the attributes.
    static member ( + ) : self: Element * attrs: #seq<string*string> -> Element

    /// Replaces the children with a single text node.
    static member WithText : text: string -> self: Element-> Element

    /// Replaces the children with a single text node.
    static member ( / ) : self: Element * text: string -> Element

/// Represents simple XML nodes.
and Node =
    | CDataNode of string
    | ElementNode of Element
    | TextNode of string

    interface INode

    /// Parses an XNode.
    static member FromXNode : node: XNode -> option<Node>

    /// Converts the node to an XNode.
    static member ToXNode : node: INode -> XNode

    /// Writes the node as HTML.
    static member ToHtml : node: INode -> string

    /// Writes the node as HTML.
    static member ToXml : node: INode -> string

    /// Writes the node as HTML.
    static member RenderHtml : writer: TextWriter -> node: INode -> unit

    /// Writes the node as XML.
    static member RenderXml : writer: TextWriter -> node: INode -> unit

    /// Writes the element to a given HTML writer.
    static member WriteHtml : writer: HtmlTextWriter -> node: INode -> unit

    /// Writes the node to a given XML writer.
    static member WriteXml : writer: XmlWriter -> node: INode -> unit

/// An interface for node-equivalent types.
and INode =

    /// The XML node this object represents.
    abstract member Node : Node
