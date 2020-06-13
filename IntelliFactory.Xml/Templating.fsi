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

/// <summary>
/// Implements simple XML templating with <c>${foo}</c>,
/// <c>data-hole="foo"</c> and <c>data-replace="foo"</c> syntax.
/// </summary>
/// <remarks>
/// <p>A <c>Template&lt;'T&gt;</c> value basically
/// represents a function from 'T to XML described by an XML file.
/// Typical usage is to define a template, add placeholders, and parse
/// a given XML file defining the template function, to obtain an efficient
/// formatter for a given type:</p>
///
/// <code><![CDATA[
///     let formatter =
///         Template<Person>()
///             .With("name", fun p -> p.Name)
///             .With("age", fun p -> p.Age)
///             .Parse(__SOURCE_DIRECTORY__ + "\templates\Person.xml")
/// ]]></code>
///
/// <p>The XML syntax distinguishes between text-valued and element or
/// element-sequence placeholders.  Text placeholders can appear in text
/// nodes and attributes.  Element placeholders either replace or fill elements
/// marked with <c>data-hole</c> or <c>data-replace</c> attributes. For example:</p>
///
/// <code><![CDATA[
/// <html>
///   <head><title>${title}</title></head>
///   <body>
///    <div data-hole="main" id="main">
///     <p>Some sample content here that is replaced during expansion.</p>
///    </div>
///   </body>
/// </html>
/// ]]></code>
/// </remarks>
module IntelliFactory.Xml.Templating

open System
open System.Collections.Generic
module SX = SimpleXml

/// Represents a compiled template.
[<Sealed>]
type Formatter<'T1,'T2> =

    new : ('T1 -> Async<'T2>) * holes: seq<string> -> Formatter<'T1, 'T2>

    /// Runs the formatter.
    member RunAsync : 'T1 -> Async<'T2>

    /// Runs the formatter.
    member Run : 'T1 -> 'T2

    /// Get the holes that this formatter fills.
    member Holes : seq<string>

/// Thrown on template parse errors.
[<Sealed>]
type ParseException =
    class
        inherit Exception

        /// Sequence of validation messages.
        member Messages : seq<string>
    end

/// Represents a custom XML interpretation parameterized
/// by the representation of a XML nodes and elements.
/// Allows constructing arbitrary XML in this interpretation.
type IXml<'Element,'Node> =

    /// Constructs a CData node.
    abstract member CData : data: string -> 'Node

    /// Constructs an Element node.
    abstract member Element :
        name: SX.Name
        * attributes: #IDictionary<SX.Name,string>
        * children: seq<'Node> -> 'Element

    /// Embeds elements into nodes.
    abstract member ElementNode : 'Element -> 'Node

    /// Constructs a Text node.
    abstract member Text : text: string -> 'Node

/// The custom XML interpretation based on SimpleXml.
[<Sealed>]
type SXml =

    interface IXml<SX.Element,SX.INode>

    static member Instance : IXml<SX.Element,SX.INode>

/// Represents a function from 'T to a custom XML interpretation.
[<Sealed>]
type CustomTemplate<'Element,'Node,'T> =

    /// Starts defining a custom template.
    new : xml: IXml<'Element,'Node> -> CustomTemplate<'Element,'Node,'T>

    /// Adds a string-valued placeholder.
    member With : hole: string * def: Func<'T,string> ->
        CustomTemplate<'Element,'Node,'T>

    /// Adds an element-valued placeholder.
    member With : hole: string * def: Func<'T,'Element> ->
        CustomTemplate<'Element,'Node,'T>

    /// Adds an node-sequence-valued placeholder.
    member With : hole: string * def: Func<'T,#seq<'Node>> ->
        CustomTemplate<'Element,'Node,'T>

    /// Adds a string-valued placeholder.
    member WithAsync : hole: string * def: ('T -> Async<string>) ->
        CustomTemplate<'Element,'Node,'T>

    /// Adds an element-valued placeholder.
    member WithAsync : hole: string * def: ('T -> Async<'Element>) ->
        CustomTemplate<'Element,'Node,'T>

    /// Adds an node-sequence-valued placeholder.
    member WithAsync : hole: string * def: ('T -> Async<#seq<'Node>>) ->
        CustomTemplate<'Element,'Node,'T>

    /// Parses and pre-processes a given XML file to obtain an
    /// efficient formatter from 'T to XML.
    member Parse : fileName: string * ?preserveWhiteSpcace : bool -> Formatter<'T,'Element>

    /// Similar to Parse, but admits XML fragment files (multiple roots).
    member ParseFragmentFile : fileName: string * ?preserveWhiteSpcace : bool -> Formatter<'T,seq<'Node>>

    /// Parses a given element as a template.
    member ParseElement : element: SX.Element -> Formatter<'T,'Element>

    /// Parses a given node sequence as a template.
    member ParseNodes : nodes: #seq<#SX.INode> -> Formatter<'T,seq<'Node>>

    /// Parses a given string as a template.
    member ParseString : template: string -> Formatter<'T,string>

    /// Changes the type of objects the template accepts.
    member Convert<'S> : Func<'S,'T> -> CustomTemplate<'Element,'Node,'S>

/// Represents a function from 'T to SimpleXml.
[<Sealed>]
type Template<'T> =

    /// Starts defining a custom template.
    new : unit -> Template<'T>

    /// Adds a string-valued placeholder.
    member With : hole: string * def: Func<'T,string> -> Template<'T>

    /// Adds a int-valued placeholder.
    member With : hole: string * def: Func<'T,int> -> Template<'T>

    /// Adds a float-valued placeholder.
    member With : hole: string * def: Func<'T,float> -> Template<'T>

    /// Adds a bool-valued placeholder.
    member With : hole: string * def: Func<'T,bool> -> Template<'T>

    /// Adds an element-valued placeholder.
    member With : hole: string * def: Func<'T,SX.Element> -> Template<'T>

    /// Adds an node-sequence-valued placeholder.
    member With : hole: string * def: Func<'T,#seq<#SX.INode>> -> Template<'T>

    /// Adds a string-valued placeholder.
    member WithAsync : hole: string * def: ('T -> Async<string>) -> Template<'T>

    /// Adds an element-valued placeholder.
    member WithAsync : hole: string * def: ('T -> Async<SX.Element>) -> Template<'T>

    /// Adds an node-sequence-valued placeholder.
    member WithAsync : hole: string * def: ('T -> Async<#seq<#SX.INode>>) -> Template<'T>

    /// Parses and pre-processes a given XML file to obtain an
    /// efficient formatter from 'T to XML.
    member Parse : fileName: string * ?preserveWhiteSpcace : bool -> Formatter<'T,SX.Element>

    /// Parses a given element as a template.
    member ParseElement : element: SX.Element -> Formatter<'T,SX.Element>

    /// Similar to Parse, but admits XML fragment files (multiple roots).
    member ParseFragmentFile : fileName: string * ?preserveWhiteSpcace : bool -> Formatter<'T,seq<SX.INode>>

    /// Parses a given node sequence as a template.
    member ParseNodes : nodes: #seq<#SX.INode> -> Formatter<'T,seq<SX.INode>>

    /// Parses a given string as a template.
    member ParseString : template: string -> Formatter<'T,string>

    /// Changes the type of objects the template accepts.
    member Convert<'S> : Func<'S,'T> -> Template<'S>
