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

/// Provides utilities for pretty-printing HTML5-flavored HTML.
module internal IntelliFactory.Xml.SimpleHtml

open WebSharper.Core.Resources

[<Sealed>]
type Node

val Comment : string -> Node
val Text : string -> Node
val Verbatim : string -> Node

val Element :
    uri: string ->
    name: string ->
    attrs: seq<string * string> ->
    children: seq<Node> ->
    Node

val Render : HtmlTextWriter -> Node -> unit
