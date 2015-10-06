namespace WebSharper.Sitelets

open System
open WebSharper
open WebSharper.Html.Server

/// Sitelets Content functions for Html.Server.
module Content =

    /// A type of HTML elements.
    type HtmlElement = Html.Element

    module Template =

        /// Defines how frequently a template should be loaded from disk.
        type LoadFrequency =

            /// Loading happens once per application start.
            | Once

            /// Loading happens once per every request, which
            /// is useful for development.
            | PerRequest

            /// Loading detects file changes and only happens
            /// when necessary, using System.IO.FileSystemWatcher
            /// to detect changes in the file system.
            | WhenChanged

    /// <summary>Defines a new page template.  Template files are parsed as XML
    /// and then analyzed for placeholders.  There are text placeholders
    /// <c>${foo}</c> that can appear inside text nodes and attributes, and
    /// node or node-list placeholders such as
    /// <c>&lt;div data-hole="bar"&gt;</c> or <c>&lt;div data-replace="bar"&gt;</c>.
    /// Node placeholder elements get completely replaced (data-replace),
    /// or get their contents replaced (data-hole) during expansion.
    /// This mechanism allows to populate placeholders with example
    /// content and validate templates as HTML5 during development.</summary>
    [<Sealed>]
    type Template<'T> =

        /// Constructs a new template from an XML file at a given path.
        new : path: string -> Template<'T>

        /// Constructs a new template from an XML file at a given path,
        /// also specifying the load frequency (defaults to WhenChanged).
        new : path: string * freq: Template.LoadFrequency -> Template<'T>

        /// <summary>Adds a text-valued hole accessible in the
        /// template as <c>${name}</c>.</summary>
        member With : hole: string * def: Func<'T,string> -> Template<'T>

        /// <summary>Adds an element-valued hole accessible in the
        /// template via the <c>data-hole="name"</c> attribute.</summary>
        member With : hole: string * def: Func<'T,#Web.INode> -> Template<'T>

        /// <summary>Adds an element-list-valued hole accessible in the
        /// template via the <c>data-hole="name"</c> attribute.</summary>
        member With : hole: string * def: Func<'T,#seq<#Web.INode>> -> Template<'T>

        /// <summary>Adds a text-valued hole accessible in the
        /// template as <c>${name}</c>.</summary>
        member With : hole: string * def: Func<'T,Async<string>> -> Template<'T>

        /// <summary>Adds an element-valued hole accessible in the
        /// template via the <c>data-hole="name"</c> attribute.</summary>
        member With : hole: string * def: Func<'T,Async<#Web.INode>> -> Template<'T>

        /// <summary>Adds an element-list-valued hole accessible in the
        /// template via the <c>data-hole="name"</c> attribute.</summary>
        member With : hole: string * def: Func<'T,Async<#seq<#Web.INode>>> -> Template<'T>

        /// Compiles the template as a simple template. Recommended to use before Run
        /// for early detection of errors. Optionally pass the root folder.
        member Compile : ?root: string -> Template<'T>

        /// Expands the template on a given value. Optionally pass the root folder.
        member Run : value: 'T * ?root: string -> seq<Web.INode>

    /// Asynchronously applies a template as a page template for sitelet content.
    /// Extra placeholders called "scripts", "styles" and "meta" are available
    /// with WebSharper-determined dependencies. If only "scripts" is present,
    /// then it will be filled with meta, styles and scripts, in this order.
    val WithTemplateAsync<'Action,'T> :
        template: Template<'T> ->
        content: Async<'T> ->
        Async<Content<'Action>>

    /// Applies a template as a page template for sitelet content.
    /// Extra placeholders called "scripts", "styles" and "meta" are available
    /// with WebSharper-determined dependencies. If only "scripts" is present,
    /// then it will be filled with meta, styles and scripts, in this order.
    val WithTemplate<'Action,'T> :
        template: Template<'T> ->
        content: 'T ->
        Async<Content<'Action>>