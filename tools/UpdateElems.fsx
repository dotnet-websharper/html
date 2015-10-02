#nowarn "25"

open System.IO
open System.Text.RegularExpressions

module Tags =

    let NeedsBuilding input output =
        let i = FileInfo(input)
        let o = FileInfo(output)
        not o.Exists || o.LastWriteTimeUtc < i.LastWriteTimeUtc

    let tagsFilePath =
        let d =
            Directory.GetDirectories(Path.Combine(__SOURCE_DIRECTORY__, "..", "packages"), "WebSharper.*")
            |> Array.maxBy (fun d -> DirectoryInfo(d).LastWriteTimeUtc)
        Path.Combine(d, "tools", "net40", "tags.csv")

    let groupByFst (s: seq<'a * 'b>) : seq<'a * seq<'b>> =
        s
        |> Seq.groupBy fst
        |> Seq.map (fun (k, s) -> k, Seq.map snd s)

    let Parse() =
        File.ReadAllLines(tagsFilePath)
        |> Array.map (fun line ->
            let [|``type``; status; isKeyword; name; srcname|] = line.Split ','
            let isKeyword = if isKeyword = "keyword" then true else false
            (``type``, (status, (isKeyword, name, srcname))))
        |> groupByFst
        |> Seq.map (fun (k, s) -> (k, groupByFst s |> Map.ofSeq))
        |> Map.ofSeq

    let start =
        Regex("
            # indentation
            ^(\s*)
            # comment and {{ marker
            //\s*{{\s*
            # type (tag, attr, event, etc.)
            ([a-z]+)
            # categories (normal, deprecated, colliding, event arg type)
            (?:\s*([a-z]+))*",
            RegexOptions.IgnorePatternWhitespace)
    let finish = Regex("// *}}")
    let dash = Regex("-([a-z])")

    type Elt =
        {
            /// tag, attr, event, etc.
            Type: string
            /// for tag, attr: normal / deprecated / colliding
            Category: string
            /// for event: the type of event arg
            /// javascript name
            Name: string
            /// lowercase name for F# source
            LowName: string
            /// lowercase name for F# source, with ``escapes`` if necessary
            LowNameEsc: string
            /// PascalCase name for F# source
            PascalName: string
        }
        /// camelCase name for F# source
        member this.CamelName =
            if System.Char.IsLower this.LowName.[0] then
                string(this.PascalName.[0]).ToLowerInvariant() + this.PascalName.[1..]
            else
                this.PascalName

        member this.CamelNameEsc =
            let s = this.CamelName
            if s = this.LowName then this.LowNameEsc else s

    let RunOn (path: string) (all: Map<string, Map<string, seq<bool * string * string>>>) (f: Elt -> string[]) =
        if NeedsBuilding tagsFilePath path then
            let e = (File.ReadAllLines(path) :> seq<_>).GetEnumerator()
            let newLines =
                [|
                    while e.MoveNext() do
                        yield e.Current
                        let m = start.Match(e.Current)
                        if m.Success then
                            while e.MoveNext() && not (finish.Match(e.Current).Success) do ()
                            let indent = m.Groups.[1].Value
                            let ``type`` = m.Groups.[2].Value
                            let allType =
                                match m.Groups.[3].Captures |> Seq.cast<Capture> |> Array.ofSeq with
                                | [||] ->
                                    seq {
                                        for KeyValue(category, elts) in all.[``type``] do
                                            for elt in elts do
                                                yield category, elt
                                    }
                                | categories ->
                                    seq {
                                        for s in categories do
                                            match Map.tryFind s.Value all.[``type``] with
                                            | None -> ()
                                            | Some elts ->
                                                for elt in elts do yield s.Value, elt
                                    }
                                |> Seq.sortBy (fun (_, (_, _, s)) -> s.ToLower())
                            for category, (isKeyword, name, upname) in allType do
                                let lowname = dash.Replace(name, fun m ->
                                    m.Groups.[1].Value.ToUpperInvariant())
                                let lownameEsc = if isKeyword then sprintf "``%s``" lowname else lowname
                                let x =
                                    {
                                        Type = ``type``
                                        Category = category
                                        Name = name
                                        LowName = lowname
                                        LowNameEsc = lownameEsc
                                        PascalName = upname
                                    }
                                for l in f x do
                                    yield indent + l
                            yield e.Current
                |]
            File.WriteAllLines(path, newLines)

    let Run() =
        let all = Parse()
        RunOn "WebSharper.Html.Server/Tags.fs" all <| fun e ->
            [|
                sprintf """let %s x = Html.NewTag "%s" x""" e.PascalName e.Name
            |]
        RunOn "WebSharper.Html.Server/Attributes.fs" all <| fun e ->
            [|
                sprintf """let %s x = Html.NewAttr "%s" x""" e.PascalName e.Name
            |]
        RunOn "WebSharper.Html.Client/Tag.fs" all <| fun e ->
            [|
                "[<Inline>]"
                "[<JavaScript>]"
                sprintf """member this.%s x = this.NewTag "%s" x""" e.PascalName e.Name
            |]
        RunOn "WebSharper.Html.Client/Attr.fs" all <| fun e ->
            [|
                "[<Inline>]"
                "[<JavaScript>]"
                sprintf """member this.%s x = this.NewAttr "%s" x""" e.PascalName e.Name
            |]
        RunOn "WebSharper.Html.Client/Html.fs" all <| fun e ->
            if e.Type = "tag" then
                [|
                    "[<Inline>]"
                    "[<JavaScript>]"
                    sprintf """let %s x = Tags.%s x""" e.PascalName e.PascalName
                |]
            else
                [|
                    "[<Inline>]"
                    "[<JavaScript>]"
                    sprintf """let %s x = Attr.%s x""" e.PascalName e.PascalName
                |]

Tags.Run()
