namespace WebSharper.Html.Tests

#if NET461

type Global() =
    inherit System.Web.HttpApplication()

    member g.Application_Start(sender: obj, args: System.EventArgs) =
        ()

#endif