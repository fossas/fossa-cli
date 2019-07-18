package errors

import "github.com/fatih/color"
import "github.com/mitchellh/go-wordwrap"

var ReportBugMessage = `

` + color.HiYellowString("REPORTING A BUG:") + `
Please try troubleshooting before filing a bug. If none of the suggestions
work, you can file a bug at ` + color.HiBlueString("https://github.com/fossas/fossa-cli/issues/new") + `.
Please attach the debug logs from:

    ` + color.HiGreenString("fossa <cmd> --debug") + `

For additional support, contact ` + color.MagentaString("support@fossa.com")

var NoAPIKeyMessage = `

` + wordwrap.WrapString("Running `fossa analyze` performs a dependency analysis and uploads the result to FOSSA. To run an analysis without uploading results, run:", 78) + `
    
    ` + color.HiGreenString("fossa analyze --output") + `

` + wordwrap.WrapString("You can provide your API key by setting the $FOSSA_API_KEY environment variable. For example, try running:", 78) + `
    
    ` + color.HiGreenString("FOSSA_API_KEY=<YOUR_API_KEY_HERE> $command") + `

` + wordwrap.WrapString("You can create an API key for your FOSSA account at:", 78) + `
    
    ` + color.HiBlueString("$endpoint/account/settings/integrations/api_tokens") + `
`
