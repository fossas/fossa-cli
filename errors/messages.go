package errors

import (
	"github.com/fatih/color"
	"github.com/mitchellh/go-wordwrap"
)

const width = 78

var ReportBugMessage = `

` + color.HiYellowString("REPORTING A BUG:") + `
` + wordwrap.WrapString("Please try troubleshooting before filing a bug. If the suggestions do not help you can file a bug at "+color.HiBlueString("https://github.com/fossas/fossa-cli/issues/new")+".", width) + `
` + wordwrap.WrapString("Please attach the debug logs from:", width) + `
  
  ` + color.HiGreenString("fossa <cmd> --debug") + `
  
` + wordwrap.WrapString("For additional support, contact "+color.MagentaString("support@fossa.com")+".", width)

var NoAPIKeyMessage = `

` + wordwrap.WrapString("Running `fossa analyze` performs a dependency analysis and uploads the result to FOSSA. To run an analysis without uploading results, run:", width) + `
    
    ` + color.HiGreenString("fossa analyze --output") + `

` + wordwrap.WrapString("You can provide your API key by setting the $FOSSA_API_KEY environment variable. For example, try running:", width) + `
    
    ` + color.HiGreenString("FOSSA_API_KEY=<YOUR_API_KEY_HERE> $command") + `

` + wordwrap.WrapString("You can create an API key for your FOSSA account at:", width) + `
    
    ` + color.HiBlueString("$endpoint/account/settings/integrations/api_tokens") + `
`
