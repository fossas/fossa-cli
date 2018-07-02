package cmdutil

import (
	"bytes"
	"encoding/json"
	"io/ioutil"
	"os"
	"text/template"

	"github.com/fossas/fossa-cli/log"
)

func ProcessTmpl(tmpl *template.Template, tmplData interface{}) (data []byte, err error) {
	var tpl bytes.Buffer
	err = tmpl.Execute(&tpl, tmplData)
	return tpl.Bytes(), err
}

func OutputData(outputFile string, tmpl *template.Template, data interface{}) (err error) {
	var (
		msg []byte
	)

	if tmpl == nil {
		msg, err = json.Marshal(data)
		println()
		if err != nil {
			log.Logger.Fatalf("Could not marshal output: %s", err.Error())
			return err
		}
	} else {
		msg, err = ProcessTmpl(tmpl, data)
		if err != nil {
			log.Logger.Fatalf("Could not process template data: %s", err.Error())
			return err
		}
	}

	if outputFile == "-" {
		_, err = os.Stdout.Write(msg)
		return err
	}

	return ioutil.WriteFile(outputFile, msg, 0664)
}
