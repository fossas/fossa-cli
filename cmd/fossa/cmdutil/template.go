package cmdutil

import (
	"bytes"
	"text/template"
)

func ProcessTmpl(tmpl *template.Template, tmplData interface{}) (data []byte, err error) {
	var tpl bytes.Buffer
	err = tmpl.Execute(&tpl, tmplData)
	return tpl.Bytes(), err
}
