package cmdutil

import (
	"text/template"
	"bytes"
)

func ProcessTmpl(tmpl *template.Template, tmplData interface{}) (data []byte, err error) {
	var tpl bytes.Buffer
	err = tmpl.Execute(&tpl, tmplData)
	return tpl.Bytes(), err
}
