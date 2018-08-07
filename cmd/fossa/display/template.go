package display

import (
	"bytes"
	"text/template"
)

func TemplateFile(filename string, data interface{}) (string, error) {
	tmpl, err := template.ParseFiles(filename)
	if err != nil {
		return "", err
	}

	return Template(tmpl, data)
}

func Template(tmpl *template.Template, data interface{}) (string, error) {
	var buf bytes.Buffer
	err := tmpl.Execute(&buf, data)
	if err != nil {
		return "", err
	}

	return string(buf.Bytes()), nil
}
