package cmdutil

import (
	"bytes"
	"fmt"
	"text/template"
)

func OutputWithTemplateFile(tmplFilename string, data interface{}) error {
	tmpl, err := template.ParseFiles(tmplFilename)
	if err != nil {
		return err
	}

	return OutputWithTemplate(tmpl, data)
}

func OutputWithTemplate(tmpl *template.Template, data interface{}) error {
	var buf bytes.Buffer
	err := tmpl.Execute(&buf, data)
	if err != nil {
		return err
	}

	fmt.Println(string(buf.Bytes()))
	return nil
}
