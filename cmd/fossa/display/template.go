package display

import (
	"bytes"
	"log"
	"text/tabwriter"
	"text/template"
)

// TemplateFile renders a template file and its context to string.
func TemplateFile(filename string, data interface{}) (string, error) {
	tmpl, err := template.ParseFiles(filename)
	if err != nil {
		return "", err
	}

	return Template(tmpl, data)
}

// TemplateString renders a template from a string template.
func TemplateString(templateString string, data interface{}) (string, error) {
	tmpl, err := template.New("base").Parse(templateString)
	if err != nil {
		log.Fatalf("Could not parse template data: %s", err.Error())
	}
	return Template(tmpl, data)
}

// Template renders a template and its context to a string.
func Template(tmpl *template.Template, data interface{}) (string, error) {
	var buf bytes.Buffer
	err := tmpl.Execute(&buf, data)
	if err != nil {
		return "", err
	}

	return buf.String(), nil
}

// TemplateFormatTabs renders a template with the desired tab format.
func TemplateFormatTabs(tmpl string, data interface{}, minWidth, tabWidth, padding int) (string, error) {
	testTemplate, err := template.New("base").Parse(tmpl)
	if err != nil {
		return "", err
	}

	var buf bytes.Buffer
	w := tabwriter.NewWriter(&buf, minWidth, tabWidth, padding, ' ', 0)
	err = testTemplate.Execute(w, data)
	if err != nil {
		return "", err
	}

	return buf.String(), nil
}
