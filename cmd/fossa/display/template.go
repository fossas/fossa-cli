package display

import (
	"strings"
	"text/tabwriter"
	"text/template"

	"github.com/fossas/fossa-cli/errors"
)

// TemplateFile renders a template file and its context to string.
func TemplateFile(filename string, data interface{}) (string, error) {
	tmpl, err := template.ParseFiles(filename)
	if err != nil {
		return "", errors.Wrap(err, "Could not parse template data")
	}

	return Template(tmpl, data)
}

// TemplateString renders a template from a string template.
func TemplateString(templateString string, data interface{}) (string, error) {
	tmpl, err := template.New("base").Parse(templateString)
	if err != nil {
		return "", errors.Wrap(err, "Could not parse template data")
	}
	return Template(tmpl, data)
}

// Template renders a template and its context to a string.
func Template(tmpl *template.Template, data interface{}) (string, error) {
	var builder strings.Builder
	err := tmpl.Execute(&builder, data)
	if err != nil {
		return "", err
	}

	return builder.String(), nil
}

// TemplateFormatTabs renders a template with the desired tab format.
func TemplateFormatTabs(tmpl string, data interface{}, minWidth, tabWidth, padding int) (string, error) {
	testTemplate, err := template.New("base").Parse(tmpl)
	if err != nil {
		return "", errors.Wrap(err, "Could not parse template data")
	}

	var builder strings.Builder
	w := tabwriter.NewWriter(&builder, minWidth, tabWidth, padding, ' ', 0)
	err = testTemplate.Execute(w, data)
	if err != nil {
		return "", err
	}

	return builder.String(), nil
}
