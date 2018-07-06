package config

func StringFlag(name string) string {
	if ctx == nil {
		return ""
	}
	return ctx.String(name)
}

func BoolFlag(name string) bool {
	if ctx == nil {
		return false
	}
	return ctx.Bool(name)
}
