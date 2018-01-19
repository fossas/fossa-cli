package log

import logging "github.com/op/go-logging"

var Log = logging.MustGetLogger("main")

var Format = logging.MustStringFormatter(
	`%{color}%{level}%{color:reset} %{message}`,
)
