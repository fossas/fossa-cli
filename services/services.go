package services

import (
	"net/url"
)

// New constructs and initializes a default implementation of Services.
func New(interactive, debug bool) Services {
	logService := newLogService(interactive, debug)
	apiService := newAPIService(logService)
	execService := newExecService(logService)
	fileService := newFileService(logService)

	return Services{
		Logger:     logService,
		API:        apiService,
		Exec:       execService,
		FileSystem: fileService,
	}
}

// Services is a container for all services. Each service implements
// side-effecting functions.
type Services struct {
	Logger     LogService
	API        APIService
	Exec       ExecService
	FileSystem FileService
}

// A Spinner provides user-interactive progress messages through ANSI codes.
type Spinner interface {
	Spin(message string)
	Stop()
}

// A LogService implements all logging functionality. All output is printed
// to STDERR, except for the specific case of `Printf()`, which prints to
// STDOUT. STDOUT should be reserved only for actual machine-parseable output,
// and STDERR should be used for all other user-interactive output.
type LogService interface {
	// Returns the spinner. There should only be at most one spinner active at
	// one time.
	Spinner() Spinner

	// Debug statements are meant for diagnosing and resolving issues. They are
	// not shown unless `--debug` is specified. When `--debug` is set, all
	// statements are logged with extra debug information.
	Debugf(format string, args ...interface{})

	// Notices inform the user of a non-error condition that is important.
	Noticef(format string, args ...interface{})

	// Warnings inform the user of a non-fatal error condition.
	Warningf(format string, args ...interface{})

	// Fatals cause the program to exit with a non-zero exit code. They inform
	// the user of fatal error conditions.
	Fatalf(format string, args ...interface{})

	// Printing sends output to STDOUT.
	Printf(format string, args ...interface{})
}

// TimeoutError is an error caused by a connection timeout.
type TimeoutError = error

// An APIService implements low-level HTTP functionality. The API package
// implements a high-level interface on top of this.
type APIService interface {
	// Initialize configures the APIService with a default server and API key.
	Initialize(server, APIKey string) error

	// These functions are for convenience; for most cases, the server and API
	// key don't change over the life of the command.
	Get(URL string, body []byte) (res string, statusCode int, err error)
	Post(URL string, body []byte) (res string, statusCode int, err error)

	GetJSON(URL string, body []byte, v interface{}) (statusCode int, err error)
	PostJSON(URL string, body []byte, v interface{}) (statusCode int, err error)

	// This is the underlying implementation of the APIService's functionality.
	MakeAPIRequest(endpoint *url.URL, method, APIKey string, body []byte) (res []byte, statusCode int, err error)
}

// A WhichResolver is a function which determines the version and existence of
// an external binary given a command.
type WhichResolver func(cmd string) (version string, ok bool)

// An ExecService implements calls to external commands.
type ExecService interface {
	RunCWD(cmd string, args ...string) (stdout string, stderr string, err error)
	Run(dir string, cmd string, args ...string) (stdout string, stderr string, err error)

	Which(args []string, candidates ...string) (cmd string, version string, err error)
	WhichWithResolver(resolver WhichResolver, candidates ...string) (cmd string, version string, err error)

	// GetEnv(name string) string
}

// An UnmarshalFunc unmarshals a particular data format. `json.Unmarshal` is an
// example of this.
type UnmarshalFunc func(data []byte, v interface{}) error

// A FileService implements filesystem interaction.
type FileService interface {
	HasFile(pathElems ...string) bool
	HasFolder(pathElems ...string) bool
	ReadFile(pathElems ...string) ([]byte, error)
	ReadJSON(v interface{}, pathElems ...string) error
	ReadUnmarshal(unmarshal UnmarshalFunc, v interface{}, pathElems ...string) error
}
