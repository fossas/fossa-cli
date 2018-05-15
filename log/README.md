# package log

## Log levels

### Debug
Debug outputs a literal debug message. Debug messages are useful for tracing execution and diagnosing unintended error cases.

### Notice
Notice outputs a literal notice message. Notices are non-error events that the user should be informed of.

### Warning
Warnings are non-fatal error events.

### Fatal
Fatal errors are non-recoverable, and cause an `os.Exit(1)`.

### Panic
Panics are errors that should never happen and indicate that something has gone terribly wrong. They are akin to assertion failures.