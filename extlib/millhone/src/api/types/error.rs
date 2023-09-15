use strum::Display;

/// Errors accessing the API.
#[derive(Debug, thiserror::Error)]
pub enum Error {
    /// Communication failed at the transport level.
    #[error("transport error")]
    Transport(#[from] TransportError),

    /// The server responded with a non-success status.
    #[error("status code '{0}'")]
    Status(u16),

    /// The request was sent and a response received,
    /// but the body failed to download or decode.
    #[error("read response body")]
    ReadResponseBody(#[source] std::io::Error),
}

impl From<ureq::Error> for Error {
    fn from(value: ureq::Error) -> Self {
        match value {
            ureq::Error::Status(code, _) => Self::Status(code),
            ureq::Error::Transport(err) => Self::from(err),
        }
    }
}

impl From<ureq::Transport> for Error {
    fn from(value: ureq::Transport) -> Self {
        Self::Transport(value.into())
    }
}

/// Transport errors when accessing the API.
#[derive(Debug, thiserror::Error)]
pub struct TransportError {
    kind: TransportErrorKind,
    message: Option<String>,
}

impl std::fmt::Display for TransportError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let kind = self.kind;
        match self.message {
            Some(ref message) => write!(f, "{kind}: {message}"),
            None => write!(f, "{kind}"),
        }
    }
}

impl From<ureq::Transport> for TransportError {
    fn from(value: ureq::Transport) -> Self {
        let message = value
            .message()
            .map(String::from)
            .or_else(|| std::error::Error::source(&value).map(|err| format!("{err:#}")));
        Self {
            kind: value.kind().into(),
            message,
        }
    }
}

/// One of the types of error the can occur when processing a Request.
#[derive(Debug, PartialEq, Eq, Clone, Copy, Display)]
#[strum(serialize_all = "snake_case")]
pub enum TransportErrorKind {
    /// Some unspecified and unhandleable error.
    Generic,

    /// The url could not be understood.
    InvalidUrl,

    /// The url scheme could not be understood.
    UnknownScheme,

    /// DNS lookup failed.
    Dns,

    /// Insecure request attempted with https only set
    InsecureRequestHttpsOnly,

    /// Connection to server failed.
    ConnectionFailed,

    /// Too many redirects.
    TooManyRedirects,

    /// A status line we don't understand `HTTP/1.1 200 OK`.
    BadStatus,

    /// A header line that couldn't be parsed.
    BadHeader,

    /// Proxy information was not properly formatted
    InvalidProxyUrl,

    /// Proxy could not connect
    ProxyConnect,

    /// Incorrect credentials for proxy
    ProxyUnauthorized,
}

impl From<ureq::ErrorKind> for TransportErrorKind {
    fn from(value: ureq::ErrorKind) -> Self {
        match value {
            ureq::ErrorKind::InvalidUrl => Self::InvalidUrl,
            ureq::ErrorKind::UnknownScheme => Self::UnknownScheme,
            ureq::ErrorKind::Dns => Self::Dns,
            ureq::ErrorKind::InsecureRequestHttpsOnly => Self::InsecureRequestHttpsOnly,
            ureq::ErrorKind::ConnectionFailed => Self::ConnectionFailed,
            ureq::ErrorKind::TooManyRedirects => Self::TooManyRedirects,
            ureq::ErrorKind::BadStatus => Self::BadStatus,
            ureq::ErrorKind::BadHeader => Self::BadHeader,
            ureq::ErrorKind::InvalidProxyUrl => Self::InvalidProxyUrl,
            ureq::ErrorKind::ProxyConnect => Self::ProxyConnect,
            ureq::ErrorKind::ProxyUnauthorized => Self::ProxyUnauthorized,
            _ => Self::Generic,
        }
    }
}
