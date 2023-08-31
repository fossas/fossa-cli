//! Provides more specific types for URLs.

use std::str::FromStr;

use url::Url;

/// A URL that is explicitly parsed as a base.
#[derive(Clone, Eq, PartialEq, Ord, PartialOrd, Hash, Debug)]
pub struct BaseUrl(Url);

/// Errors encountered when parsing [`BaseUrl`].
#[derive(Debug, thiserror::Error)]
pub enum ParseBaseUrlError {
    /// Encountered when the underlying URL fails to parse at all.
    #[error("parse url")]
    ParseUrl(#[from] url::ParseError),

    /// Encountered when the URL parses, but is not a base.
    #[error("parsed URL '{0}' is not a base")]
    NotBaseUrl(Url),
}

impl BaseUrl {
    /// Parse a [`BaseUrl`] from a string.
    /// The URL must be correctly formed, and must represent a base URL
    /// (which is a URL suitable for use with `join`).
    ///
    /// Parsing fails if the scheme and `:` delimiter are not followed by a `/` slash,
    /// as is typically the case of `data:` and `mailto:` URLs.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use millhone::url::BaseUrl;
    ///
    /// // URLs which can be used as a base for other URLs parse.
    /// let base = BaseUrl::parse("https://example.net/a/b.html").expect("valid base url");
    /// assert_eq!(base.as_str(), "https://example.net/a/b.html");
    ///
    /// // Relative URLs, which cannot be a base for other URLs, do not parse.
    /// BaseUrl::parse("../a/b/c.html").expect_err("invalid base url");
    /// ```
    pub fn parse(input: impl AsRef<str>) -> Result<Self, ParseBaseUrlError> {
        let parsed = Url::parse(input.as_ref())?;
        if parsed.cannot_be_a_base() {
            return Err(ParseBaseUrlError::NotBaseUrl(parsed));
        }
        Ok(Self(parsed))
    }

    /// Join self to the provided string, parsed as a URL.
    ///
    /// Note: a trailing slash is significant.
    /// Without it, the last path component is considered to be a “file” name
    /// to be removed to get at the “directory” that is used as the base:
    ///
    /// # Examples
    ///
    /// ```rust
    /// use millhone::url::BaseUrl;
    /// use url::Url;
    /// # use millhone::url::ParseBaseUrlError;
    ///
    /// # fn run() -> Result<(), ParseBaseUrlError> {
    /// let base = BaseUrl::parse("https://example.net/a/b.html")?;
    /// let url = base.join("c.png")?;
    /// assert_eq!(url.as_str(), "https://example.net/a/c.png");  // Not /a/b.html/c.png
    ///
    /// let base = BaseUrl::parse("https://example.net/a/b/")?;
    /// let url = base.join("c.png")?;
    /// assert_eq!(url.as_str(), "https://example.net/a/b/c.png");
    /// # Ok(())
    /// # }
    /// # run().unwrap();
    /// ```
    ///
    /// # Errors
    ///
    /// If the function can not parse a [`Url`] from the given string
    /// with this URL as the base URL, a [`url::ParseError`] variant is returned.
    pub fn join(&self, other: impl AsRef<str>) -> Result<Url, url::ParseError> {
        self.0.join(other.as_ref())
    }

    /// Render self as a string.
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

impl std::fmt::Display for BaseUrl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl FromStr for BaseUrl {
    type Err = ParseBaseUrlError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Self::parse(s)
    }
}
