use std::collections::HashSet;

use retry::{
    delay::{jitter, Exponential},
    retry_with_index,
};
use tap::{Pipe, TapFallible};
use tracing::{debug, info, warn};
use ureq::Agent;
use url::Url;

use super::Error;
use crate::{api::ApiSnippet, ext::api::declare_route, extract::Fingerprint, url::BaseUrl};

declare_route!("api/v1/lookup/");

#[tracing::instrument(skip_all, fields(url))]
pub fn run(agent: &Agent, base: &BaseUrl, fp: &Fingerprint) -> Result<HashSet<ApiSnippet>, Error> {
    let target = route_url(base).join(&fp.as_base64_url())?;
    tracing::Span::current().record("url", target.as_str());

    // In the future we may want to parallelize this, but we're also parallelizing across files.
    // To do that we need to construct a pipeline instead of the simple iteration we do today.
    let strategy = Exponential::from_millis(10).map(jitter).take(5);
    let response = retry_with_index(strategy, |retry| {
        debug!(%retry, "fetching matching snippets");
        agent
            .get(target.as_str())
            .call()
            .tap_ok(|_| info!(%retry, "fetched matching snippets"))
    });

    match response {
        Err(retry::Error { error, .. }) => Error::from(error).pipe(Err),
        Ok(response) => response.into_json().map_err(Error::ReadResponseBody),
    }
}
