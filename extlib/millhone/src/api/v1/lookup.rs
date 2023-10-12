use std::collections::HashSet;

use reqwest::Client;
use tokio_retry::{
    strategy::{jitter, ExponentialBackoff},
    RetryIf,
};
use url::Url;

use super::Error;
use crate::{api::ApiSnippet, ext::api::declare_route, extract::Fingerprint, url::BaseUrl};

declare_route!("api/v1/lookup/");

#[tracing::instrument(skip_all, fields(url))]
pub async fn run(
    agent: &Client,
    base: &BaseUrl,
    fp: &Fingerprint,
) -> Result<HashSet<ApiSnippet>, Error> {
    let target = route_url(base)
        .join(&fp.as_base64_url())
        .map_err(|err| Error::EncodeReqUrl(base.to_string(), fp.as_base64_url(), err))?;
    tracing::Span::current().record("url", target.as_str());

    let strategy = ExponentialBackoff::from_millis(10).map(jitter).take(5);
    RetryIf::spawn(
        strategy,
        || async { fetch(agent, target.as_str()).await },
        |err: &Error| !matches!(err, Error::Status(_, _, 401)),
    )
    .await
}

#[tracing::instrument(skip_all)]
async fn fetch(agent: &Client, url: &str) -> Result<HashSet<ApiSnippet>, Error> {
    let response = agent
        .get(url)
        .send()
        .await
        .map_err(|err| Error::Request(url.to_string(), err))?;

    let status = response.status();
    let body = response
        .bytes()
        .await
        .map_err(|err| Error::DownloadResponseBody(url.to_string(), err))?;

    if !status.is_success() {
        let body = String::from_utf8_lossy(&body).to_string();
        return Err(Error::Status(url.to_string(), body, status.as_u16()));
    }

    serde_json::from_slice(&body)
        .map_err(|err| Error::ParseResponseBody(url.to_string(), body.to_vec(), err))
}
