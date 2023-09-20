use std::collections::HashSet;

use itertools::Itertools;
use retry::{
    delay::{jitter, Exponential},
    retry_with_index,
};
use serde::Serialize;
use tap::{Conv, Pipe, TapFallible};
use tracing::{debug, info};
use ureq::Agent;
use url::Url;

use super::Error;
use crate::{api::IngestionSnippet, ext::api::declare_route, url::BaseUrl};

const MAX_SNIPPET_BATCH: usize = 1000;

declare_route!("api/v1/ingest");

#[tracing::instrument(skip_all, fields(url))]
pub fn run(
    agent: &Agent,
    base: &BaseUrl,
    snippets: HashSet<IngestionSnippet>,
) -> Result<(), Error> {
    if snippets.is_empty() {
        debug!("no snippets to upload");
        return Ok(());
    }

    let target = route_url(base);
    tracing::Span::current().record("url", target.as_str());

    // The server has a limit on the number of snippets per request.
    let batches = snippets
        .into_iter()
        .batching(|it| {
            it.take(MAX_SNIPPET_BATCH)
                .collect::<HashSet<_>>()
                .pipe(|batch| if batch.is_empty() { None } else { Some(batch) })
        })
        .collect_vec();

    // In the future we may want to parallelize this, but we're also parallelizing uploads generally.
    // To do that we need to construct a pipeline instead of the simple iteration we do today.
    let batch_count = batches.len();
    debug!(%batch_count, "uploading batches");
    for (i, snippets) in batches.into_iter().enumerate() {
        let batch_num = i + 1;
        let batch = Ingest { snippets };
        let strategy = Exponential::from_millis(10).map(jitter).take(5);
        let upload = retry_with_index(strategy, |retry| {
            debug!(%retry, %batch_num, %batch_count, "uploading batch");
            agent
                .post(target.as_str())
                .send_json(&batch)
                .tap_ok(|_| info!(%retry, %batch_num, %batch_count, "uploaded batch"))
        });
        if let Err(retry::Error { error, .. }) = upload {
            return error.conv::<Error>().pipe(Err);
        }
    }

    Ok(())
}

#[derive(Debug, Serialize)]
pub struct Ingest {
    snippets: HashSet<IngestionSnippet>,
}
