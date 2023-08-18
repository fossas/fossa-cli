use millhone::{api::prelude::*, url::BaseUrl};
use stable_eyre::eyre::Context;
use tracing::{error, info, warn};

#[tracing::instrument(skip_all, fields(%endpoint))]
pub fn main(endpoint: &BaseUrl) -> stable_eyre::Result<()> {
    info!("Validating connection to backend");

    let client = millhone::api::v1::Client::new(endpoint);
    let health = client.health().context("get service status")?;
    match health.overall() {
        State::Healthy => info!("Backend is healthy: {health}"),
        State::Degraded => warn!("Backend is degraded: {health}"),
        State::Down => error!("Backend is down: {health}"),
        state => error!("Backend is in an unknown state: {state:?}"),
    }

    Ok(())
}
