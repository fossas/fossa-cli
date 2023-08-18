use millhone::api::prelude::*;
use stable_eyre::eyre::Context;
use tracing::{error, info, warn};

#[tracing::instrument(skip_all, fields(%endpoint))]
pub fn main(endpoint: &BaseUrl) -> stable_eyre::Result<()> {
    info!("Validating connection to backend");

    let client = ApiClientV1::new(endpoint);
    let health = client.health().context("get service status")?;
    match health.overall() {
        ApiState::Healthy => info!("Backend is healthy: {health}"),
        ApiState::Degraded => warn!("Backend is degraded: {health}"),
        ApiState::Down => error!("Backend is down: {health}"),
        state => error!("Backend is in an unknown state: {state:?}"),
    }

    Ok(())
}
