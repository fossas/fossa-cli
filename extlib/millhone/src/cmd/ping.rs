use millhone::api::prelude::*;
use stable_eyre::{eyre::Context, Report};
use tracing::{error, info, warn};

#[tracing::instrument(skip_all, fields(%endpoint))]
pub async fn main(endpoint: &BaseUrl) -> Result<(), Report> {
    info!("Validating connection to backend");

    let client = ApiClientV1::new(endpoint).context("create client")?;
    let health = client.health().await.context("get service status")?;
    match health.overall() {
        ApiState::Healthy => info!("Backend is healthy: {health}"),
        ApiState::Degraded => warn!("Backend is degraded: {health}"),
        ApiState::Down => error!("Backend is down: {health}"),
        state => error!("Backend is in an unknown state: {state:?}"),
    }

    Ok(())
}
