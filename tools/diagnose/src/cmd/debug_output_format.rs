use stable_eyre::Result;
use tracing::{debug, error, info, trace, warn};

pub fn main() -> Result<()> {
    #[tracing::instrument]
    fn inner(value: &str) {
        error!("message at level 'Error' inside function 'inner'");
        info!("message at level 'Info' inside function 'inner'");
        warn!("message at level 'Warn' inside function 'inner'");
        debug!("message at level 'Debug' inside function 'inner'");
        trace!("message at level 'Trace' inside function 'inner'");
        let _ = value;
        nested_inner("some value passed to 'nested_inner'")
    }

    #[tracing::instrument]
    fn nested_inner(value: &str) {
        error!("message at level 'Error' inside function 'nested_inner_function', called from inside 'inner'");
        info!("message at level 'Info' inside function 'nested_inner_function', called from inside 'inner'");
        warn!("message at level 'Warn' inside function 'nested_inner_function', called from inside 'inner'");
        debug!("message at level 'Debug' inside function 'nested_inner_function', called from inside 'inner'");
        trace!("message at level 'Trace' inside function 'nested_inner_function', called from inside 'inner'");
        let _ = value;
    }

    #[tracing::instrument(fields(uppercased))]
    fn uppercase(input: &str) -> String {
        let result = input.to_uppercase();
        tracing::Span::current().record("uppercased", &result);
        result
    }

    info!("demonstrating how messages are logged.");
    info!("");
    error!("message at level 'Error'");
    info!("message at level 'Info'");
    warn!("message at level 'Warn'");
    debug!("message at level 'Debug'");
    trace!("message at level 'Trace'");

    let target = "hello";
    info!("");
    info!("-------");
    info!("demonstrating how spans are logged.");
    info!("calling 'uppercase' with the argument '{target}', then logging the result:");
    info!("");
    info!("the word '{target}' uppercased is...");
    let result = uppercase(target);
    info!("> '{result}'");

    info!("");
    info!("-------");
    info!("demonstrating how nested spans are logged.");
    info!("calling the function 'inner' with an argument, which will in turn call 'nested_inner' with a different argument.");
    info!("");
    inner("some value passed to 'inner'");

    Ok(())
}
