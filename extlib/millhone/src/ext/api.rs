/// Define an API route from the provided literal.
///
/// - Creates the constant `ROUTE` in the current scope.
/// - Creates the function `route_url` in the current scope.
///
/// `route_url` uses `expect` to unwrap any possible URL parse errors,
/// so this macro also creates a test module (named `test_route`)
/// which validates that the route provided to this macro joins
/// successfully with an arbitrary base URL.
macro_rules! declare_route {
    ($route:literal) => {
        const ROUTE: &str = $route;

        /// Joins the given base URL with the route provided to the `declare_route` macro.
        fn route_url(base: &$crate::url::BaseUrl) -> Url {
            base.join(ROUTE).expect("route must join with base URL")
        }

        #[cfg(test)]
        mod test_route {
            use super::*;
            use crate::url::BaseUrl;

            #[test]
            fn joins_with_base() {
                let base = "http://example.com";
                let parsed = BaseUrl::parse(base).expect("must parse base");
                let joined = parsed.join(ROUTE).expect("must join with base");
                let expected = format!("{base}/{ROUTE}");
                assert_eq!(
                    joined.as_str(),
                    expected,
                    "routes should be simple relative URLs"
                );
            }

            #[test]
            fn creates_target_url() {
                let base = "http://example.com";
                let parsed = BaseUrl::parse(base).expect("must parse base");
                let target = route_url(&parsed);
                let expected = format!("{base}/{ROUTE}");
                assert_eq!(
                    target.as_str(),
                    expected,
                    "routes should be simple relative URLs"
                );
            }
        }
    };
}

pub(crate) use declare_route;
