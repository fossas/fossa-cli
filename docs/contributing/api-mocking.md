# API Mocking

Testing functions that interact with the API can be complicated because the API can have many different responses or errors, but the behavior is defined outside this app.  The code that calls the API needs to be tested in a predictable and reproducible way.

The solution is mock the API.  This means that we will tell the test harness how to interpret calls to the API so that the code under test can experience different scenarios.  This is not a fake API, which would seek to mimic the API behavior, but rather a collection of requests and responses.

The framework is build around two core parts:

1. Expectations
2. Mock Effects

## Expectations

The core of the mocking is `ApiExpectations`.  `ApiExpectations` are both a description of behavior and an assertion about what behavior should occur in the test.

Expectations are added through helpers, such as `returnsOnce`.  These are added to the end of a list of expectations that are checked whenever a new request comes in.

Expectations consist of:
1. `FossaApiClientF a`: a request to match against
2. `ApiResult a`: a behavior such as returning a value or raising an error
3. `ExpectationRepetition`: a repetition specification

`ApiResult a` has two modes.  It can be a return value of type `a` that is handed back to the code under test as a successful call.  It can also be `ApiFail` which contains a `Text` message that is raised as a diagnostic failure to represent a failed call.

`ExpectationRepetition` can be `Once` or `Always`.  An expectation that is set up with `Once` will be invoked the first time it matches a request.  It will expire and be considered satisfied after that call.  An expectation set up with `Always` can be invoked many times and is always considered satisfied.

Expectations are checked and matched in the order in which they are added.

Any unsatisfied expectations at the end of the test will trigger an assertion error.  Remember that the requests must match exactly (via `Eq`).

Examples:

```haskell
-- return the same value every time a call is made.
GetOrganization `alwaysReturns` Fixtures.organization

-- Simulate an error
GetProject revision `fails` "Mock HTTP error"

-- Return one value the first time, then another on any subsequent calls
(GetScan locator scanId)
  `returnsOnce` Fixtures.scanResponse{responseScanStatus = Nothing}
(GetScan locator scanId)
  `alwaysReturns` Fixtures.scanResponse{responseScanStatus = Just "Available"}
```

## MockApi Effect

The `FossaApiClient` effects needs to have a carrier in order for the tests to run.  This is provided by the `FossaApiClientMockC` carrier in `MockApi.hs`.  This is a special carrier that implements the API using the expectations.

The expectations are configured through the `MockApi` effect.  It can add expectations, run a request against the expectations or trigger assertions.  These should not be used directly.  The exported helpers should be used instead.

Internally the `MockApiC` carrier keeps track of the expectations and provides an interface to them that `FossaApiClientMockC` can use to implement `FossaApiClient` effects against the expectations.  These are already configured in `Test.Effect`, so you should not need to set them up manually.  If you do, for some reason, make sure that the `MockApiC` is inside the `Diagnostics` effect so that its state is not lost when `recover` is used.
