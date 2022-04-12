# API Mocking

Testing functions that interact with the API can be complicated because the API can have many different responses or errors, but the behavior is outside the scope of the test.  APIs can have errors, they may be stateful, there may be network instability and a host of other issues that conspire to cause test failures.  Unit tests shouldn't make any API calls because it makes the test slow, brittle and non-determistic which undermines many of the advantages of unit tests such as using them for CI on every change.

The code that calls the API still needs to be tested in a predictable and reproducible way.  We need a way to remove all that uncertainty without introducing burdensome complexity in our testing environment such as running an entire application and networking stack.

The solution is mock the API.  This means that we will tell the test harness how to interpret calls to the API so that the code under test can experience different scenarios.  This is not a fake API, which would seek to mimic the API behavior, but rather a collection of requests and responses.

## Usage

Using the Mock API is a matter of setting expectations of calls that will be made and at the same time stating what those calls should return.  This lets us tell the test how to handle the expected calls but it also tells the test that anything else is unexpected.  This is important because it guards against accidental calls so you never have to say, "I didn't know that function had stateful behavior!"

Expectations are added through helpers, such as `returnsOnce`.  Each of these take in a request to match against (from the effect) and then a behavior to execute when the request is made.

The helpers are:

- ``req `returnsOnce` a`` - Expects a request of `req` for which it will return `a`.
- ``req `alwaysReturns` a`` - Expects zero or requests of `req` for which it will always return `a`.
- ``req `fails` "Mock Error"`` - Expects exactly one request matching `req` for which it will raise a `Diagnostics` error with the text `"Mock Error"`.

Expectations are added to the end of a list of expectations that are checked whenever a new request comes in.  That means you can specify a series of values to be returned on subsequent calls, but if you specify that a request should always return the same value it will never reach any expectations you add for the quest after that.

An expectation is considered _satisfied_ when it has been used to fulful the minimum number it expects.  An expectation is considered _exhausted_ if has fulfulled all the requests it can and cannot match any further requests.  `returnsOnce` and `fails` are both satisfied and exhausted after one matching request.  `alwaysReturns` expects zero or more requests so it is always satisfied but never exhausted.

Any unsatisfied expectations at the end of the test will trigger an assertion error.  Remember that the requests must match exactly (via `Eq`) in order to be satisfied..

### Examples

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

## Implementation

The framework is build around three core parts:

1. Expectations define how to match requests and what to do about them.
2. The `MockApi` effect and carrier define how the tests can interact with the expectations.
3. The `FossaApiClientMock` carrier implements the API against the `MockApi`.

The `MockApi` is separate from the `FossaApiClientMock` for two reasons.  One is that it's clean to have a carrier that just implements the `FossaApiClient` effect against the mock without having any extra state.  More importantly it has to do with the way that errors and state interact.

The `FossaApiClient` typically emits errors through `Diagnostics` and so is subject to all the error-recovery semantics therein.  This is used throughout the codebase to implement fall-backs and error-handling.  This means that the `FossaApiClient` implementation needs to be on top of the `Diagnostics` carrier in order to throw those errors.

However, when `Diagnostics` recovers a branch from an error, the internals of that computation are discarded.  This means that any state tracked in that branch is discarded.  Expectations need to be tracked across calls that are recovered from because otherwise the expectation that caused the failure will not be exhausted for when the request is retried, leading to endless failures and no way to test retry.  It will also maen that we cannot track what calls were made in that branch of code and so satisfy and exhaust those expectations.

`MockApi` then has to be below `Diagnostics` in the carrier stack.  This allows it to keep track of its state regardless of what errors are thrown, caught or recovered from.

### Expectations

The core of the mocking is `ApiExpectations`.  `ApiExpectations` are both a description of behavior and an assertion about what behavior should occur in the test.  The requests that the expectations match are of type `FossaApiClientF a`, where the return-type, `a`, can be different depending on the type of request.  This leads to an awkward situation where we need to match return types to the request types, but we do not know the actual type of the value.

This leads to creating a heterogeneous list.  We can do that by using a GADT to "erase" the return type.

```haskell
data ApiExpectation where
  ApiExpectation :: ExpectationRepetition -> FossaApiClientF a -> ApiResult a -> ApiExpectation
```

Expectations consist of:
1. `ExpectationRepetition`: a repetition specification
1. `FossaApiClientF a`: a request to match against
1. `ApiResult a`: a behavior such as returning a value or raising an error

The repetition specifies how to exhaust the the expectation.  It's currently only two options, but there is room for expansion.

- `Once` means the expectation should be exhausted after one match.
- `Always` means the expectation should never be exhausted.

The request is used to match incoming requests with `Eq`.  It would be more flexible as a predicate, but this would make error messages harder to write.  A copy of the request is very useful in debugging and writing errors.

`ApiResult a` has two modes.  It can be a return value of type `a` that is handed back to the code under test as a successful call.  It can also be `ApiFail` which contains a `Text` message that is raised as a diagnostic failure to represent a failed call.  Further types could be added here such as the ability to specify an ordered list of responses to return one at a time.

### MockApi Effect

The expectations are configured through the `MockApi` effect.  It can add expectations, run a request against the expectations or trigger assertions.  These effects aren't intended to be used directly from tests and the helpers should be used instead.

### MockApi Carrier

The `MockApi` is primarily responsible for keeping track of state, but also handles some assertions on that state.  Expectations are added and matched as the test progresses and at some point the test will want to check whether the expectations have been satisfied.  The `MockApi` handles all the logic of what expectations match requests and how they get satisfied or exhausted.

The internal state is just a list of expectations in the order in which they should be checked.  The key value of a list is how easy it is to add, traverse and remove elements.  Expectations are removed when they are exhaused so the data structure needs to support that.  The set of expectations is small enough that the performance implications are minor.  The framework should use a more complicated type if the need arrises.

### FossaApiClientMock

The `FossaApiClientMockC` is the carrier that implements the API in terms of the expectations.  When a request is made it just asks the `MockApi` what to do and lets it check and manage it's own internal expectations.