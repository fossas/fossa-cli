# API Mocking

API testing can be complicated because the API can have many different responses
or errors. Additionally the API behavior is generally outside the scope of the
test.  APIs can have errors, they may be stateful, there may be network
instability and a host of other issues that conspire to cause test failures.
Unit tests shouldn't make any API calls because it makes the test slow, brittle
and non-determistic which undermines many of the advantages of unit tests such
as using them for CI on every change.

The code that calls the API still needs to be tested in a predictable and
reproducible way.  We need a way to remove all that uncertainty without
introducing burdensome complexity in our testing environment such as running an
entire application and networking stack.

The solution is mock the API.  This means that we will tell the test harness how
to interpret calls to the API so that the code under test can experience
different scenarios.  This is not a fake API, which would seek to mimic the API
behavior, but rather a collection of expected requests and responses.

## Usage

Mock API usage is a matter of setting expectations of calls that will be made
and stating what those calls should return.  This tells the test how to handle
the expected calls and that anything else is unexpected.  This is important to
guard against accidental calls so you never have to say, "I didn't know that
function had stateful behavior!"

Expectations are added through helpers, such as `returnsOnce`.  Each of these
take in a request to match against (from the effect) and then a behavior to
execute when the request is made.

An expectation is considered _satisfied_ when it has been used to fulful the
minimum number it expects.  An expectation is considered _exhausted_ if has
fulfulled the maximum number of requests it expects.  At the end of a test all
expectations should be satisfied, though they need not be exhausted.  Reusing an
exhausted expectation is an error.

The helpers are:

- ``req `returnsOnce` a`` - Expects a request of `req` for which it will return
  `a`.  This is both satisfied and exhausted after one request.
- ``req `alwaysReturns` a`` - Expects zero or more requests of `req` for which it
  will always return `a`.  This is always satisfied and never exhausted.
- ``req `fails` "Mock Error"`` - Expects exactly one request matching `req` for
  which it will raise a `Diagnostics` error with the text `"Mock Error"`.  This is both satisfied and exhausted after one request.

Expectations are appended to a list of expectations that are checked
whenever a new request comes in.  That means you can specify a series of values
to be returned on subsequent calls by specifying multiple expectations. Note
that if you specify that a request should always return the same value it will
never reach any expectations you add for the request after that, and those
expectations will never be satisfied, causing a test error.

Any unsatisfied expectations at the end of the test will trigger an assertion
error.  Remember that the requests must match exactly (via `Eq`) in order to be
satisfied.

### Examples

Examples of setting up expectations:

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

A full test:

```haskell
-- Checks whether scans have completed, which is different depending on the type
-- of project
projectIsReady revision = do
  project <- getProject revision
  if projectIsMonorepo project
    then (== "AVAILABLE") <$> getLatestScan revision
    else (== StatusSucceeded) . buildTaskStatus . buildTask <$> getLatestBuild revision

spec =
  describe "projectIsRead" $ do
    it' "should fetch scans if the project is a monorepo" $ do
      GetProject Fixtures.projectRevision
        `returnsOnce` Fixtures.project { projectIsMonorepo = True }
      GetLatestScan Fixtures.projectRevision
        `returnsOnce` Fixtures.successfulScan

      isReady <- projectIsReady Fixtures.projectRevision
      isReady `shouldBe'` True

    it' "should fetch builds if the project is NOT a monorepo" $ do
      GetProject Fixtures.projectRevision
        `returnsOnce` Fixtures.project { projectIsMonorepo = False }
      GetLatestBuild Fixtures.projectRevision
        `returnsOnce` Fixtures.successfulBuild

      isReady <- projectIsReady Fixtures.projectRevision
      isReady `shouldBe'` True

    it' "should fail if the project is invalid" $ do
      GetProject Fixtures.projectRevision `fails` "Mock invalid project error"

      expectFatal $ projectIsReady Fixtures.projectRevision
```

## Implementation

The framework is build around three core parts:

1. Expectations define how to match requests and what to do about them.
2. The `MockApi` effect and carrier define how the tests can interact with the
   expectations.
3. The `FossaApiClientMock` carrier implements the API against the `MockApi`.

The `MockApi` is separate from the `FossaApiClientMock` for two reasons.  One is
that it's clean to have a carrier that just implements the `FossaApiClient`
effect against the mock without having any extra state.  More importantly it has
to do with the way that errors and state interact.

The `FossaApiClient` typically emits errors through `Diagnostics` and so is
subject to all the error-recovery semantics therein.  This is used throughout
the codebase to implement fall-backs and error-handling.  This means that the
`FossaApiClient` carrier needs to be on top of the `Diagnostics` carrier
in order to throw those errors.

However, when `Diagnostics` recovers a branch from an error, the internals of
that computation are discarded.  This means that any state tracked in that
branch is discarded and so the mock would have no way to know what calls were
made and which expectations to exhaust.  This would mean that we could not
create a situation where a request fails, the failure is recovered from and then
the request retries and succeeds because the mock would never remember if it had
made the failing call or not.

`MockApi` then has to be below `Diagnostics` in the carrier stack.  This allows
it to keep track of its state regardless of what errors are thrown, caught or
recovered from.

### Expectations

The core of the mocking is `ApiExpectations`.  `ApiExpectations` are both a
description of behavior and an assertion about what behavior should occur in the
test.  The requests that the expectations match are of type `FossaApiClientF a`,
where the return-type, `a`, can be different depending on the type of request.
This leads to an awkward situation of matching return types to the request types
without knowing the actual type of the value.

The expectations need to ensure that request and return types match, but
otherwise can "forget" the type.  This can be achieved with a GADT to "erase"
the return type.

```haskell
data ApiExpectation where
  ApiExpectation ::
    ExpectationRepetition
    -> FossaApiClientF a
    -> ApiResult a
    -> ApiExpectation
```

Expectations consist of:
1. `ExpectationRepetition`: a repetition specification used to track when it is
   exhausted
1. `FossaApiClientF a`: a request to match against
1. `ApiResult a`: a behavior such as returning a value or raising an error

The repetition specifies how to exhaust the the expectation.  It's currently
only two options, but there is room for expansion.

- `Once` means the expectation should be exhausted after one match.
- `Always` means the expectation should never be exhausted.

The request is used to match incoming requests with `Eq`.  It would be more
flexible as a predicate, but this would make error messages harder to write.  A
copy of the request is very useful in debugging and writing errors.

`ApiResult a` has two modes.  It can be a return value of type `a` that is
handed back to the code under test as a successful call.  It can also be
`ApiFail` which contains a `Text` message that is raised as a diagnostic failure
to represent a failed call.  Further types could be added here such as the
ability to specify an ordered list of responses to return one at a time.

### MockApi Effect

The expectations are configured through the `MockApi` effect.  It can add
expectations, run a request against the expectations or trigger assertions.
These effects aren't intended to be used directly from tests and the helpers
should be used instead.

### MockApi Carrier

The `MockApi` is primarily responsible for keeping track of state, but also
handles some assertions on that state.  Expectations are added and matched as
the test progresses and at some point the test will want to check whether the
expectations have been satisfied.  The `MockApi` handles all the logic of what
expectations match requests and how they get satisfied or exhausted.

The internal state is just a list of expectations in the order in which they
should be checked.  Expectations are removed when they are exhaused so the data
structure needs to support that.  The set of expectations is small enough that
the performance implications of using a list are minor.  The framework should
use a more complicated type if the need arrises.

### FossaApiClientMock

The `FossaApiClientMockC` is the carrier that implements the API in terms of the
expectations.  When a request is made it just asks the `MockApi` what to do and
lets it check and manage it's own internal expectations.