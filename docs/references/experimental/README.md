## Experimental Features

At FOSSA, we like to develop in the open. 
This helps us get feedback from our users early and often, and helps our users take advantage of the newest cutting-edge features we're working on.

FOSSA also has strong commitments to backwards compatibility and support.
We respect the fact that we live in the CI pipeline, and we want to avoid anything that would get in the way of our users shipping software.

These two commitments are hard to reconcile: developing in the open means some level of backwards-incompatible changes and potential user disruption.
Our way of bridging this gap is "experimental options", which offer a peek into what we're working on next while still retaining the flexibility to iterate.

### What denotes an experimental feature?

Experimental features are always prefixed by `experimental`. For example, the flag `--output` is _not_ experimental, while the hypothetical flag `--experimental-text-output` would be.
In the case where we are testing out new functionality for an existing feature, we may simply prepend `experimental`: to reuse the previous example, a new output format might be tested under simply `--experimental-output`.

### When are experimental features used?

Experimental features are _always_ opt-in. This means that we'll never turn on an experimental feature unless the user explicitly enables it!

### CLI Versioning with experimental features

Experimental features aren't considered part of our semver-style versioning scheme. This lets us focus on only stable features in the version scheme and gives us the room we need to experiment in the open.
This means that experimental features may be introduced, change in backwards incompatible ways, or even be removed in a `PATCH` release (`x.x.PATCH`).

That being said, we do try to maintain backwards compatibility, and if we can't we will try our best to issue deprecation warnings before making backwards-incompatible changes to experimental features.

### Feature promotion

When experimental features are promoted, we perform the following steps:

1. We add the feature without an `experimental` prefix.
2. We mark the `experimental`-prefixed version of the feature as deprecated, and log warnings when it's used.
3. After at least two CLI releases we remove the `experimental`-prefixed version of the feature.

Once an experimental feature is promoted it's considered part of our backwards compatibility goals and is considered part of our semver-style CLI versioned releases.

### Support level

We do our best to support our users using experimental features! If you have a question or a problem about the experimental feature, send us an email or open an issue. 
We'd appreciate if you'd include:

- In the title/subject, whether you are using experimental features.
- In the body, the features you're using (ideally along with your actual command being run).

The main difference compared to stable features is that we don't consider experimental features to be "critical", meaning that we consider them to be something that users can stop using to unblock their CI pipeline.
As such, our first recommendation if you're in a situation where an experimental feature is blocking your CI pipeline is to disable the feature (but still send us a support request, we'd like to know about it)!

### Submitting feedback

We love getting feedback from our users, and feedback on experimental options is no different!
If you send us an email or open an issue relating to an experimental feature, we'd appreciate if you'd include:

- In the title/subject, whether you are using experimental features.
- In the body, the features you're using (ideally along with your actual command being run).

This helps us route the feedback to the person who owns the feature!
