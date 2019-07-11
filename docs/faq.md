# Frequently Asked Questions
This page lists the most commonly asked questions about the FOSSA CLI, if your question is not listed you can contact support@fossa.com. If your question is related to a bug or feature request please open up a new GitHub issue.

Frequently asked questions related to specific build environments can be found in their corresponding pages, [golang faq](integrations/golang.md#faq) for example.

### Why is FOSSA not finding my test and development dependencies?
Fossa by default excludes test and development dependencies as they have little to no implications when related to licensing issues. If you desire to include these in your scan there are options for each module type that will allow you to scan for then. These options can be found in the language specific pages.

### Are FOSSA CLI docker images available for me to download?
Fossa does not provide prebuilt docker images due to the many different requirements that users have such as base operating systems and included components. If you would like a custom FOSSA CLI container tt is very easy to create one by adding the following line to a `Dockerfile` with the desired base image and components:

`RUN curl -H 'Cache-Control: no-cache' https://raw.githubusercontent.com/fossas/fossa-cli/master/install.sh | bash`