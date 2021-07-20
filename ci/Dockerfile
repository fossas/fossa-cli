# Build the correct binaries
FROM fossa/haskell-static-alpine:ghc-8.10.4 as builder
RUN cabal update
RUN cabal install hlint
RUN cabal install fourmolu
# Replace symlinks with their actual files so we can slim down the image
RUN cp $(readlink -f $(which hlint)) /root/.cabal/bin/hlint
RUN cp $(readlink -f $(which fourmolu)) /root/.cabal/bin/fourmolu

# Copy the binaries into a smaller image
FROM fossa/haskell-static-alpine:ghc-8.10.4 as final
COPY --from=builder /root/.cabal/bin/hlint /root/.cabal/bin/hlint
COPY --from=builder /root/.cabal/bin/fourmolu /root/.cabal/bin/fourmolu