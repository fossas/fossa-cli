package golang

import "github.com/fossas/fossa-cli/analyzers/golang/resolver"

// ResolverFromLockfile loads and caches lockfile resolvers.
func (a *Analyzer) ResolverFromLockfile(tool resolver.Type, dir string) (resolver.Resolver, error) {
	// Look up resolver in the cache.
	key := string(tool) + "__fossa-cli-golang-resolver-cache-key__" + dir
	cached, ok := a.resolverCache[key]
	if ok {
		return cached, nil
	}

	// If not found, construct a new resolver.
	r, err := resolver.FromLockfile(tool, dir)
	if err != nil {
		return nil, err
	}

	// Cache the resolver.
	a.resolverCache[key] = r
	return r, nil
}
