package main

import "k8s.io/client-go/kubernetes"

func main() {
	// This is here for import tracing.
	var _ kubernetes.Interface
}
