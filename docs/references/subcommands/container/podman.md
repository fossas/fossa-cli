## Experimental Scanner - Podman

`fossa-cli` can use podman client to perform container image scanning analysis. 

# Integration via Podman's Docker Compatible API

`fossa-cli` will look for environment variable `DOCKER_HOST`,
to infer docker engine api's socket location. As of now, `fossa-cli`
only works with `unix://` socket. 

For podman, you can use `podman machine start` command, to retrieve
Docker client compatible `DOCKER_HOST`.

```bash
➜ podman machine start
Starting machine "podman-machine-default"
Waiting for VM ...
Mounting volume... /Users/fossa:/Users/fossa

This machine is currently configured in rootless mode. If your containers
require root permissions (e.g. ports < 1024), or if you run into compatibility
issues with non-podman clients, you can switch using the following command: 

	podman machine set --rootful

API forwarding listening on: /Users/fossa/.local/share/containers/podman/machine/podman-machine-default/podman.sock

The system helper service is not installed; the default Docker API socket
address can\'t be used by podman. If you would like to install it run the
following commands:

	sudo /usr/local/Cellar/podman/4.2.0/bin/podman-mac-helper install
	podman machine stop; podman machine start

You can still connect Docker API clients by setting DOCKER_HOST using the
following command in your terminal session:

	export DOCKER_HOST='unix:///Users/fossa/.local/share/containers/podman/machine/podman-machine-default/podman.sock'

Machine "podman-machine-default" started successfully
```

Now we can specify `DOCKER_HOST` when running `fossa-cli`. 

```bash
DOCKER_HOST='unix:///Users/fossa/.local/share/containers/podman/machine/podman-machine-default/podman.sock' fossa container analyze
```

Likewise, if you are using `podman-remote`, you should be able to use generate unix socket, and use it with `fossa-cli`. Refer to documentation below for more details.

Refer to documentation here:
- https://podman.io/blogs/2020/07/01/rest-versioning.html
- https://docs.podman.io/en/latest/_static/api.html
- https://access.redhat.com/documentation/en-us/red_hat_enterprise_linux/8/html/building_running_and_managing_containers/assembly_using-the-container-tools-api_building-running-and-managing-containers


# Integration via Podman Executables

`fossa-cli` will perform the following command:

```bash
# check if image exists
podman image inspect <ARG>

# export said image to temporary location, 
# and perform analysis on exported image.
podman save --format docker-archive -o <temp-path>
```
`fossa-cli` will look for `podman` executable in `$PATH`.

> Note this is same approach as integration via Docker Archive except that
> `fossa-cli` performs necessary invocation and does cleanup of artifacts.

# Integration via Docker Archive

We can manually export image using podman, and analyze such image
with `fossa-cli`.  

```bash
podman build . -t someImg:1.0.0
podman save --format docker-archive -o image.tar

fossa container analyze image.tar

# Cleanup
rm image.tar
```
