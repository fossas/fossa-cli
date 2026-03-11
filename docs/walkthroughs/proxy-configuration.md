# Using FOSSA CLI with HTTP Proxies

FOSSA CLI supports HTTP proxies through standard environment variables.
This is useful in corporate environments where outbound connections must go through a proxy server.

## Configuring Proxy Environment Variables

FOSSA CLI uses the standard proxy environment variables recognized by most command-line tools:

```sh
# For HTTP connections
export http_proxy=http://proxy.example.com:8080

# For HTTPS connections (most common)
export https_proxy=http://proxy.example.com:8080

# Hosts to exclude from proxy (comma-separated)
export no_proxy=localhost,127.0.0.1,internal.example.com
```

For proxy servers that require authentication:

```sh
export https_proxy=http://username:password@proxy.example.com:8080
```

## Verifying Proxy Configuration Locally

Usually, you can test proxy configuration by simply configuring the proxy with the settings required by your organization;
if it works then you're done. But if you need to verify it locally you can do so with any standard proxy server.

Below is a walkthrough on how to verify using the `squid` proxy server:

1. Install Squid:
   - Ubuntu/Debian: `sudo apt-get install squid`
   - RHEL/CentOS: `sudo yum install squid`
   - macOS: `brew install squid`

2. Create a minimal configuration at `/tmp/squid.conf`:

```
http_port 3128
http_access allow all
```

3. Run Squid:

> [!NOTE]
> If you don't know where your installation of `squid` stores its access log,
> run it with `-d 3` to have `squid` log its access log location.

```sh
squid -f /tmp/squid.conf -N
```

4. Run `fossa`:

```sh
fossa analyze
```

5. Check the access log:

> [!NOTE]
> This example demonstrates the standard location for `homebrew`-installed `squid`;
> your location may be different.

```sh
; cat /opt/homebrew/var/logs/access.log
1747163889.439    486 ::1 TCP_TUNNEL/200 5934 CONNECT app.fossa.com:443 - HIER_DIRECT/35.71.190.183 -
1747163889.695    254 ::1 TCP_TUNNEL/200 5505 CONNECT app.fossa.com:443 - HIER_DIRECT/35.71.190.183 -
1747163889.887    231 ::1 TCP_TUNNEL/200 5505 CONNECT app.fossa.com:443 - HIER_DIRECT/35.71.190.183 -
1747163892.236   1232 ::1 TCP_TUNNEL/200 18550 CONNECT app.fossa.com:443 - HIER_DIRECT/35.71.190.183 -
1747163892.495   1210 ::1 TCP_TUNNEL/200 6597 CONNECT s3.us-east-1.amazonaws.com:443 - HIER_DIRECT/54.231.201.248 -
1747163892.496   1616 ::1 TCP_TUNNEL/200 5762 CONNECT app.fossa.com:443 - HIER_DIRECT/35.71.190.183 -
1747163892.591    400 ::1 TCP_TUNNEL/200 4889 CONNECT app.fossa.com:443 - HIER_DIRECT/35.71.190.183 -
1747163898.487    670 ::1 TCP_TUNNEL/200 5934 CONNECT app.fossa.com:443 - HIER_DIRECT/35.71.190.183 -
1747163898.651    301 ::1 TCP_TUNNEL/200 5505 CONNECT app.fossa.com:443 - HIER_DIRECT/35.71.190.183 -
1747163898.806    208 ::1 TCP_TUNNEL/200 5505 CONNECT app.fossa.com:443 - HIER_DIRECT/35.71.190.183 -
1747163899.158    217 ::1 TCP_TUNNEL/200 5762 CONNECT app.fossa.com:443 - HIER_DIRECT/35.71.190.183 -
1747163900.966   1770 ::1 TCP_TUNNEL/200 18386 CONNECT app.fossa.com:443 - HIER_DIRECT/35.71.190.183 -
1747163901.012   1462 ::1 TCP_TUNNEL/200 6597 CONNECT s3.us-east-1.amazonaws.com:443 - HIER_DIRECT/54.231.201.248 -
1747164273.479    214 ::1 TCP_TUNNEL/200 5934 CONNECT app.fossa.com:443 - HIER_DIRECT/52.223.16.182 -
1747164273.588    134 ::1 TCP_TUNNEL/200 5505 CONNECT app.fossa.com:443 - HIER_DIRECT/52.223.16.182 -
1747164273.812    258 ::1 TCP_TUNNEL/200 5505 CONNECT app.fossa.com:443 - HIER_DIRECT/52.223.16.182 -
1747164273.969    104 ::1 TCP_TUNNEL/200 5762 CONNECT app.fossa.com:443 - HIER_DIRECT/52.223.16.182 -
1747164276.816    419 ::1 TCP_TUNNEL/200 4889 CONNECT app.fossa.com:443 - HIER_DIRECT/52.223.16.182 -
1747164276.816   2872 ::1 TCP_TUNNEL/200 18341 CONNECT app.fossa.com:443 - HIER_DIRECT/52.223.16.182 -
1747164276.816   2652 ::1 TCP_TUNNEL/200 6597 CONNECT s3.us-east-1.amazonaws.com:443 - HIER_DIRECT/52.217.231.104 -
```

## Common Proxy Issues

### Invalid Proxy URL Format

Ensure your proxy URLs have the correct format:

- Valid: `http://proxy.example.com:8080`
- Valid: `http://username:password@proxy.example.com:8080`
- Invalid: `proxy.example.com:8080` (missing protocol)

### TLS Certificate Issues

If your proxy server performs TLS inspection, you may need to configure FOSSA CLI to trust your organization's certificate authority.
See the [SSL/TLS Configuration](./ssl-cert.md) walkthrough.

### Network Connectivity

If you encounter connection timeouts, ensure that your proxy server allows connections to your instance of FOSSA
and the object store that your instance of FOSSA uses.

If you use the instance of FOSSA at `https://app.fossa.com`, those settings are:
- `app.fossa.com`
- `analysis.fossa.com`
- `s3.us-east-1.amazonaws.com`

Other instances, especially on-premise instances, may have different settings-
please work with your IT department and/or FOSSA support in these cases to determine the correct values.

### Corporate Proxy Challenges

Some corporate proxies may present additional challenges:

1. Content filtering - Some proxies inspect and filter certain types of content
2. Authentication - Proxies may require periodic re-authentication
3. Bandwidth limitations - Upload of large scan results may be throttled

If you encounter persistent issues with your proxy, contact your IT department for assistance.
