# Go buildinfo fixtures

Real buildinfo sections carved out of Go binaries, so the parser is tested
against bytes the Go linker actually emitted rather than only against
synthetic blobs encoded by the tests themselves.

## gh-2.86.0-darwin-arm64.bin

The inline (Go >= 1.18) buildinfo region of the GitHub CLI, `gh` version
2.86.0 (Homebrew bottle, darwin/arm64, built with go1.25.6; main module
versioned `(devel)`, 161 dep lines).

Carved by locating the 16-byte-aligned `\xff Go buildinf:` magic, then
copying from the magic through the end of the second inline varint string
(the modinfo). Equivalent to:

```python
data = open(path_to_gh, "rb").read()
off = data.find(b"\xff Go buildinf:")          # 16-byte aligned
assert data[off + 15] & 0x2                     # inline-strings flag

def uvarint(buf, i):
    v = s = 0
    while True:
        b = buf[i]; i += 1
        v |= (b & 0x7f) << s
        if not b & 0x80: return v, i
        s += 7

i = off + 32                                    # header size
vlen, i = uvarint(data, i); i += vlen           # go version string
mlen, i = uvarint(data, i); i += mlen           # modinfo string
open("gh-<version>-<os>-<arch>.bin", "wb").write(data[off:i])
```

The carved blob starts with the magic at offset 0 (which is 16-byte
aligned), so `parse_go_buildinfo` accepts it directly.
