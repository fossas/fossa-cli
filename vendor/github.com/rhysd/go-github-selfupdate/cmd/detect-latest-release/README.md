This command line tool is a small wrapper of [`selfupdate.DetectLatest()`](https://godoc.org/github.com/rhysd/go-github-selfupdate/selfupdate#DetectLatest).

Please install using `go get`.

```
$ go get -u github.com/rhysd/go-github-selfupdate/cmd/detect-latest-release
```

To know the usage, please try the command without any argument.

```
$ detect-latest-release
```

For example, following shows the latest version of [github-clone-all](https://github.com/rhysd/github-clone-all).

```
$ detect-latest-release rhysd/github-clone-all
```

