## Release

This document contains how to make a release for this package.

### Hackage

```
$ cabal v2-update  # update cabal hackage index
```

```
$ cabal outdated # for outdated dependencies
```

Then remove the `-Werror` flag from the `.cabal` file. This is required by hackage.

```
$ cabal sdist  # This creates the tarball which you upload manually on hackage
```
