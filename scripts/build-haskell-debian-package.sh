#!/bin/bash

# To do before running this:
#
# 1. In aeson-jsonpath.cabal, remove the `data-file: ...` line.
# 2. Run this shell scripts in the parent directory of the package.

# I have no idea what's happening in this script, but it creates the
# libghc-aeson-jsonpath-dev package and that's the goal. I should try
# to understand the code here when I get time.

set -e

cd aeson-jsonpath

cabal-debian --official

echo "=== Cleaning build artifacts ==="
cabal clean
rm -rf dist-newstyle/ dist/ .stack-work/

echo "=== Getting version ==="
VERSION=$(grep -m1 "^version:" *.cabal | awk '{print $2}' | tr -d ' ')
echo "Version: ${VERSION}"

echo "=== Creating orig tarball ==="
cd ..
rm -f haskell-aeson-jsonpath_${VERSION}.orig.tar.gz

tar czf haskell-aeson-jsonpath_${VERSION}.orig.tar.gz \
    --exclude-vcs \
    --exclude=.git \
    --exclude=debian \
    --exclude='*.swp' \
    --exclude='dist*' \
    --exclude='.stack-work' \
    --exclude='jsonpath-compliance-test-suite' \
    --exclude='scripts' \
    --exclude='cabal.project.local' \
    --transform "s,^aeson-jsonpath,haskell-aeson-jsonpath-${VERSION}," \
    aeson-jsonpath/

cd aeson-jsonpath

echo "=== Setting up debian/rules ==="
cat > debian/rules << 'EOF'
#!/usr/bin/make -f

# Set HOME to writable location
export HOME = $(CURDIR)/debian/build-home

# Disable cabal from trying to access remote repos
export CABAL_CONFIG = /dev/null

include /usr/share/cdbs/1/rules/debhelper.mk
include /usr/share/cdbs/1/class/hlibrary.mk
EOF

chmod +x debian/rules

echo "=== Creating source options ==="
mkdir -p debian/source
cat > debian/source/options << 'EOF'
extend-diff-ignore = "^dist-newstyle/"
extend-diff-ignore = "^dist/"
extend-diff-ignore = "^\.stack-work/"
EOF

echo "=== Building Debian package ==="
dpkg-buildpackage -us -uc

echo "=== Done ==="
echo ""
echo "Packages created:"
ls -lh ../*.deb
