#!/usr/bin/env bash
#
# Build and sign the Debian *source* package for setdown, ready to upload to
# mentors.debian.net for sponsor review.
#
# Unlike scripts/build-deb.sh (a local, unsigned test .deb), this produces
# the actual signed .dsc/.changes Debian expects for an archive upload. Run
# this yourself in a real terminal: the signing step needs to prompt you for
# your GPG passphrase interactively, which only works with a real terminal
# attached - it will not work piped through another program or run
# non-interactively.
#
# Usage:
#   scripts/build-source-package.sh [-k KEYID]
#
#   -k KEYID   GPG key to sign with (see `gpg --list-secret-keys`).
#              Defaults to your gpg default signing key if omitted.
#
# Requires: docker (for the actual Debian build - see docs/building-a-local-deb.md
# for why), gpg (for signing, run on your host, not inside the container, so
# it can prompt you normally), and the version in setdown.cabal must already
# be published on Hackage - the Debian orig tarball has to be a real
# upstream release, not a reconstruction of the working tree.

set -euo pipefail

KEY=""
while getopts "k:" opt; do
   case "$opt" in
      k) KEY="$OPTARG" ;;
      *) echo "Usage: $0 [-k KEYID]" >&2; exit 1 ;;
   esac
done

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

if ! command -v docker >/dev/null 2>&1; then
   echo "error: docker is required but was not found on PATH" >&2
   exit 1
fi
if ! command -v gpg >/dev/null 2>&1; then
   echo "error: gpg is required but was not found on PATH" >&2
   exit 1
fi

VERSION="$(sed -n 's/^version: *//p' "$REPO_ROOT/setdown.cabal" | tr -d '[:space:]')"
if [ -z "$VERSION" ]; then
   echo "error: could not read a version out of setdown.cabal" >&2
   exit 1
fi

CHANGELOG_VERSION="$(sed -n '1s/^haskell-setdown (\([^)]*\)-[0-9]*).*/\1/p' "$REPO_ROOT/debian/changelog")"
if [ "$CHANGELOG_VERSION" != "$VERSION" ]; then
   echo "error: setdown.cabal is version $VERSION but debian/changelog's latest entry is" \
        "for $CHANGELOG_VERSION - update debian/changelog first." >&2
   exit 1
fi

echo "==> Building the Debian source package for setdown $VERSION"

BUILD_DIR="$REPO_ROOT/dist/source-build"
rm -rf "$BUILD_DIR"
mkdir -p "$BUILD_DIR"

ORIG_TARBALL="$BUILD_DIR/haskell-setdown_${VERSION}.orig.tar.gz"
HACKAGE_URL="https://hackage.haskell.org/package/setdown-${VERSION}/setdown-${VERSION}.tar.gz"

echo "==> Fetching the published release from Hackage: $HACKAGE_URL"
if ! curl -fsSL -o "$ORIG_TARBALL" "$HACKAGE_URL"; then
   echo "error: could not download $HACKAGE_URL" >&2
   echo "       Has version $VERSION actually been published to Hackage yet?" >&2
   echo "       (cabal upload / stack upload - the Debian orig tarball has to be a real" >&2
   echo "       upstream release, not a reconstruction of the working tree.)" >&2
   exit 1
fi

SRC_DIR="$BUILD_DIR/haskell-setdown-${VERSION}"
tar xzf "$ORIG_TARBALL" -C "$BUILD_DIR"
mv "$BUILD_DIR/setdown-${VERSION}" "$SRC_DIR"
cp -R "$REPO_ROOT/debian" "$SRC_DIR/debian"

TOOLCHAIN='ghc haskell-devscripts debhelper cdbs lintian devscripts dpkg-dev build-essential \
   alex happy libghc-cmdargs-dev libghc-uuid-dev libghc-split-dev libghc-async-dev \
   libghc-quickcheck2-dev libghc-tasty-dev libghc-tasty-golden-dev libghc-tasty-hunit-dev \
   libghc-tasty-quickcheck-dev'

# Building the .dsc and building the .changes are split into two separate
# container runs, with the .dsc signed on the host in between - on purpose.
# A .changes file embeds a checksum *of the .dsc itself*, computed at the
# moment .changes is generated. Signing the .dsc wraps it in PGP armor,
# which changes its bytes - so if .changes were generated first and the
# .dsc signed afterwards (the obvious, simpler order, and the one this
# script used to use), .changes' own checksum for the .dsc goes stale and
# dput refuses the upload outright ("Checksum doesn't match"). Signing
# first, then generating .changes from the already-signed .dsc, is how the
# real dpkg-buildpackage avoids this when it does the signing itself; this
# reproduces that order by hand since signing needs to happen on the host.

echo "==> Building the unsigned .dsc in a Debian trixie container..."
docker run --rm \
   -v "$BUILD_DIR:/build" \
   -w "/build/haskell-setdown-${VERSION}" \
   debian:trixie \
   bash -eu -o pipefail -c "
      export DEBIAN_FRONTEND=noninteractive
      apt-get update -qq
      apt-get install -y -qq $TOOLCHAIN

      dpkg-source --before-build .
      debian/rules clean
      dpkg-source -b .
   "

DSC="$BUILD_DIR/haskell-setdown_${VERSION}-1.dsc"
CHANGES="$BUILD_DIR/haskell-setdown_${VERSION}-1_source.changes"

echo
echo "==> Unsigned .dsc built: $DSC"
echo
echo "==> Signing the .dsc with GPG - you may be prompted for your passphrase now."

GPG_ARGS=()
if [ -n "$KEY" ]; then
   GPG_ARGS+=(--local-user "$KEY")
fi

gpg "${GPG_ARGS[@]}" --clearsign --output "$DSC.signed" "$DSC"
mv "$DSC.signed" "$DSC"

echo
echo "==> Generating .changes against the signed .dsc, in a fresh container..."
docker run --rm \
   -v "$BUILD_DIR:/build" \
   -w "/build/haskell-setdown-${VERSION}" \
   debian:trixie \
   bash -eu -o pipefail -c "
      export DEBIAN_FRONTEND=noninteractive
      apt-get update -qq
      apt-get install -y -qq $TOOLCHAIN

      dpkg-genbuildinfo --build=source -O../haskell-setdown_${VERSION}-1_source.buildinfo
      dpkg-genchanges --build=source -O../haskell-setdown_${VERSION}-1_source.changes
      dpkg-source --after-build .

      echo '==> lintian:'
      lintian --pedantic ../*_source.changes || true
   "

echo
echo "==> Signing the .changes with GPG - you may be prompted for your passphrase again."

gpg "${GPG_ARGS[@]}" --clearsign --output "$CHANGES.signed" "$CHANGES"
mv "$CHANGES.signed" "$CHANGES"

echo
echo "==> Done. Signed and ready to upload:"
echo "  $DSC"
echo "  $CHANGES"
echo
echo "Upload with (see docs/uploading-the-source-package.md for one-time setup):"
echo "  ./scripts/upload-source-package.sh"
