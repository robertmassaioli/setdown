#!/usr/bin/env bash
#
# Build a local, installable .deb of the setdown command.
#
# Debian's packaging tools (cabal-debian, dpkg-buildpackage) shell out to
# dpkg-query and expect a real Debian/Ubuntu system, so this can't run
# natively on macOS - it builds inside a throwaway Debian container instead.
# See docs/building-a-local-deb.md for the full explanation.
#
# Usage: scripts/build-deb.sh
# Output: dist/setdown_<version>-1_<arch>.deb

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

if ! command -v docker >/dev/null 2>&1; then
   echo "error: docker is required but was not found on PATH" >&2
   exit 1
fi

echo "==> Building setdown .deb in a Debian trixie container (the first run downloads" \
     "the Haskell toolchain and can take a few minutes; later runs are faster)..."

docker run --rm \
   -v "$REPO_ROOT:/build/repo" \
   -w /build/repo \
   debian:trixie \
   bash -eu -o pipefail -c '
      export DEBIAN_FRONTEND=noninteractive
      apt-get update -qq
      apt-get install -y -qq \
         ghc haskell-devscripts debhelper cdbs lintian devscripts \
         dpkg-dev build-essential alex happy \
         libghc-cmdargs-dev libghc-uuid-dev libghc-split-dev libghc-async-dev \
         libghc-quickcheck2-dev libghc-tasty-dev libghc-tasty-golden-dev \
         libghc-tasty-hunit-dev libghc-tasty-quickcheck-dev

      dpkg-buildpackage -us -uc -b

      echo "==> lintian:"
      lintian --pedantic ../setdown_*.deb || true

      mkdir -p dist
      mv ../setdown_*.deb dist/

      echo "==> Cleaning up build artifacts left in the source tree..."
      rm -rf debian/.debhelper debian/files debian/setdown.substvars debian/setdown \
         debian/hlibrary.setup Setup.hi Setup.o build-ghc-stamp configure-ghc-stamp dist-ghc
      rm -f ../haskell-setdown_*.buildinfo ../haskell-setdown_*.changes
   '

echo
echo "==> Done. Built:"
ls -1 "$REPO_ROOT"/dist/setdown_*.deb
echo
echo "Install and try it, e.g. in a throwaway container (apt resolves its runtime"
echo "dependencies; a bare 'dpkg -i' will not):"
echo "  docker run --rm -it -v \"$REPO_ROOT/dist:/pkgs\" debian:trixie \\"
echo "    bash -c 'apt-get update -qq && apt install -y /pkgs/setdown_*.deb && setdown --help'"
