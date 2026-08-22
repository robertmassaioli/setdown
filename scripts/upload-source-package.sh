#!/usr/bin/env bash
#
# Upload a signed Debian source package (built by build-source-package.sh) to
# mentors.debian.net for sponsor review.
#
# This is deliberately a separate script from build-source-package.sh: building
# is safe and repeatable; this is the one genuinely irreversible, identity-bound
# step - it posts your package for public review under your account. Run it
# yourself, and expect to be asked to confirm before anything is sent.
#
# Usage:
#   scripts/upload-source-package.sh [path/to/*_source.changes]
#
# If no path is given, uses the newest *_source.changes in dist/source-build/
# (i.e. whatever build-source-package.sh most recently produced).
#
# One-time setup this script cannot do for you:
#   1. Register an account at https://mentors.debian.net
#   2. Add your GPG public key to that account's profile - uploads signed
#      with a key mentors.debian.net doesn't know about are rejected outright.
#   3. Install dput (see docs/uploading-the-source-package.md - it isn't
#      packaged for macOS via Homebrew or MacPorts, so this needs a manual
#      pipx install).
#   4. Add a [mentors] stanza to ~/.dput.cf (also in that doc).

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

if ! command -v dput >/dev/null 2>&1; then
   echo "error: dput is required but was not found on PATH." >&2
   echo "       See docs/uploading-the-source-package.md for how to install it on macOS." >&2
   exit 1
fi

if ! grep -q '^\[mentors\]' "$HOME/.dput.cf" 2>/dev/null; then
   echo "error: no [mentors] stanza found in ~/.dput.cf" >&2
   echo "       See docs/uploading-the-source-package.md for the config to add." >&2
   exit 1
fi

CHANGES="${1:-}"
if [ -z "$CHANGES" ]; then
   CHANGES="$(ls -t "$REPO_ROOT"/dist/source-build/*_source.changes 2>/dev/null | head -n1 || true)"
   if [ -z "$CHANGES" ]; then
      echo "error: no *_source.changes file given, and none found in dist/source-build/." >&2
      echo "       Run scripts/build-source-package.sh first." >&2
      exit 1
   fi
fi

if [ ! -f "$CHANGES" ]; then
   echo "error: $CHANGES does not exist" >&2
   exit 1
fi

if ! head -n1 "$CHANGES" | grep -q '^-----BEGIN PGP SIGNED MESSAGE-----'; then
   echo "error: $CHANGES does not look like it's been GPG-signed." >&2
   echo "       Run scripts/build-source-package.sh, which signs it as its last step." >&2
   exit 1
fi

echo "==> About to upload to mentors.debian.net:"
echo "      $CHANGES"
echo
echo "    This posts the package publicly for sponsor review under your account -"
echo "    it is not a private or easily-reversible action."
echo
read -r -p "Continue? [y/N] " CONFIRM
case "$CONFIRM" in
   [yY]|[yY][eE][sS]) ;;
   *) echo "Aborted."; exit 1 ;;
esac

dput mentors "$CHANGES"
