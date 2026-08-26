#!/usr/bin/env bash
#
# Upload a signed Debian source package (built by build-source-package.sh) to
# mentors.debian.net for sponsor review.
#
# dput runs inside a throwaway Debian container - it isn't reliably
# installable on macOS, but it's a plain `apt install dput` on Debian, same
# as every other packaging tool this project uses in Docker.
#
# This is deliberately a separate script from build-source-package.sh: building
# is safe and repeatable; this is the one genuinely irreversible, identity-bound
# step - it posts your package for public review under your account.
#
# Run this yourself, in a real terminal: the container runs with -it, so
# dput can prompt normally and `--shell` gives you a real, usable shell.
#
# Usage:
#   scripts/upload-source-package.sh [path/to/*_source.changes]
#   scripts/upload-source-package.sh --shell
#
# --shell   Drop into an interactive shell in the same container and mounts,
#           instead of uploading. Use this to set up ~/.dput.cf yourself the
#           first time (see docs/uploading-the-source-package.md for the
#           config to add, or write your own), to inspect what would be
#           uploaded, or to run dput by hand.
#
# Your dput config persists across runs in ~/.config/setdown-dput on your
# host, mounted as the container's home directory - edit ~/.dput.cf there
# directly from your host editor, or from inside --shell, whichever you
# prefer. This script never writes to it itself.
#
# One-time setup this script cannot do for you:
#   1. Register an account at https://mentors.debian.net
#   2. Add your GPG public key to that account's profile - uploads signed
#      with a key mentors.debian.net doesn't know about are rejected outright.
#   3. Set up ~/.config/setdown-dput/.dput.cf yourself (run with --shell) -
#      see docs/uploading-the-source-package.md for the config to add.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
CONFIG_DIR="$HOME/.config/setdown-dput"
CONTAINER_GNUPGHOME="$CONFIG_DIR/.gnupg"

if ! command -v docker >/dev/null 2>&1; then
   echo "error: docker is required but was not found on PATH" >&2
   exit 1
fi
if ! command -v gpg >/dev/null 2>&1; then
   echo "error: gpg is required (on your host, to sync your public key into the" >&2
   echo "       container - see below) but was not found on PATH" >&2
   exit 1
fi

mkdir -p "$CONFIG_DIR"
mkdir -p "$REPO_ROOT/dist/source-build"

# dput verifies the .changes/.dsc signature itself before uploading, so the
# container needs your public key to check it against - not your private key,
# which never leaves your host. Re-syncing every run is cheap and keeps this
# working if you add or rotate keys later.
mkdir -p "$CONTAINER_GNUPGHOME"
chmod 700 "$CONTAINER_GNUPGHOME"
gpg --export | gpg --homedir "$CONTAINER_GNUPGHOME" --import --quiet 2>/dev/null || true

if [ "${1:-}" = "--shell" ]; then
   echo "==> Dropping into a shell with dput installed."
   echo "    Your config directory is mounted as home (persists at $CONFIG_DIR on your"
   echo "    host) - set up ~/.dput.cf here, or edit it directly on the host at"
   echo "    $CONFIG_DIR/.dput.cf. See docs/uploading-the-source-package.md for the"
   echo "    config to add."
   echo "    dist/source-build/ is mounted read-only at /upload, if you want to run"
   echo "    dput by hand."
   echo "    Ctrl-D to exit when you're done."
   echo
   docker run --rm -it \
      -v "$CONFIG_DIR:/root" \
      -v "$REPO_ROOT/dist/source-build:/upload:ro" \
      -w /root \
      debian:trixie \
      bash -c 'apt-get update -qq && apt-get install -y -qq dput gnupg nano vim-tiny && bash'
   exit 0
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

if grep -q '^Distribution: UNRELEASED$' "$CHANGES"; then
   echo "error: $CHANGES targets distribution UNRELEASED, which mentors.debian.net" >&2
   echo "       refuses to accept - that's a local-development placeholder, not a real" >&2
   echo "       upload target. Change the distribution in debian/changelog's latest" >&2
   echo "       entry (e.g. to 'unstable' for a new package's first upload), then" >&2
   echo "       rebuild with scripts/build-source-package.sh." >&2
   exit 1
fi

if [ ! -f "$CONFIG_DIR/.dput.cf" ] || ! grep -q '^\[mentors\]' "$CONFIG_DIR/.dput.cf"; then
   echo "error: no [mentors] stanza found in $CONFIG_DIR/.dput.cf" >&2
   echo "       Run '$0 --shell' to set it up yourself first - see" >&2
   echo "       docs/uploading-the-source-package.md for the config to add." >&2
   exit 1
fi

CHANGES_DIR="$(cd "$(dirname "$CHANGES")" && pwd)"
CHANGES_FILE="$(basename "$CHANGES")"

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

docker run --rm -it \
   -v "$CONFIG_DIR:/root" \
   -v "$CHANGES_DIR:/upload:ro" \
   -w /upload \
   debian:trixie \
   bash -c "apt-get update -qq && apt-get install -y -qq dput gnupg && dput mentors '$CHANGES_FILE'"
