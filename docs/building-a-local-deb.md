# Building a local `.deb`

This gives you a real, installable Debian/Ubuntu package containing the `setdown` command —
useful for trying out a change before it's released, or for installing setdown on a
Debian/Ubuntu machine without waiting on the official Debian archive (see
[`ai-planning/14-debian-ubuntu-packaging.md`](../ai-planning/14-debian-ubuntu-packaging.md) for
where that process stands).

This is for **local testing only**. It is not the same thing as the signed source package that
eventually gets uploaded to Debian for sponsor review — that process needs your GPG key and a
`mentors.debian.net` account, so it can't be a one-step script.

## No GPG signing here, on purpose

The script builds with `dpkg-buildpackage -us -uc -b`: **u**nsigned **s**ource, **u**nsigned
**c**hanges, **b**inary-only. GPG signatures are an *archive* requirement — Debian uses them to
verify who's asking to upload a package into the official archive, checked on the `.dsc`/
`.changes` files of a *source* upload. A local `.deb` you `apt install` and test yourself has no
such check, so there's nothing to sign here.

It also couldn't work inside this script even if it mattered: signing needs your GPG passphrase
typed interactively into `pinentry`, which isn't available inside a non-interactive
`docker run`. That's why signing the actual archive-upload source package is a separate, manual
step you run yourself in a real terminal — not something this script, or any script, should try
to automate.

## Prerequisites

Just [Docker](https://www.docker.com). Nothing else needs to be installed on your machine.

Why Docker and not a plain local build: the actual Debian packaging tools
(`cabal-debian`, `dpkg-buildpackage`) shell out to `dpkg-query` and other Debian-specific
programs, so they can't run natively on macOS. The script builds inside a throwaway Debian
container instead, so the result is exactly what a real Debian system would produce.

## Quick start

```shell
./scripts/build-deb.sh
```

The first run downloads a Debian image and the Haskell toolchain, so it takes a few minutes;
later runs are faster since Docker caches the image layers. When it finishes, you'll have:

```
dist/setdown_<version>-1_<arch>.deb
```

## What the script does

1. Starts a throwaway `debian:trixie` container with this repository mounted read-write.
2. Installs the Debian Haskell Group's packaging toolchain inside it (`ghc`,
   `haskell-devscripts`, `debhelper`, `cdbs`, `lintian`, and every `libghc-*-dev` package
   `debian/control` declares as a build dependency).
3. Runs `dpkg-buildpackage -us -uc -b` — a real, unsigned, binary-only build from the
   `debian/` directory already checked into this repo.
4. Runs `lintian --pedantic` against the result and prints whatever it finds (informational —
   the script doesn't fail the build over lint warnings).
5. Moves the built `.deb` into `dist/` (gitignored) and cleans up every build artifact
   `dpkg-buildpackage` left in the source tree (`dist-ghc/`, `debian/.debhelper/`, etc.), so
   your working tree stays clean.

## Installing and trying it

The package declares real runtime dependencies (GHC's runtime C libraries — `libffi8`,
`libgmp10`, `libnuma1`, `libc6`), so install it with `apt`, not a bare `dpkg -i` — `apt`
resolves those automatically, `dpkg -i` will just fail with unmet dependencies.

In a throwaway container:

```shell
docker run --rm -it -v "$PWD/dist:/pkgs" debian:trixie \
  bash -c 'apt-get update -qq && apt install -y /pkgs/setdown_*.deb && setdown --help'
```

On a real Debian/Ubuntu machine, copy `dist/setdown_*.deb` over and run:

```shell
sudo apt install ./setdown_*.deb
```

## Architecture

The script builds for whatever architecture Docker defaults to on your machine (e.g. `arm64`
on Apple Silicon). To build for a different one, set the platform explicitly before running it,
e.g.:

```shell
DOCKER_DEFAULT_PLATFORM=linux/amd64 ./scripts/build-deb.sh
```
