# Building the signed Debian source package

This produces the actual `.dsc`/`.changes` Debian expects for an archive upload — the artifact
that goes to `mentors.debian.net` for sponsor review (see
[`ai-planning/14-debian-ubuntu-packaging.md`](../ai-planning/14-debian-ubuntu-packaging.md)).
This is different from [building a local `.deb`](building-a-local-deb.md): that one is an
unsigned package for trying setdown out yourself; this one is signed, and is the thing that
actually moves setdown towards being in Debian.

## Run this yourself, in a real terminal

```shell
./scripts/build-source-package.sh
```

Do not run this through anything that isn't your own interactive terminal. The signing step
needs to prompt you for your GPG passphrase via `pinentry`, which only works with a real
terminal attached.

Pass `-k KEYID` to sign with a specific key (see `gpg --list-secret-keys`); it defaults to your
GPG default signing key otherwise.

## Prerequisites

- [Docker](https://www.docker.com), for the actual Debian build (see
  [building-a-local-deb.md](building-a-local-deb.md) for why this can't run natively on macOS).
- `gpg`, with a signing key already set up.
- The version in `setdown.cabal` must already be **published on Hackage**
  (`cabal upload` / `stack upload`). Debian's orig tarball has to be a real upstream release —
  the script fetches it directly from Hackage rather than reconstructing it from the working
  tree, and it will fail with a clear error if that version hasn't been published yet.
- `debian/changelog`'s latest entry must already reference the same version as
  `setdown.cabal` — the script checks this and fails fast if they've drifted apart, rather than
  building something inconsistent.
- For an actual upload (not just a local test build), `debian/changelog`'s latest entry needs a
  real target distribution, not `UNRELEASED` — see "Before a real upload" below.

## What it does

1. Reads the version out of `setdown.cabal` and checks it against `debian/changelog`.
2. Downloads that version's real tarball from Hackage as the Debian orig tarball
   (`haskell-setdown_<version>.orig.tar.gz`) into a scratch `dist/source-build/` directory
   (gitignored).
3. Unpacks it and overlays this repository's `debian/` directory on top — the same layout the
   Debian Haskell Group's own tooling uses.
4. Builds the **unsigned** `.dsc` (`dpkg-source -b .`) inside a throwaway `debian:trixie`
   container.
5. Signs the `.dsc` **on your host**, not inside the container — `gpg --clearsign`, in place.
   This is the step that prompts you interactively.
6. Generates `.changes` (`dpkg-genchanges`) against the now-*signed* `.dsc`, in a fresh
   container, then runs `lintian --pedantic` against it.
7. Signs `.changes` on your host the same way.

Steps 4–7 are deliberately split around the signing step, rather than building both files and
signing them afterwards. A `.changes` file embeds a checksum *of the `.dsc` file itself*,
computed at the moment `.changes` is generated — signing the `.dsc` changes its bytes (adds the
PGP armor), so if `.changes` were generated first and the `.dsc` signed afterwards, that
embedded checksum goes stale and `dput` refuses the upload outright with a
"Checksum doesn't match" error. Signing the `.dsc` before generating `.changes` is how
`dpkg-buildpackage` avoids this itself when it does the signing in one pass; this reproduces
that order by hand since signing has to happen on the host, not inside the build container.

## Before a real upload

`debian/changelog`'s latest entry should currently say `UNRELEASED` as its distribution — that's
the correct state while iterating locally, and `upload-source-package.sh` will refuse to upload
a package built against it (mentors.debian.net rejects `UNRELEASED` outright; it's a
local-development placeholder, not a real upload target). Before your first real upload, change
it to the real target — `unstable`, for a new package's first upload — and rebuild.

## After it finishes

The signed files are in `dist/source-build/`:

```
haskell-setdown_<version>-1.dsc
haskell-setdown_<version>-1_source.changes
```

Uploading them is a separate, deliberately-manual step — see
[uploading-the-source-package.md](uploading-the-source-package.md) and
`scripts/upload-source-package.sh`.
