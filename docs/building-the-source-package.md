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

## What it does

1. Reads the version out of `setdown.cabal` and checks it against `debian/changelog`.
2. Downloads that version's real tarball from Hackage as the Debian orig tarball
   (`haskell-setdown_<version>.orig.tar.gz`) into a scratch `dist/source-build/` directory
   (gitignored).
3. Unpacks it and overlays this repository's `debian/` directory on top — the same layout the
   Debian Haskell Group's own tooling uses.
4. Builds the **unsigned** source package (`dpkg-buildpackage -S -us -uc`) inside a throwaway
   `debian:trixie` container, then runs `lintian --pedantic` against it.
5. Signs the resulting `.dsc` and `.changes` **on your host**, not inside the container —
   `gpg --clearsign`, in place, exactly what `debsign` does under the hood. This is the step
   that prompts you interactively.

## After it finishes

The signed files are in `dist/source-build/`:

```
haskell-setdown_<version>-1.dsc
haskell-setdown_<version>-1_source.changes
```

Uploading them is a separate, deliberately-manual step — see
[uploading-the-source-package.md](uploading-the-source-package.md) and
`scripts/upload-source-package.sh`.
