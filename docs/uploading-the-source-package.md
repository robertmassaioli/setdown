# Uploading the source package to mentors.debian.net

This is the last step: sending the signed source package (from
[building-the-source-package.md](building-the-source-package.md)) to `mentors.debian.net` for
sponsor review. It's a separate script from the build, deliberately — building is safe and
repeatable; this is the one genuinely irreversible, identity-bound step, since it posts your
package publicly under your account.

```shell
./scripts/upload-source-package.sh
```

With no argument, it uploads whatever `build-source-package.sh` most recently produced (the
newest `*_source.changes` in `dist/source-build/`). Pass a path explicitly to upload something
else. Either way, it checks the file actually looks GPG-signed before doing anything, and asks
you to confirm before it uploads.

## One-time setup

None of this can be scripted — it's tied to your own account and credentials.

### 1. Register on mentors.debian.net and add your GPG key

Create an account at [mentors.debian.net](https://mentors.debian.net), then add your GPG
public key to your profile there. This matters more than it might sound: mentors.debian.net
rejects unsigned uploads outright, and it authenticates *who* is uploading entirely by checking
the signature on the `.changes` file against the keys registered to accounts — there's no
separate username/password login for the upload itself. Make sure it's the same key you sign
with in `build-source-package.sh` (`gpg --list-secret-keys` to check which one that is).

### 2. Install `dput`

`dput` isn't packaged for macOS — no Homebrew formula, no MacPorts port. The current upstream
(`dput-ng`) is maintained on [Salsa](https://salsa.debian.org/debian/dput-ng); the plain PyPI
`dput` package is a stale 2014 release, so install straight from source instead:

```shell
pip install pipx   # if you don't already have it
pipx install "git+https://salsa.debian.org/debian/dput-ng.git"
pipx inject dput-ng python-debian paramiko validictory
```

If that proves fiddly (Python packaging on macOS can be), a Debian/Ubuntu VM or a Docker
container with `dput` installed via `apt` is a reliable fallback for just this one command —
unlike the build steps, this isn't something worth maintaining a permanent Dockerized script
for, since it's a single one-time-per-release upload, not a repeated build.

### 3. Configure `~/.dput.cf`

Add this stanza (current as of mentors.debian.net's own documentation — it moved to HTTPS
uploads a while back, not the older FTP-only flow):

```ini
[mentors]
fqdn = mentors.debian.net
incoming = /upload
method = https
allow_unsigned_uploads = 0
progress_indicator = 2
allowed_distributions = .*
```

## What the script checks before uploading

1. `dput` is on `PATH`.
2. `~/.dput.cf` has a `[mentors]` stanza.
3. A `.changes` file exists — either the one you passed, or the newest one in
   `dist/source-build/`.
4. That file starts with `-----BEGIN PGP SIGNED MESSAGE-----` — catches the mistake of
   uploading something `build-source-package.sh` hasn't actually signed yet.

Then it prints exactly what it's about to do and asks you to confirm before running
`dput mentors <file>`.
