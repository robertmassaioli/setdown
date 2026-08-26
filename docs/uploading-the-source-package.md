# Uploading the source package to mentors.debian.net

This is the last step: sending the signed source package (from
[building-the-source-package.md](building-the-source-package.md)) to `mentors.debian.net` for
sponsor review. It's a separate script from the build, deliberately — building is safe and
repeatable; this is the one genuinely irreversible, identity-bound step, since it posts your
package publicly under your account.

`dput` runs inside a throwaway Debian container, the same way the rest of this project's
packaging tooling does — it isn't reliably installable on macOS, but it's a plain
`apt install dput` on Debian.

```shell
./scripts/upload-source-package.sh
```

With no argument, it uploads whatever `build-source-package.sh` most recently produced (the
newest `*_source.changes` in `dist/source-build/`). Pass a path explicitly to upload something
else. Either way, it checks the file actually looks GPG-signed before doing anything, and asks
you to confirm before it uploads. Run it yourself, in a real terminal — the container runs
interactively so `dput` can prompt you normally.

## One-time setup

None of this can be scripted — it's tied to your own account and credentials.

### 1. Register on mentors.debian.net and add your GPG key

Create an account at [mentors.debian.net](https://mentors.debian.net), then add your GPG
public key to your profile there. This matters more than it might sound: mentors.debian.net
rejects unsigned uploads outright, and it authenticates *who* is uploading entirely by checking
the signature on the `.changes` file against the keys registered to accounts — there's no
separate username/password login for the upload itself. Make sure it's the same key you sign
with in `build-source-package.sh` (`gpg --list-secret-keys` to check which one that is).

### 2. Set up your `dput` config yourself

```shell
./scripts/upload-source-package.sh --shell
```

This drops you into a shell in the same container the real upload uses, with `dput`, `gnupg`,
`nano`, and `vim-tiny` installed, and your config directory mounted as home. Create
`~/.dput.cf` there yourself (`nano ~/.dput.cf`), or edit it directly from your host at
`~/.config/setdown-dput/.dput.cf` — it's the same file either way, and it's yours to set up
however you like; the upload script never writes to it itself. It persists across every future
run, so this is a one-time step. Add this stanza (current as of mentors.debian.net's own
documentation — it moved to HTTPS uploads a while back, not the older FTP-only flow):

```ini
[mentors]
fqdn = mentors.debian.net
incoming = /upload
method = https
allow_unsigned_uploads = 0
progress_indicator = 2
allowed_distributions = .*
```

`dist/source-build/` is also mounted read-only at `/upload` in this shell, in case you want to
inspect what would be uploaded or run `dput` by hand. Exit with Ctrl-D when you're done.

## What the script checks before uploading

1. A `.changes` file exists — either the one you passed, or the newest one in
   `dist/source-build/`.
2. That file starts with `-----BEGIN PGP SIGNED MESSAGE-----` — catches the mistake of
   uploading something `build-source-package.sh` hasn't actually signed yet.
3. It doesn't target distribution `UNRELEASED` — mentors.debian.net rejects that outright (see
   [building-the-source-package.md](building-the-source-package.md#before-a-real-upload)); this
   just gives a clearer error than `dput`'s own Python traceback would.
4. `~/.config/setdown-dput/.dput.cf` has a `[mentors]` stanza (pointing you at `--shell` if
   not).

Then it prints exactly what it's about to do and asks you to confirm before running
`dput mentors <file>` inside the container.
