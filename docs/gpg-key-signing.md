# How to generate a GPG key for signing (e.g. Debian packages)

This guide covers generating a personal GPG key suitable for signing things
like git commits or Debian packages, and what to expect from key expiration.

## Generating a key

Run this yourself in a terminal (it prompts for a passphrase, so don't pipe
it through anything that logs output):

```
gpg --full-generate-key
```

When prompted:

- **Key type**: `(9) ECC (sign and encrypt)`, then curve `(1) Curve 25519`
  (Ed25519). It's modern, fast, and both GitHub and GitLab verify Ed25519
  signatures fine. If you need compatibility with older tooling, RSA 4096
  (`(1) RSA and RSA`) is the safe fallback.
- **Expiration**: pick based on how the key will be used — see
  [Choosing an expiration](#choosing-an-expiration) below.
- **Real name / email**: use the identity you want signatures attributed to.
  If you want a green "Verified" badge on GitHub, the email must match a
  verified email on your GitHub account.
- **Passphrase**: set one — it protects the private key at rest.

Once generated, find the key ID:

```
gpg --list-secret-keys --keyid-format=long
```

### Using it for git commit signing

```
git config --global user.signingkey <KEYID>
git config --global commit.gpgsign true
gpg --armor --export <KEYID>
```

Paste the output of the last command into GitHub → Settings → SSH and GPG
keys.

## Choosing an expiration

A short expiration (e.g. 1 year) is reasonable for a key you use often and
will remember to rotate. A longer expiration (2-3 years) suits a key used
for infrequent but important releases — e.g. a package signing key — where
nobody is likely to notice it's about to lapse until something breaks.

## What happens when the key expires

Two distinct things happen, and they're often conflated:

1. **Signing stops working.** Once the key passes its expiry date, `gpg`
   refuses to create new signatures with it. `debsign`,
   `dpkg-buildpackage -k<id>`, or any Release-file signing (`reprepro`,
   `apt-ftparchive`) will start failing. You will not be able to release new
   packages with that key until it's fixed.

2. **Old signatures aren't retroactively invalid data**, but many verifiers
   check the key's *current* validity rather than "was it valid at signing
   time." This is the sharp edge with `apt` specifically — if a repository's
   Release-file signing key expires, `apt-get update` starts throwing
   `KEYEXPIRED` errors for every client, even for content that was signed
   before the key expired. This has broken real Debian/Ubuntu-adjacent
   archives before.

## Renewing before (or after) expiry

Expiration is just a timestamp in the key's self-signature, not a property
baked into the keypair. Extend it on the *same* key — same fingerprint, same
trust chain — with:

```
gpg --edit-key <KEYID>
gpg> expire
gpg> save
```

Then re-publish the updated public key anywhere it's distributed (keyserver,
apt keyring package, GitHub, etc.). Do this *before* the old expiry lapses
so there's no gap where signing or verification breaks. Set a reminder a
few weeks ahead of the expiry date — a key used for infrequent releases is
exactly the kind of thing nobody remembers until it stops working.
