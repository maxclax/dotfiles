# eusign

Command-line CAdES signing with JKS key stores through the ІІТ EUSignCP
library (`euscp.dylib`). macOS only.

## Layout

| Path | Purpose |
|------|---------|
| `~/opt/eusign/lib/` | `euscp.dylib` and its sibling ІІТ libraries |
| `~/opt/eusign/certs/` | certificate file store (`CACertificates.p7b`) |
| `~/Applications/EU.app` | ІІТ desktop client (optional) |
| `~/.local/bin/sign-file` | the CLI |

The chezmoi hook `run_onchange_after_30-eusign` downloads the official ІІТ
packages, refuses any that are not signed and notarized under ІІТ's Apple
Developer ID, and extracts them user-locally without sudo.

## Keys in 1Password

All keys live in one 1Password item, titled `eusign` (override with
`EUSIGN_OP_ITEM`, and `EUSIGN_OP_VAULT` if the title is not unique). Each key
is one section of that item:

- the section name is the profile name you pass to `--key`
- a `password` field with the key store password
- the key store attached as a file in that section (or attached to the item
  itself as `<profile>.jks`)
- optionally an `alias` field, when the store holds several keys

To add a key, add a section to the item, fill in the fields and attach the
key store, then confirm it with `sign-file --key NAME --check`.

The key store and password are read into memory for each run and never written
to disk.

```bash
sign-file --keys                     # list profiles
sign-file --key NAME --list          # aliases in that key store
sign-file --key NAME --check         # confirm the password opens the key
sign-file --key NAME document.xml    # detached → document.xml.p7s
export EUSIGN_KEY=NAME               # default profile for this shell
```

## Keys from environment variables

Without a profile, `sign-file` reads the key store from disk:

```bash
export EUSIGN_JKS="/path/to/key-store.jks"
export EUSIGN_JKS_PASSWORD='…'   # load from 1Password or the keychain
export EUSIGN_ALIAS='…'          # only if the store holds several keys
```

## Signing modes

```bash
sign-file --key NAME document.xml                      # detached → document.xml.p7s
sign-file --key NAME document.xml signed.p7s
sign-file --key NAME --attached document.xml signed.p7s  # data embedded
```

## Updating the libraries

Bump the version marker in the hook and run `chezmoi apply`. Add
`EUSIGN_FORCE_REINSTALL=1` to re-download even when the libraries are present.
