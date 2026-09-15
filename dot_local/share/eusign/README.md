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

Add, renew and remove keys with the `make` targets below.

The key store and password are read into memory for each run and never written
to disk.

```bash
sign-file --keys                     # list profiles
sign-file --key NAME --list          # aliases in that key store
sign-file --key NAME --check         # confirm the password opens the key
sign-file --key NAME document.xml    # detached → document.xml.p7s
export EUSIGN_KEY=NAME               # default profile for this shell
```

## Managing keys

Run these yourself; agents must never add, renew or delete keys.

```bash
make eusign_keys                                   # list profiles
make eusign_info key=NAME                          # certificate validity, no password
make eusign_check key=NAME                         # confirm the password opens the key
make eusign_key_set key=NAME jks=/path/to/key.jks  # add a key, or renew one in place
make eusign_key_delete key=NAME                    # remove a key (asks you to confirm)
```

`eusign_key_set` asks for the key store password and opens the key locally, so a
wrong password never reaches 1Password. It then stores the key store and password
in the profile's section, replacing what was there, and reads the stored copy
back to compare it before reporting success. The first key creates the item;
set `EUSIGN_OP_VAULT` to choose its vault.

`eusign_info` flags certificates that are expired or expire within 30 days.

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

## Signature levels

`--level` sets the CAdES level. The default is `x-long`.

| Level | Contents | Network |
|---|---|---|
| `bes` | signature and signer certificate | none |
| `t` | adds a timestamp from the signer's certification authority | timestamp server |
| `x-long` | adds the timestamp plus certificate and revocation data, so it stays verifiable long-term | timestamp and OCSP servers |

Many document-exchange services reject `bes`. Check any signature's level with:

```bash
sign-file --inspect document.xml.p7s
```

The servers come from ІІТ's public list of certification authorities, which the
install hook saves as `~/opt/eusign/certs/CAs.json`. `sign-file` picks the entry
matching the signing certificate's issuer, so any authority in that list works.

The requests leave from Apple's `/usr/bin/python3`: `sign-file` re-runs itself
under it when the developer tools are installed. One firewall rule for that
binary therefore keeps working across nix updates.

## Updating the libraries

Bump the version marker in the hook and run `chezmoi apply`. Add
`EUSIGN_FORCE_REINSTALL=1` to re-download even when the libraries are present.
