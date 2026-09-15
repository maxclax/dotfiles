# eusign

Command-line CAdES signing with a JKS key store through the ІІТ EUSignCP
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

## Environment

```bash
export EUSIGN_JKS="/path/to/key-store.jks"
export EUSIGN_JKS_PASSWORD='…'   # load from 1Password or the keychain
export EUSIGN_ALIAS='…'          # only if the store holds several keys
```

Keep the key store path and password out of this repository.

## Usage

```bash
sign-file --list                          # key aliases, no password needed
sign-file document.xml                    # detached → document.xml.p7s
sign-file document.xml signed.p7s
sign-file --attached document.xml signed.p7s
```

## Updating the libraries

Bump the version marker in the hook and run `chezmoi apply`. Add
`EUSIGN_FORCE_REINSTALL=1` to re-download even when the libraries are present.
