# eusign — instructions for AI agents

`sign-file` creates qualified electronic signatures (CAdES `.p7s`) on this Mac.
**A signature is legally binding.** Follow these rules exactly.

## Rules

1. **Sign only on an explicit human request** in the current conversation that
   names the file(s) and the key profile. Never sign on your own initiative,
   never sign more files than were named, and never sign a file received from
   a third party unless the human confirms they have reviewed it.
2. **Confirm before every signature.** Show the human the file path, size,
   SHA-256, profile, mode (detached or attached) and output path, then wait
   for an explicit "yes".
3. **Never touch key material.** Do not read, print, log, copy or store the key
   store or its password. Do not run `op read` or `op item get` on the
   `eusign` item yourself, do not pass passwords as arguments, and do not set
   `EUSIGN_JKS_PASSWORD`. `sign-file` fetches what it needs in memory.
4. **Never overwrite** an existing `.p7s` without confirmation.
5. **Keep identity private.** `sign-file` prints the certificate holder
   (`cn=…`) and profiles may be named after tax IDs. Do not repeat either
   outside the private conversation with the human.
6. **Stop on any error.** Report it and wait. Never retry a failed signature or
   a wrong password in a loop.

## Commands

```bash
sign-file --keys                                   # list profiles
sign-file --key NAME --check                       # confirm the key opens; signs nothing
sign-file --key NAME --info                        # certificate validity; no password
sign-file --key NAME document.xml                  # detached → document.xml.p7s
sign-file --key NAME document.xml signed.p7s       # detached, explicit output
sign-file --key NAME --attached document.xml out.p7s   # only when asked for attached
```

Detached is the default and leaves the original file unchanged. Use
`--attached` only when the human asks for it.

## Make commands

The same key operations exist as make targets in the home directory
(`make -C ~ TARGET`).

| Command | Agents | Purpose |
|---|---|---|
| `make eusign_keys` | may run | list profiles |
| `make eusign_info key=NAME` | may run | certificate validity; flags `renew soon` and `EXPIRED` |
| `make eusign_check key=NAME` | may run | confirm the stored password opens the key |
| `make eusign_key_set key=NAME jks=PATH` | **never** | add or renew a key; human only |
| `make eusign_key_delete key=NAME` | **never** | remove a key; human only |

There is no make target for signing. Sign only with `sign-file`, and only after
the confirmation in rule 2.

## Workflow

1. `sign-file --keys`. If there is more than one profile and the human did not
   name one, ask which.
2. `sign-file --key NAME --check` once per session before the first signature,
   and `sign-file --key NAME --info`. If the signing certificate is expired, stop;
   if it expires within 30 days, warn the human before continuing. Either way,
   give them the command from **Renewing a key**.
3. `shasum -a 256 FILE` and `ls -l FILE`, then present the confirmation summary
   from rule 2 and wait.
4. Sign.
5. Confirm the `.p7s` exists and is not empty, then report its path and size.

## Errors

| Output contains | Meaning | Do |
|---|---|---|
| `op:` with sign-in, authorization or unlock wording | 1Password is locked or needs approval | Ask the human to unlock 1Password and approve the prompt, then retry once |
| `no profile 'X' … have: …` | Wrong profile name | Ask the human which profile to use |
| `EUSign lib dir missing` | Libraries not installed | Tell the human to run `chezmoi apply` |
| `EUReadPrivateKeyBinary` | Wrong password or unreadable key | Stop and tell the human; do not retry |
| certificate expired, not found, or cannot be used | Key needs renewal | Stop and give the human the command from **Renewing a key** |
| exit code 134 or 139, or no output | Crash | Stop and report it; do not retry |

## Keys

All keys live in one 1Password item (`eusign` by default), one section per
key. Only the human adds, renews or deletes keys. Never run
`make eusign_key_set`, `make eusign_key_delete`, `sign-file --set-key` or
`sign-file --delete-key`, even when asked to "just renew it": they prompt for the
key store password, and that password must never pass through you.

### Renewing a key

When `--info` shows `renew soon` or `EXPIRED`, or signing fails with a
certificate error, tell the human to run these with their profile name and the
path to the new key store:

```bash
make eusign_key_set key=NAME jks=/path/to/new-key.jks
make eusign_info key=NAME
```

The first command asks for the new password, checks that it opens the key, and
replaces the old key in the same profile. See `README.md` beside this file for
the key layout.
