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
sign-file --key NAME document.xml                  # detached → document.xml.p7s
sign-file --key NAME document.xml signed.p7s       # detached, explicit output
sign-file --key NAME --attached document.xml out.p7s   # only when asked for attached
```

Detached is the default and leaves the original file unchanged. Use
`--attached` only when the human asks for it.

## Workflow

1. `sign-file --keys`. If there is more than one profile and the human did not
   name one, ask which.
2. `sign-file --key NAME --check` once per session before the first signature.
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
| certificate expired, not found, or cannot be used | Key needs renewal | Stop and tell the human the key must be renewed |
| exit code 134 or 139, or no output | Crash | Stop and report it; do not retry |

## Keys

All keys live in one 1Password item (`eusign` by default), one section per
key. Adding or renewing keys is done by the human, not by agents. See
`README.md` beside this file for the layout.
