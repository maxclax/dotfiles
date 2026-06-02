---
name: kindle-highlights
description: Convert a Kindle Scribe "Export Notebook" highlights PDF into a correctly-formatted Denote org file, written next to the PDF. Green highlights become quotes. A separate Hazel rule moves the .org into the Denote dir — this skill never touches the org dir.
allowed-tools: Read, Write, Bash, Glob
argument-hint: [path-to-notebook.pdf]
---

Turn a Kindle Scribe highlights export (a "Notebook - <Book>.pdf") into a Denote
note in plain Org. I read on the Kindle and color my highlights; **green means
"keep this as a quote."** This skill files the highlights as a book note and marks
the green ones so they later resurface at the top of my Org agenda.

## Division of labor (important)

This skill ONLY parses the PDF and writes a correctly-named, correctly-formatted
Denote `.org` file **in the same directory as the source PDF**. It does NOT move
the file. A separate Hazel rule watches that folder and moves the new `.org` into
my Denote directory. So: do not write to or read the org dir; just produce a valid
Denote file beside the PDF.

## Inputs

- If `$ARGUMENTS` is a PDF path, use it.
- Otherwise use the newest PDF in the Kindle Scribe drop folder:
  `~/Library/CloudStorage/GoogleDrive-*/My Drive/Kindle Scribe/`
  (the most recently modified `*.pdf`).

The output `.org` is written to the **same directory** as that PDF.

## How the export looks (so you parse it correctly)

The PDF is Amazon's "Export Notebook" format:

- A header with the **book title**, author (`by Surname, First`), and a
  `read.amazon.com/kp/kshare?asin=XXXX` preview link (the ASIN is the book id).
- An `Annotations (N)` summary, e.g. `3 Highlights | Pink (1), Green (1), Yellow (1)`.
- Then one block per annotation:
  - `Page NN | Highlight (Color)` followed by the highlighted **text** and a date.
  - `Page NN | Note` followed by an **image of handwriting** — NOT machine text.
- **The color is literal text** in `Highlight (Color)`. Match it as a string;
  never infer color from pixels.

## Output file

### Denote file name (write beside the PDF)
`IDENTIFIER--TITLE-SLUG__KEYWORDS.org`
- `IDENTIFIER` = `date +%Y%m%dT%H%M%S`
- `TITLE-SLUG` = book title, lowercased, non-alphanumerics → single hyphens,
  no leading/trailing hyphen
- `KEYWORDS` = the filetags joined by `_`

Example: `20260602T170000--the-ikigai-journey__book_highlights.org`

### Front matter — must be valid Denote Org, aligned exactly like this
```
#+title:      <Book Title>
#+date:       [<YYYY-MM-DD Day HH:MM>]
#+filetags:   :book:highlights:
#+identifier: <IDENTIFIER>

#+author:     <First Surname>
#+source:     <read.amazon.com kshare link>
#+asin:       <ASIN>
```
- The four standard keys (`title`, `date`, `filetags`, `identifier`) come first,
  in that order, value column aligned at the same place. Then a blank line, then
  the extra keys.
- `date` uses Denote's inactive-timestamp form, e.g. `[2026-06-02 Tue 17:00]`.
- `identifier` MUST equal the `IDENTIFIER` in the filename.
- Filetags are always exactly `:book:highlights:` — fixed, the same for every
  book. Do NOT derive keywords from the title or subject (no `:ikigai:` etc.).
  The filename keywords are therefore always `__book_highlights`.

### Body — highlights in reading order
One heading per highlight, in the order they appear, with page attribution:
```
* <highlight text>  — <Surname>, <Book Title> (p.NN)
```
If the PDF shows part/section headers (e.g. "Part One: …"), keep them as `*`
headings and nest highlights under them as `**`. Never truncate highlight text.

### Color rules (the important part)
The `Highlight (Color)` label decides what each highlight becomes. Match the color
as a literal string. Org tags go at the END of the heading line; for `TODO` the
keyword goes right after the stars.

| Color  | Becomes                                  | Heading shape |
|--------|------------------------------------------|---------------|
| Green  | a quote (surfaces in the agenda banner)  | `* <text> — <attr>   :quote:` |
| Blue   | an actionable follow-up (shows in agenda)| `* TODO <text> — <attr>` |
| Pink   | a key idea                               | `* <text> — <attr>   :idea:` |
| Orange | a term/definition to look up             | `* <text> — <attr>   :vocab:` |
| Yellow | a plain highlight                        | `* <text> — <attr>` |

`<attr>` = `<Surname>, <Book Title> (p.NN)`.

Notes:
- Use org-native tags (`:quote:`, `:idea:`, `:vocab:`) — NOT `#hashtags`. The
  agenda quote banner (`my/agenda-quotes-collect`) and the rest of my system read
  org tags; the `#quote` hashtag form is only parsed inside `Journelly.org`.
- `:quote:` is the one that matters most — it puts the line in the random quote
  banner at the top of my dashboard.
- Blue → `TODO` makes book follow-ups appear in my Org agenda like any task.
- Any color not listed (or unlabeled) → treat as a plain highlight.

### Handwritten notes
Ignore them. A `Page NN | Note` block is an image of handwriting that can't be
read reliably — skip it silently. Do NOT add a `* Notes` heading, a placeholder,
or any "not transcribed" line. Only highlights go in the file.

## Steps

1. Resolve the PDF path (argument, else newest in the Kindle Scribe folder).
2. Read the PDF and extract: title, author, ASIN/source link, and every
   annotation with its page, type, color, text, and date.
3. Build the Org content, applying the color rules above (Green→quote, Blue→TODO,
   Pink→idea, Orange→vocab, Yellow→plain).
4. Compute `IDENTIFIER` (`date +%Y%m%dT%H%M%S`) and write the file with the Denote
   name **into the same directory as the PDF**.
5. Report: book title, output filename, and a per-color breakdown
   (quotes / todos / ideas / vocab / plain). Remind that Hazel will move it
   to the Denote dir.

## Rules

- Write the `.org` file beside the PDF. Do NOT move it and do NOT touch the org dir.
- One note per book; never split a book across files.
- Apply the color rules exactly: Green→`:quote:`, Blue→`TODO`, Pink→`:idea:`,
  Orange→`:vocab:`, Yellow/other→plain highlight.
- Ignore handwritten notes entirely — no Notes section, no placeholder lines.
- Preserve highlighted text verbatim (curly quotes, em dashes, etc.).
- Do not move, rename, or delete the source PDF.
- Quote any paths in shell commands — folders contain spaces.
