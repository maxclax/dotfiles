#!/usr/bin/env python3
"""Extract the current Vitamin R objective from the app's plist (RTFD binary)."""
import subprocess, base64, re, sys, os

plist = os.path.expanduser('~/Library/Preferences/net.publicspace.dist.vitaminr4.plist')
r = subprocess.run(
    ['plutil', '-extract', 'objective', 'raw', '-o', '-', plist],
    capture_output=True
)
if r.returncode != 0:
    sys.exit(1)

data = base64.b64decode(r.stdout.strip())
raw = data.decode('latin-1', errors='ignore')

# Extract RTF block from RTFD flat binary
m = re.search(r'\{\\rtf1.+?\}(?=\x01)', raw, re.DOTALL)
if not m:
    sys.exit(1)

rtf = m.group(0)

# Strip header groups (fonttbl, colortbl, expandedcolortbl, \* groups)
result, depth, skip_depth, i = [], 0, None, 0
while i < len(rtf):
    c = rtf[i]
    if c == '{':
        depth += 1
        if skip_depth is None and re.match(r'\{\\(fonttbl|colortbl|expandedcolortbl|\*)', rtf[i:i+30]):
            skip_depth = depth
        i += 1
    elif c == '}':
        if skip_depth == depth:
            skip_depth = None
        depth -= 1
        i += 1
    elif skip_depth is not None:
        i += 1
    else:
        result.append(c)
        i += 1
rtf = ''.join(result)

# Convert RTF unicode escapes to real characters
rtf = re.sub(r'\\uc0\\u(\d+)\s*', lambda m: chr(int(m.group(1))), rtf)
rtf = re.sub(r'\\u(\d+)\s*', lambda m: chr(int(m.group(1))), rtf)
rtf = re.sub(r'\\\n', '', rtf)
rtf = re.sub(r'\\[a-zA-Z]+[-\d]*\s*', ' ', rtf)

text = re.sub(r'\s+', ' ', rtf).strip()
if text:
    print(text)
else:
    sys.exit(1)
