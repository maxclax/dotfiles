## Qutebrowser configuration

config.load_autoconfig(False)

# ── Editor ────────────────────────────────────────────────────────────────────
# Open text fields in Emacs with Ctrl-E
c.editor.command = ['emacsclient', '-c', '{}']

# ── UI ────────────────────────────────────────────────────────────────────────
c.fonts.default_size = '14pt'
c.zoom.default = '120%'

# ── Behaviour ─────────────────────────────────────────────────────────────────
c.auto_save.session = True
c.session.lazy_restore = True

# ── Emacs-style bindings in command/prompt mode ───────────────────────────────
config.bind('<Ctrl-p>', 'command-history-prev', mode='command')
config.bind('<Ctrl-n>', 'command-history-next', mode='command')
config.bind('<Ctrl-a>', 'rl-beginning-of-line', mode='command')
config.bind('<Ctrl-e>', 'rl-end-of-line', mode='command')
config.bind('<Ctrl-b>', 'rl-backward-char', mode='command')
config.bind('<Ctrl-f>', 'rl-forward-char', mode='command')
config.bind('<Alt-b>',  'rl-backward-word', mode='command')
config.bind('<Alt-f>',  'rl-forward-word', mode='command')
config.bind('<Ctrl-k>', 'rl-kill-line', mode='command')
config.bind('<Ctrl-u>', 'rl-unix-line-discard', mode='command')
config.bind('<Ctrl-w>', 'rl-unix-word-rubout', mode='command')
