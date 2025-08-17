" -*- mode: vimrc -*-"

" => Custom commands ------------------------------------------------------ {{{1

" Command to close current buffer without closing the window.
command! Bd :bp | :sp | :bn | :bd
