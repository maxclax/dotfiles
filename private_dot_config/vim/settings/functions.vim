" -*- mode: vimrc -*-"

" => Abbreviations ------------------------------------------------------ {{{1

function! InsertIPD()
    return "import ipdb; ipdb.set_trace()"
endfunction

function! InsertIPT()
    return "{% load djcore_tags %}{{ __placeholder__|ipdb }}"
endfunction

" Insert mapping
inoremap ipd <C-R>=InsertIPD()<CR>
inoremap ipt <C-R>=InsertIPT()<CR>
