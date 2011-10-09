" switch on syntax highlighting
syntax on

" enable filetype detection, plus loading of filetype plugins
filetype plugin indent on

set textwidth=70
set number
nnoremap <F2> :set nonumber!<CR>:set foldcolumn=0<CR>

" Spell check highlighting colours
hi clear SpellBad
hi SpellBad term=reverse ctermfg=red

" Set .wiki filetype to creole, for use with bitbucket wiki
" repositories.
au BufNewFile,BufRead *.wiki set filetype=creole
