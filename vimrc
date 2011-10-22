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

" turn off auto adding comments on next line
" so you can cut and paste reliably
" http://vimdoc.sourceforge.net/htmldoc/change.html#fo-table
set fo=tcq
set nocompatible
set modeline

highlight LiteralTabs ctermbg=darkgreen guibg=darkgreen
match LiteralTabs /\s\	/
highlight ExtraWhitespace ctermbg=darkgreen guibg=darkgreen
match ExtraWhitespace /\s\+$/

" Show me a ruler
set ruler
