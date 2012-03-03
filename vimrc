" load pathogen, it is not an error if the file does not exist
runtime bundle/vim-pathogen/autoload/pathogen.vim
" now actually set it up if it loaded
if exists("g:loaded_pathogen")
    call pathogen#infect()
endif

" switch on syntax highlighting
syntax on

" enable filetype detection, plus loading of filetype plugins
filetype plugin indent on

set number
nnoremap <F2> :set nonumber!<CR>:set foldcolumn=0<CR>

" Highlight long lines
function HighlightLongLines()
    let w:highlight_long_lines
        \ = exists('w:highlight_long_lines') ?
        \ ! w:highlight_long_lines : 1
    if w:highlight_long_lines
        let w:m1=matchadd('Search', '\%<77v.\%>73v', -1)
        let w:m2=matchadd('ErrorMsg', '\%>76v.\+', -1)
    else
        call matchdelete(w:m1)
        call matchdelete(w:m2)
    endif
endfunction
" Enable it by default in all windows
au BufWinEnter * call HighlightLongLines()
" Map it to <F3>
nnoremap <silent> <F3> :call HighlightLongLines()<CR>

" Spell check highlighting colours
hi clear SpellBad
hi SpellBad term=reverse ctermfg=red

" turn off auto adding comments on next line
" so you can cut and paste reliably
" http://vimdoc.sourceforge.net/htmldoc/change.html#fo-table
set fo=tcq
set nocompatible
set modeline

" Show me a ruler
set ruler

" Turn off search highlighting, I care not for it
set nohlsearch

" We need to set grep to always show filename for vim-latex to work
set grepprg=grep\ -nH\ $*

" Set the default tex filetype
let g:tex_flavor='latex'

" Use xelatex to produce PDFs by default
let g:Tex_DefaultTargetFormat='pdf'
let g:Tex_ViewRule_pdf='xdg-open'
let g:Tex_CompileRule_pdf='xelatex --interaction=nonstopmode $*'
let g:Tex_MultipleCompileFormats='pdf'

set background=dark
colorscheme solarized

highlight ExtraWhitespace ctermbg=2 guibg=2
match ExtraWhitespace /\s\+$\| \+\ze\t/
