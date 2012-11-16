" Vundle: {{{
set nocompatible
filetype off                    " required!

set rtp+=~/.vim/bundle/vundle/
call vundle#rc()

" Vundle manages Vundle required
Bundle 'gmarik/vundle'

" My Bundles: {{{
Bundle 'altercation/vim-colors-solarized'
Bundle 'godlygeek/tabular'
Bundle 'scrooloose/nerdtree'
Bundle 'git://vim-latex.git.sourceforge.net/gitroot/vim-latex/vim-latex'
Bundle 'lukerandall/haskellmode-vim'
Bundle 'VimClojure'
Bundle 'indenthaskell.vim'
Bundle 'pbrisbin/html-template-syntax'
Bundle 'rodjek/vim-puppet'
"}}}

filetype plugin indent on	" required
"}}}

" switch on syntax highlighting
syntax on

set relativenumber
nnoremap <F2> :set relativenumber!<CR>:set foldcolumn=0<CR>

set background=dark
colorscheme solarized

highlight ExtraWhitespace ctermbg=2 guibg=2
match ExtraWhitespace /\s\+$\| \+\ze\t/

" Highlight long lines {{{
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
"au BufWinEnter * call HighlightLongLines()
" Map it to <F3>
nnoremap <silent> <F3> :call HighlightLongLines()<CR>
"}}}

" Spell check highlighting colours
hi clear SpellBad
hi SpellBad term=reverse ctermfg=red

" Auto wrap text and comments
set fo=tcq

" Allow setting vim options from a comment at the end or tail of a file
set modeline

" Show me a ruler
set ruler

" Turn off search highlighting, I care not for it
set nohlsearch

" Latex: {{{
" We need to set grep to always show filename for vim-latex to work
set grepprg=grep\ -nH\ $*

" Set the default tex filetype
let g:tex_flavor='latex'

" Use xelatex to produce PDFs by default
let g:Tex_DefaultTargetFormat='pdf'
let g:Tex_ViewRule_pdf='xdg-open'
let g:Tex_CompileRule_pdf='xelatex --interaction=nonstopmode $*'
let g:Tex_MultipleCompileFormats='pdf'
" }}}

" Haskell: {{{
" Setup up haskell related stuff
" Use GHC functionality for haskell files
au BufEnter *.{l,}hs compiler ghc

" Configure browser for haskell_doc.vim
let g:haddock_browser = "xdg-open"
" }}}

" vim: set et ts=4 sw=4 fdm=marker:
