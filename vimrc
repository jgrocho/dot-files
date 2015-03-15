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
Bundle 'jnwhiteh/vim-golang'
Bundle 'airblade/vim-gitgutter'
Bundle 'jelera/vim-javascript-syntax'
Bundle 'ctrlp.vim'
Bundle 'Tabular'
Bundle 'tpope/vim-fugitive'
Bundle 'textobj-user'
Bundle 'textobj-entire'
Bundle 'nosami/Omnisharp'
Bundle 'tpope/vim-dispatch'
Bundle 'scrooloose/syntastic'
Bundle 'derekwyatt/vim-scala'
Bundle 'jamessan/vim-gnupg'
"}}}

filetype plugin indent on	" required
"}}}

" switch on syntax highlighting
syntax on

set number
set relativenumber
nnoremap <F2> :set number! relativenumber!<CR>:set foldcolumn=0<CR>

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
" Turn on incremental search
set incsearch

highlight clear SignColumn

" Configure browser for haskell_doc.vim
let g:haddock_browser = "xdg-open"
" vim: set et ts=4 sw=4 fdm=marker:
