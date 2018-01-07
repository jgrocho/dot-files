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
Bundle 'VimClojure'
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
Bundle 'lambdatoast/elm.vim'
Bundle 'eagletmt/ghcmod-vim'
Bundle 'eagletmt/neco-ghc'
Bundle 'tomtom/tlib_vim'
Bundle 'MarcWeber/vim-addon-mw-utils'
Bundle 'garbas/vim-snipmate'
Bundle 'scrooloose/nerdcommenter'
Bundle 'ervandew/supertab'
Bundle 'Shougo/neocomplete.vim'
Bundle 'Shougo/vimproc.vim'
"}}}

filetype plugin indent on	" required
"}}}

" switch on syntax highlighting
syntax enable

set number
set relativenumber
nnoremap <F2> :set number! relativenumber!<CR>:set foldcolumn=0<CR>

colorscheme solarized

highlight ExtraWhitespace ctermbg=red guibg=red
match ExtraWhitespace /\s\+$\| \+\ze\t/

" Highlight long lines {{{
function! HighlightLongLines()
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

set smartindent
set autoindent

set completeopt=menuone,menu,longest

set wildignore+=*\\tmp\\*,*.swp,*.swo,*.zip,*.git,.cabal-sandbox,.stack-work
set wildmode=longest,list,full
set wildmenu
set completeopt+=longest

highlight clear SignColumn

"{{{ Configure synatastic
map <Leader>s :SyntasticToggleMode<CR>

set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 0
let g:syntastic_check_on_open = 0
let g:syntastic_check_on_wq = 0
"}}}

"{{{ Configure ghc-mod
map <silent> tw :GhcModTypeInsert<CR>
map <silent> ts :GhcModSplitFunCase<CR>
map <silent> tq :GhcModType<CR>
map <silent> te :GhcModTypeClear<CR>
"}}}

"{{{ Configure supertab
let g:SuperTabDefaultCompletionType = '<c-x><c-o>'

if has("gui_running")
    imap <c-space> <c-r>=SuperTabAlternateCompletion("\<lt>c-x>\<lt>c-o>")<cr>
else " no gui
    if has("unix")
        inoremap <Nul> <c-r>=SuperTabAlternateCompletion("\<lt>c-x>\<lt>c-o>")<cr>
    endif
endif
"}}}

map <Leader>n :NERDTreeToggle<CR>

vmap a= :Tabularize /=<CR>
vmap a; :Tabularize /::<CR>
vmap a- :Tabularize /-><CR>
vmap a( :Tabularize /^[^(]*\zs(<CR>

set omnifunc=syntaxcomplete#Complete

let g:haskell_tabular = 1
let g:haskellmode_completion_ghc = 1
autocmd FileType haskell setlocal omnifunc=necoghc#omnifunc

" Configure browser for haskell_doc.vim
let g:haddock_browser = "xdg-open"
" vim: set et ts=4 sw=4 fdm=marker:
