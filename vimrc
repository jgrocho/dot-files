" Plug: {{{

" Auto-install plug {{{
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif
" }}}

call plug#begin('~/.vim/plugged')

" My Plugins: {{{
Plug 'altercation/vim-colors-solarized'
Plug 'godlygeek/tabular'
Plug 'scrooloose/nerdtree'
"Plug 'git://vim-latex.git.sourceforge.net/gitroot/vim-latex/vim-latex'
Plug 'lukerandall/haskellmode-vim'
Plug 'vim-scripts/VimClojure'
Plug 'vim-scripts/indenthaskell.vim'
Plug 'pbrisbin/html-template-syntax'
Plug 'rodjek/vim-puppet'
Plug 'jnwhiteh/vim-golang'
Plug 'airblade/vim-gitgutter'
Plug 'jelera/vim-javascript-syntax'
Plug 'vim-scripts/ctrlp.vim'
Plug 'vim-scripts/Tabular'
Plug 'tpope/vim-fugitive'
Plug 'vim-scripts/textobj-user'
Plug 'vim-scripts/textobj-entire'
Plug 'nosami/Omnisharp'
Plug 'tpope/vim-dispatch'
Plug 'scrooloose/syntastic'
Plug 'derekwyatt/vim-scala'
Plug 'jamessan/vim-gnupg'
Plug 'lambdatoast/elm.vim'
Plug 'eagletmt/ghcmod-vim'
Plug 'eagletmt/neco-ghc'
Plug 'tomtom/tlib_vim'
Plug 'MarcWeber/vim-addon-mw-utils'
Plug 'garbas/vim-snipmate'
Plug 'scrooloose/nerdcommenter'
Plug 'ervandew/supertab'
Plug 'Shougo/neocomplete.vim'
Plug 'Shougo/vimproc.vim'
"}}}

call plug#end()
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
