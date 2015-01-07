setlocal expandtab
setlocal tabstop=4 shiftwidth=4

" Setup up haskell related stuff
" Use GHC functionality for haskell files
au BufEnter *.{l,}hs compiler ghc

" Configure browser for haskell_doc.vim
let g:haddock_browser = "xdg-open"
