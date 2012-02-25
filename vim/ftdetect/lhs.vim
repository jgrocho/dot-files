" Set literate haskell files to tex by default
au BufRead,BufNewFile *.lhs	set filetype=tex

" Turn off the IMAP feature for literate haskell files
au BufRead,BufNewFile *.lhs	let b:Imap_FreezeImap=1
