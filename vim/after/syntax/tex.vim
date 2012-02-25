" Remember where the cursor is, and go to upperleft
let s:oldline=line(".")
let s:oldcolumn=col(".")
call cursor(1,1)

unlet b:current_syntax
setlocal isk+=_
syntax cluster lhsTeXContainer contains=tex.*Zone,texAbstract

" Literate Haskell is Haskell in between text, so at least read Haskell
" highlighting
if version < 600
    syntax include @haskellTop <sfile>:p:h/haskell.vim
else
    syntax include @haskellTop syntax/haskell.vim
endif

syntax region lhsHaskellBeginEndBlock start="\\begin{code}" matchgroup=NONE end="\%(\\end{code}.*$\)\@=" contains=@haskellTop,beginCodeBegin containedin=@lhsTeXContainer

syntax match beginCodeBegin "\\begin" nextgroup=beginCodeCode contained
syntax region beginCodeCode matchgroup=texDelimiter start="{" end="}"

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_tex_syntax_inits")
  if version < 508
    let did_tex_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink beginCodeBegin	      texCmdName
  HiLink beginCodeCode	      texSection

  delcommand HiLink
endif

" Restore cursor to original position, as it may have been disturbed
" by the searches in our guessing code
call cursor (s:oldline, s:oldcolumn)

unlet s:oldline
unlet s:oldcolumn

let b:current_syntax = "lhaskell"
"let b:current_syntax = "tex"

" vim: ts=8
