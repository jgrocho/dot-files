" We need to set grep to always show filename for vim-latex to work
setlocal grepprg=grep\ -nH\ $*

" Set the default tex filetype
let g:tex_flavor='latex'

" Use xelatex to produce PDFs by default
let g:Tex_DefaultTargetFormat='pdf'
let g:Tex_ViewRule_pdf='xdg-open'
let g:Tex_CompileRule_pdf='xelatex --interaction=nonstopmode $*'
let g:Tex_MultipleCompileFormats='pdf'
