"syn match   pythonDefStatement  /^\s*\%(def\|class\)/
"       \ nextgroup=pythonFunction skipwhite
"syn region  pythonFunctionFold  start="^\z(\s*\)\%(def\|class\)\>"
"       \ end="\ze\%(\s*\n\)\+\%(\z1\s\)\@!." fold transparent
"syn match   pythonFunction	"[a-zA-Z_][a-zA-Z0-9_]*" contained
"
"hi link pythonDefStatement Statement
