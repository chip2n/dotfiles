" Chiptheme
" Maintainer: Andreas Arvidsson (andreas.arvidson@gmail.com)
"
set background=dark
if version > 580
	hi clear
	if exists("syntax_on")
		syntax reset
	endif
endif

let g:colors_name = "chiptheme"

" General UI
hi Normal         gui=NONE      guifg=#f8f8ff      guibg=#1f1f1f guisp=#1f1f1f
hi Normal       cterm=NONE    ctermfg=NONE       ctermbg=NONE
hi NonText        gui=NONE      guifg=#8eabbd      guibg=#1f1f1f guisp=#262626
hi NonText      cterm=NONE    ctermfg=4          ctermbg=NONE
hi LineNr         gui=NONE      guifg=#666060      guibg=#292929 guisp=#2e2a26
hi LineNr       cterm=NONE    ctermfg=8          ctermbg=NONE
hi CursorLine     gui=NONE      guifg=NONE         guibg=#292929
hi CursorLine   cterm=NONE    ctermfg=NONE       ctermbg=0
hi CursorLineNr   gui=NONE      guifg=#f8f8ff      guibg=NONE
hi CursorLineNr cterm=NONE    ctermfg=7         ctermbg=0
hi TabLine        gui=NONE      guifg=#666060      guibg=#222222
hi TabLine      cterm=NONE    ctermfg=8          ctermbg=0
hi TabLineSel     gui=NONE      guifg=#666060      guibg=#292929
hi TabLineSel   cterm=NONE    ctermfg=NONE       ctermbg=4
hi TabLineFill    gui=NONE      guifg=#1f1f1f      guibg=#292929
hi TabLineFill  cterm=NONE    ctermfg=NONE       ctermbg=NONE
hi VertSplit      gui=NONE      guifg=#292929      guibg=#222020
hi VertSplit    cterm=NONE    ctermfg=0          ctermbg=0
hi StatusLineNC   gui=NONE      guifg=#292929      guibg=#ffffff
hi StatusLineNC cterm=NONE    ctermfg=8          ctermbg=0
hi StatusLine     gui=NONE      guifg=#292929      guibg=#ffffff
hi StatusLine  cterm=NONE     ctermfg=NONE       ctermbg=4
hi Visual         gui=NONE      guifg=#666060      guibg=#292929 guisp=#f8f8ff
hi Visual       cterm=NONE    ctermfg=8          ctermbg=0
hi Folded         gui=NONE      guifg=#666060      guibg=#292929 guisp=#737373
hi Folded       cterm=NONE    ctermfg=7          ctermbg=0
hi Folded         gui=NONE      guifg=#ffffff      guibg=#ff0000
hi OverLength   cterm=NONE    ctermfg=15         ctermbg=1

" Syntax
hi Comment     gui=NONE   guifg=#444040 guibg=NONE guisp=NONE
hi Comment   cterm=NONE ctermfg=8   ctermbg=NONE
hi SpecialComment guifg=#DDDD00 guibg=NONE guisp=NONE gui=NONE ctermfg=5 ctermbg=NONE cterm=NONE
hi Include     gui=NONE guifg=#bf728f guibg=NONE guisp=NONE
hi Include   cterm=NONE ctermfg=12 ctermbg=NONE
hi Statement gui=None guifg=#f2b009 guibg=NONE guisp=NONE
hi Statement cterm=None ctermfg=12 ctermbg=NONE
hi Conditional gui=NONE guifg=#ffa500 guibg=NONE guisp=NONE 
hi Conditional cterm=NONE ctermfg=4 ctermbg=NONE
hi Operator guifg=#5ba7c2 guibg=NONE guisp=NONE gui=NONE ctermfg=4 ctermbg=NONE cterm=NONE
hi Character guifg=#00ffff guibg=NONE guisp=NONE gui=NONE ctermfg=3 ctermbg=NONE cterm=NONE
hi SpecialChar guifg=#DDDD00 guibg=NONE guisp=NONE gui=NONE ctermfg=3 ctermbg=NONE cterm=NONE
hi String guifg=#ff7070 guibg=NONE guisp=NONE gui=bold ctermfg=10 ctermbg=NONE cterm=bold
hi Number guifg=#FF7070 guibg=NONE guisp=NONE gui=bold ctermfg=9 ctermbg=NONE cterm=bold
hi Boolean guifg=#d2b48c guibg=NONE guisp=NONE gui=NONE ctermfg=9 ctermbg=NONE cterm=NONE

"hi CursorColumn guibg=#292929
"hi ColorColumn guibg=#292929 ctermbg=8

hi IncSearch guifg=#ffff00 guibg=#0000ff guisp=#0000ff gui=bold ctermfg=11 ctermbg=21 cterm=bold
"hi WildMenu -- no settings --
hi SignColumn guibg=#292929
"hi Typedef guifg=#ffa500 guibg=NONE guisp=NONE gui=NONE ctermfg=214 ctermbg=NONE cterm=NONE
"hi PreCondit guifg=#bf728f guibg=NONE guisp=NONE gui=NONE ctermfg=213 ctermbg=NONE cterm=NONE
"hi CTagsMember -- no settings --
"hi CTagsGlobalConstant -- no settings --
"hi DiffText -- no settings --
"hi ErrorMsg -- no settings --
"hi Ignore -- no settings --
hi Debug guifg=#DDDD00 guibg=NONE guisp=NONE gui=NONE ctermfg=184 ctermbg=NONE cterm=NONE
hi PMenuSbar guifg=NONE guibg=#848688 guisp=#848688 gui=NONE ctermfg=NONE ctermbg=102 cterm=NONE
hi Identifier guifg=#60DD60 guibg=NONE guisp=NONE gui=NONE ctermfg=10 ctermbg=NONE cterm=NONE
hi StorageClass guifg=#ffa500 guibg=NONE guisp=NONE gui=NONE ctermfg=12 ctermbg=NONE cterm=NONE
"hi Todo -- no settings --
hi Special guifg=#bf728f guibg=NONE guisp=NONE gui=NONE ctermfg=132 ctermbg=NONE cterm=NONE
"hi Label guifg=#ffa500 guibg=NONE guisp=NONE gui=NONE ctermfg=214 ctermbg=NONE cterm=NONE
"hi CTagsImport -- no settings --
hi PMenuSel guifg=#88dd88 guibg=#949698 guisp=#949698 gui=NONE ctermfg=114 ctermbg=246 cterm=NONE
hi Search guifg=#000000 guibg=#8db6cd guisp=#8db6cd gui=NONE ctermfg=NONE ctermbg=110 cterm=NONE
"hi CTagsGlobalVariable -- no settings --
hi Delimiter guifg=#DDDD00 guibg=NONE guisp=NONE gui=NONE ctermfg=184 ctermbg=NONE cterm=NONE
"hi SpellRare -- no settings --
"hi EnumerationValue -- no settings --
"hi Float -- no settings --
"hi Union -- no settings --
"hi TabLineFill -- no settings --
"hi Question -- no settings --
hi WarningMsg guifg=#ff0000 guibg=#f8f8ff guisp=#f8f8ff gui=bold ctermfg=196 ctermbg=189 cterm=bold
"hi VisualNOS -- no settings --
"hi DiffDelete -- no settings --
"hi ModeMsg -- no settings --
"hi Define guifg=#bf728f guibg=NONE guisp=NONE gui=NONE ctermfg=213 ctermbg=NONE cterm=NONE
"hi Function guifg=#60DD60 guibg=NONE guisp=NONE gui=NONE ctermfg=77 ctermbg=NONE cterm=NONE
"hi FoldColumn -- no settings --
hi PreProc guifg=#bf728f guibg=NONE guisp=NONE gui=NONE ctermfg=9 ctermbg=NONE cterm=NONE
"hi EnumerationName -- no settings --
"hi MoreMsg -- no settings --
"hi SpellCap -- no settings --
hi Exception guifg=#ffa500 guibg=NONE guisp=NONE gui=NONE ctermfg=214 ctermbg=NONE cterm=NONE
"hi Keyword guifg=#ffa500 guibg=NONE guisp=NONE gui=NONE ctermfg=214 ctermbg=NONE cterm=NONE
hi Type guifg=#bf728f guibg=NONE guisp=NONE gui=NONE ctermfg=13 ctermbg=NONE cterm=NONE
"hi DiffChange -- no settings --
hi Cursor guifg=#000000 guibg=#f8f8ff guisp=#00ee00 gui=NONE ctermfg=NONE ctermbg=10 cterm=NONE
"hi SpellLocal -- no settings --
"hi Error guifg=NONE guibg=#cd0000 guisp=#cd0000 gui=NONE ctermfg=NONE ctermbg=160 cterm=NONE
"hi PMenu guifg=#dddddd guibg=#545658 guisp=#545658 gui=NONE ctermfg=253 ctermbg=240 cterm=NONE
"hi SpecialKey guifg=#87ceeb guibg=NONE guisp=NONE gui=NONE ctermfg=117 ctermbg=NONE cterm=NONE
"hi Constant guifg=#FF7070 guibg=NONE guisp=NONE gui=NONE ctermfg=9 ctermbg=NONE cterm=NONE
"hi DefinedName -- no settings --
"hi Tag guifg=#DDDD00 guibg=NONE guisp=NONE gui=NONE ctermfg=184 ctermbg=NONE cterm=NONE
"hi PMenuThumb guifg=NONE guibg=#a4a6a8 guisp=#a4a6a8 gui=NONE ctermfg=NONE ctermbg=248 cterm=NONE
"hi MatchParen -- no settings --
"hi LocalVariable -- no settings --
hi Repeat guifg=#ffa500 guibg=NONE guisp=NONE gui=NONE ctermfg=214 ctermbg=NONE cterm=NONE
"hi SpellBad -- no settings --
"hi CTagsClass -- no settings --
"hi Directory guifg=#87ceeb guibg=NONE guisp=NONE gui=NONE ctermfg=117 ctermbg=NONE cterm=NONE
hi Structure guifg=#ffa500 guibg=NONE guisp=NONE gui=NONE ctermfg=12 ctermbg=NONE cterm=NONE
"hi Macro guifg=#f2b009 guibg=NONE guisp=NONE gui=NONE ctermfg=213 ctermbg=NONE cterm=NONE
"hi Underlined -- no settings --
"hi DiffAdd -- no settings --

hi Title guifg=#aaaaaa

hi cursorim guifg=#191a24 guibg=#675391 guisp=#675391 gui=NONE ctermfg=235 ctermbg=60 cterm=NONE
"hi clear -- no settings --
