" Vim syntax file
" Language:     KAOS
" Maintainer:   Jeremy Apthorp <nornagon@nornagon.net>
" Last Change:  2008 Apr 13
" Revision:     0.2

" For version 5.x: Clear all syntax items
" For version 6.x: Quit when a syntax file was already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn case match
syn keyword kaosTodo            NOTE TODO FIXME XXX contained

syn case ignore

syn match kaosComment         "//.*$" contains=kaosTodo
syn region kaosComment        start="/\*" end="\*/" contains=kaosTodo

syn keyword kaosKeyword       script remove define returning atomic install ovar
syn keyword kaosConditional   if else
syn keyword kaosLoop          do while enum reps until for
syn keyword kaosBuiltin       owner pointer newsimple newcompound print unid
syn keyword kaosBuiltin       norn attributes move dcor perm kill tick pose
syn keyword kaosBuiltin       windowleft windowright null stop plane centrex
syn keyword kaosBuiltin       killpart changepart newtextpart textformat
syn keyword kaosBuiltin       parttext nopages moveto floatto inputmask
syn keyword kaosBuiltin       floatat newtextinputpart setfocus randomagent
syn keyword kaosBuiltin       shoutorder
syn keyword kaosType          string agent numeric void

syn region kaosString         start=+"+ end=+"+ skip="\\\"" oneline

syn match caosKind            "([rwm]\+)" contained
syn match caosVar             "$[a-zA-Z_][a-zA-Z0-9_]*" nextgroup=caosKind contained

syn cluster caos              contains=caosKind,caosVar,kaosCaos
syn keyword kaosKeyword       _caos nextgroup=kaosCaos skipwhite skipempty
syn region kaosCaos           start=+{+ end=+}+ contains=@caos,kaosComment contained

syn match kaosDecimal         "\<\d\+\>"

syn match kaosBlockParam      "\h\w*" contained
syn region kaosBlockParamList start="\%(\%({\)\s*\)\@<=|" end="|" oneline display contains=kaosBlockParam

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_kaos_syntax_inits")
  if version < 508
    let did_kaos_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink kaosTodo        Todo
  HiLink kaosKeyword     Keyword
  HiLink kaosBuiltin     Identifier
  HiLink kaosConditional Keyword
  HiLink kaosLoop        Repeat
  HiLink kaosType        Type
  HiLink kaosDecimal     Number
  HiLink kaosComment     Comment
  HiLink kaosString      String
  HiLink kaosBlockParam  Identifier

  HiLink caosVar         Identifier
  HiLink caosKind        Type

  delcommand HiLink
endif

let b:current_syntax = "kaos"
