if !exists('g:polyglot_disabled') || index(g:polyglot_disabled, 'javascript') == -1
  
" Vim indent file
" Language: Javascript
" Maintainer: Chris Paul ( https://github.com/bounceme )
" URL: https://github.com/pangloss/vim-javascript
" Last Change: December 4, 2017

" Only load this indent file when no other was loaded.
if exists('b:did_indent')
  finish
endif
let b:did_indent = 1

" Now, set up our indentation expression and keys that trigger it.
setlocal indentexpr=GetJavascriptIndent()
setlocal autoindent nolisp nosmartindent
setlocal indentkeys+=0],0)
" Testable with something like:
" vim  -eNs "+filetype plugin indent on" "+syntax on" "+set ft=javascript" \
"       "+norm! gg=G" '+%print' '+:q!' testfile.js \
"       | diff -uBZ testfile.js -

let b:undo_indent = 'setlocal indentexpr< smartindent< autoindent< indentkeys<'

" Only define the function once.
if exists('*GetJavascriptIndent')
  finish
endif

let s:cpo_save = &cpo
set cpo&vim

" indent correctly if inside <script>
" vim/vim@690afe1 for the switch from cindent
" overridden with b:html_indent_script1
call extend(g:,{'html_indent_script1': 'inc'},'keep')

" Regex of syntax group names that are or delimit string or are comments.
let s:bvars = {
      \ 'syng_strcom': 'string\|comment\|regex\|special\|doc\|template\%(braces\)\@!',
      \ 'syng_str': 'string\|template\|special' }
" template strings may want to be excluded when editing graphql:
" au! Filetype javascript let b:syng_str = '^\%(.*template\)\@!.*string\|special'
" au! Filetype javascript let b:syng_strcom = '^\%(.*template\)\@!.*string\|comment\|regex\|special\|doc'

function s:GetVars()
  call extend(b:,extend(s:bvars,{'js_cache': [0,0,0]}),'keep')
endfunction

" Get shiftwidth value
if exists('*shiftwidth')
  function s:sw()
    return shiftwidth()
  endfunction
else
  function s:sw()
    return &l:shiftwidth ? &l:shiftwidth : &l:tabstop
  endfunction
endif

" Performance for forwards search(): start search at pos rather than masking
" matches before pos.
let s:z = has('patch-7.4.984') ? 'z' : ''

" Expression used to check whether we should skip a match with searchpair().
let s:skip_expr = "s:SynAt(line('.'),col('.')) =~? b:syng_strcom"
let s:in_comm = s:skip_expr[:-14] . "'comment\\|doc'"

let s:rel = has('reltime')
" searchpair() wrapper
if s:rel
  function s:GetPair(start,end,flags,skip)
    return searchpair('\m'.a:start,'','\m'.a:end,a:flags,a:skip,s:l1,a:skip ==# 's:SkipFunc()' ? 2000 : 200)
  endfunction
else
  function s:GetPair(start,end,flags,skip)
    return searchpair('\m'.a:start,'','\m'.a:end,a:flags,a:skip,s:l1)
  endfunction
endif

function s:SynAt(l,c)
  let byte = line2byte(a:l) + a:c - 1
  let pos = index(s:synid_cache[0], byte)
  if pos == -1
    let s:synid_cache[:] += [[byte], [synIDattr(synID(a:l, a:c, 0), 'name')]]
  endif
  return s:synid_cache[1][pos]
endfunction

function s:ParseCino(f)
  let [s, n, divider] = [strridx(&cino, a:f)+1, '', 0]
  while s && &cino[ s ] =~ '[^,]'
    if &cino[ s ] == '.'
      let divider = 1
    elseif &cino[ s ] ==# 's'
      if n !~ '\d'
        return n . s:sw() + 0
      endif
      let n = str2nr(n) * s:sw()
      break
    else
      let [n, divider] .= [&cino[ s ], 0]
    endif
    let s += 1
  endwhile
  return str2nr(n) / max([divider, 1])
endfunction

" Optimized {skip} expr, only callable from the search loop which
" GetJavascriptIndent does to find the containing [[{(] (side-effects)
function s:SkipFunc()
  if s:top_col == 1
    throw 'out of bounds'
  elseif s:check_in
    if eval(s:skip_expr)
      return 1
    endif
    let s:check_in = 0
  elseif getline('.') =~ '\%<'.col('.').'c\/.\{-}\/\|\%>'.col('.').'c[''"]\|\\$'
    if eval(s:skip_expr)
      return 1
    endif
  elseif search('\m`\|\${\|\*\/','nW'.s:z,s:looksyn)
    if eval(s:skip_expr)
      let s:check_in = 1
      return 1
    endif
  else
    let s:synid_cache[:] += [[line2byte('.') + col('.') - 1], ['']]
  endif
  let [s:looksyn, s:top_col] = getpos('.')[1:2]
endfunction

function s:AlternatePair()
  let [pat, l:for] = ['[][(){};]', 2]
  while s:SearchLoop(pat,'bW','s:SkipFunc()')
    if s:LookingAt() == ';'
      if !l:for
        if s:GetPair('{','}','bW','s:SkipFunc()')
          return
        endif
        break
      else
        let [pat, l:for] = ['[{}();]', l:for - 1]
      endif
    else
      let idx = stridx('])}',s:LookingAt())
      if idx == -1
        return
      elseif !s:GetPair(['\[','(','{'][idx],'])}'[idx],'bW','s:SkipFunc()')
        break
      endif
    endif
  endwhile
  throw 'out of bounds'
endfunction

function s:Nat(int)
  return a:int * (a:int > 0)
endfunction

function s:LookingAt()
  return getline('.')[col('.')-1]
endfunction

function s:Token()
  return s:LookingAt() =~ '\k' ? expand('<cword>') : s:LookingAt()
endfunction

function s:PreviousToken(...)
  let [l:pos, tok] = [getpos('.'), '']
  if search('\m\k\{1,}\|\S','ebW')
    if getline('.')[col('.')-2:col('.')-1] == '*/'
      if eval(s:in_comm) && !s:SearchLoop('\S\ze\_s*\/[/*]','bW',s:in_comm)
        call setpos('.',l:pos)
      else
        let tok = s:Token()
      endif
    else
      let two = a:0 || line('.') != l:pos[1] ? strridx(getline('.')[:col('.')],'//') + 1 : 0
      if two && eval(s:in_comm)
        call cursor(0,two)
        let tok = s:PreviousToken(1)
        if tok is ''
          call setpos('.',l:pos)
        endif
      else
        let tok = s:Token()
      endif
    endif
  endif
  return tok
endfunction

function s:Pure(f,...)
  return eval("[call(a:f,a:000),cursor(a:firstline,".col('.').")][0]")
endfunction

function s:SearchLoop(pat,flags,expr)
  return s:GetPair(a:pat,'\_$.',a:flags,a:expr)
endfunction

function s:ExprCol()
  if getline('.')[col('.')-2] == ':'
    return 1
  endif
  let bal = 0
  while s:SearchLoop('[{}?:]','bW',s:skip_expr)
    if s:LookingAt() == ':'
      let bal -= !search('\m:\%#','bW')
    elseif s:LookingAt() == '?'
      if getline('.')[col('.'):col('.')+1] =~ '^\.\d\@!'
        " ?. conditional chain, not ternary start
      elseif !bal
        return 1
      else
        let bal += 1
      endif
    elseif s:LookingAt() == '{'
      return !s:IsBlock()
    elseif !s:GetPair('{','}','bW',s:skip_expr)
      break
    endif
  endwhile
endfunction

" configurable regexes that define continuation lines, not including (, {, or [.
let s:opfirst = '^' . get(g:,'javascript_opfirst',
      \ '\C\%([<>=,.?^%|/&]\|\([-:+]\)\1\@!\|\*\+\|!=\|in\%(stanceof\)\=\>\)')
let s:continuation = get(g:,'javascript_continuation',
      \ '\C\%([<=,.~!?/*^%|&:]\|+\@<!+\|-\@<!-\|=\@<!>\|\<\%(typeof\|new\|delete\|void\|in\|instanceof\|await\)\)') . '$'

function s:Continues()
  let tok = matchstr(strpart(getline('.'),col('.')-15,15),s:continuation)
  if tok =~ '[a-z:]'
    return tok == ':' ? s:ExprCol() : s:PreviousToken() != '.'
  elseif tok !~ '[/>]'
    return tok isnot ''
  endif
  return s:SynAt(line('.'),col('.')) !~? (tok == '>' ? 'jsflow\|^html' : 'regex')
endfunction

" Check if line 'lnum' has a balanced amount of parentheses.
function s:Balanced(lnum,line)
  let l:open = 0
  let pos = match(a:line, '[][(){}]')
  while pos != -1
    if s:SynAt(a:lnum,pos + 1) !~? b:syng_strcom
      let l:open += matchend(a:line[pos],'[[({]')
      if l:open < 0
        return
      endif
    endif
    let pos = match(a:line, !l:open ? '[][(){}]' : '()' =~ a:line[pos] ?
          \ '[()]' : '{}' =~ a:line[pos] ? '[{}]' : '[][]', pos + 1)
  endwhile
  return !l:open
endfunction

function s:OneScope()
  if s:LookingAt() == ')' && s:GetPair('(', ')', 'bW', s:skip_expr)
    let tok = s:PreviousToken()
    return (count(split('for if let while with'),tok) ||
          \ tok =~# '^await$\|^each$' && s:PreviousToken() ==# 'for') &&
          \ s:Pure('s:PreviousToken') != '.' && !(tok == 'while' && s:DoWhile())
  elseif s:Token() =~# '^else$\|^do$'
    return s:Pure('s:PreviousToken') != '.'
  elseif strpart(getline('.'),col('.')-2,2) == '=>'
    call cursor(0,col('.')-1)
    return s:PreviousToken() != ')' || s:GetPair('(', ')', 'bW', s:skip_expr)
  endif
endfunction

function s:DoWhile()
  let cpos = searchpos('\m\<','cbW')
  while s:SearchLoop('\C[{}]\|\<\%(do\|while\)\>','bW',s:skip_expr)
    if s:LookingAt() =~ '\a'
      if s:Pure('s:IsBlock')
        if s:LookingAt() ==# 'd'
          return 1
        endif
        break
      endif
    elseif s:LookingAt() != '}' || !s:GetPair('{','}','bW',s:skip_expr)
      break
    endif
  endwhile
  call call('cursor',cpos)
endfunction

" returns total offset from braceless contexts. 'num' is the lineNr which
" encloses the entire context, 'cont' if whether a:firstline is a continued
" expression, which could have started in a braceless context
function s:IsContOne(cont)
  let [l:num, pind] = b:js_cache[1] ?
        \ [b:js_cache[1], indent(b:js_cache[1]) + s:sw()] : [1,0]
  let [ind, b_l] = [indent('.') + !a:cont, 0]
  while line('.') > l:num && ind > pind || line('.') == l:num
    if indent('.') < ind && s:OneScope()
      let b_l += 1
    elseif !a:cont || b_l || ind < indent(a:firstline)
      break
    else
      call cursor(0,1)
    endif
    let ind = min([ind, indent('.')])
    if s:PreviousToken() is ''
      break
    endif
  endwhile
  return b_l
endfunction

function s:IsSwitch()
  return search(printf('\m\C\%%%dl\%%%dc%s',b:js_cache[1],b:js_cache[2],
        \ '{\_s*\%(\%(\/\/.*\_$\|\/\*\_.\{-}\*\/\)\@>\_s*\)*\%(case\|default\)\>'),'nW'.s:z)
endfunction

" https://github.com/sweet-js/sweet.js/wiki/design#give-lookbehind-to-the-reader
function s:IsBlock()
  let tok = s:PreviousToken()
  if join(s:stack) =~? 'xml\|jsx' && s:SynAt(line('.'),col('.')-1) =~? 'xml\|jsx'
    let s:in_jsx = 1
    return tok != '{'
  elseif tok =~ '\k'
    if tok ==# 'type'
      return s:Pure('eval',"s:PreviousToken() !~# '^\\%(im\\|ex\\)port$' || s:PreviousToken() == '.'")
    elseif tok ==# 'of'
      return s:Pure('eval',"!s:GetPair('[[({]','[])}]','bW',s:skip_expr) || s:LookingAt() != '(' ||"
            \ ."s:{s:PreviousToken() ==# 'await' ? 'Previous' : ''}Token() !=# 'for' || s:PreviousToken() == '.'")
    endif
    return index(split('return const let import export extends yield default delete var await void typeof throw case new in instanceof')
          \ ,tok) < (line('.') != a:firstline) || s:Pure('s:PreviousToken') == '.'
  elseif tok == '>'
    return getline('.')[col('.')-2] == '=' || s:SynAt(line('.'),col('.')) =~? 'jsflow\|^html'
  elseif tok == '*'
    return s:Pure('s:PreviousToken') == ':'
  elseif tok == ':'
    return s:Pure('eval',"s:PreviousToken() =~ '^\\K\\k*$' && !s:ExprCol()")
  elseif tok == '/'
    return s:SynAt(line('.'),col('.')) =~? 'regex'
  elseif tok !~ '[=~!<,.?^%|&([]'
    return tok !~ '[-+]' || line('.') != a:firstline && getline('.')[col('.')-2] == tok
  endif
endfunction

function GetJavascriptIndent()
  call s:GetVars()
  let s:synid_cache = [[],[]]
  let l:line = getline(v:lnum)
  " use synstack as it validates syn state and works in an empty line
  let s:stack = [''] + map(synstack(v:lnum,1),"synIDattr(v:val,'name')")

  " start with strings,comments,etc.
  if s:stack[-1] =~? 'comment\|doc'
    if l:line !~ '^\s*\/[/*]'
      return l:line =~ '^\s*\*' ? cindent(v:lnum) : -1
    endif
  elseif s:stack[-1] =~? b:syng_str
    if b:js_cache[0] == v:lnum - 1 && s:Balanced(v:lnum-1,getline(v:lnum-1))
      let b:js_cache[0] = v:lnum
    endif
    return -1
  endif

  let nest = get(get(b:,'hi_indent',{}),'blocklnr')
  let s:l1 = max([0, prevnonblank(v:lnum) - (s:rel ? 2000 : 1000), nest])
  call cursor(v:lnum,1)
  if s:PreviousToken() is ''
    return
  endif
  let [l:lnum, lcol, pline] = getpos('.')[1:2] + [getline('.')[:col('.')-1]]

  let l:line = substitute(l:line,'^\s*','','')
  let l:line_s = l:line[0]
  if l:line[:1] == '/*'
    let l:line = substitute(l:line,'^\%(\/\*.\{-}\*\/\s*\)*','','')
  endif
  if l:line =~ '^\/[/*]'
    let l:line = ''
  endif

  " the containing paren, bracket, or curly. Many hacks for performance
  call cursor(v:lnum,1)
  let idx = index([']',')','}'],l:line[0])
  if b:js_cache[0] > l:lnum && b:js_cache[0] < v:lnum ||
        \ b:js_cache[0] == l:lnum && s:Balanced(l:lnum,pline)
    call call('cursor',b:js_cache[1:])
  else
    let [s:looksyn, s:top_col, s:check_in, s:l1] = [v:lnum - 1,0,0,
          \ max([s:l1, &smc ? search('\m^.\{'.&smc.',}','nbW',s:l1 + 1) + 1 : 0])]
    try
      if idx != -1
        call s:GetPair(['\[','(','{'][idx],'])}'[idx],'bW','s:SkipFunc()')
      elseif getline(v:lnum) !~ '^\S' && s:stack[-1] =~? 'block\|^jsobject$'
        call s:GetPair('{','}','bW','s:SkipFunc()')
      else
        call s:AlternatePair()
      endif
    catch /^\Cout of bounds$/
      call cursor(v:lnum,1)
    endtry
    let b:js_cache[1:] = line('.') == v:lnum ? [0,0] : getpos('.')[1:2]
  endif

  let [b:js_cache[0], num] = [v:lnum, b:js_cache[1]]

  let [num_ind, is_op, b_l, l:switch_offset, s:in_jsx] = [s:Nat(indent(num)),0,0,0,0]
  if !num || s:LookingAt() == '{' && s:IsBlock()
    let ilnum = line('.')
    if num && !s:in_jsx && s:LookingAt() == ')' && s:GetPair('(',')','bW',s:skip_expr)
      if ilnum == num
        let [num, num_ind] = [line('.'), indent('.')]
      endif
      if idx == -1 && s:PreviousToken() ==# 'switch' && s:IsSwitch()
        let l:switch_offset = &cino !~ ':' ? s:sw() : s:ParseCino(':')
        if pline[-1:] != '.' && l:line =~# '^\%(default\|case\)\>'
          return s:Nat(num_ind + l:switch_offset)
        elseif &cino =~ '='
          let l:case_offset = s:ParseCino('=')
        endif
      endif
    endif
    if idx == -1 && pline[-1:] !~ '[{;]'
      call cursor(l:lnum, lcol)
      let sol = matchstr(l:line,s:opfirst)
      if sol is '' || sol == '/' && s:SynAt(v:lnum,
            \ 1 + len(getline(v:lnum)) - len(l:line)) =~? 'regex'
        if s:Continues()
          let is_op = s:sw()
        endif
      elseif num && sol =~# '^\%(in\%(stanceof\)\=\|\*\)$' &&
            \ s:LookingAt() == '}' && s:GetPair('{','}','bW',s:skip_expr) &&
            \ s:PreviousToken() == ')' && s:GetPair('(',')','bW',s:skip_expr) &&
            \ (s:PreviousToken() == ']' || s:LookingAt() =~ '\k' &&
            \ s:{s:PreviousToken() == '*' ? 'Previous' : ''}Token() !=# 'function')
        return num_ind + s:sw()
      else
        let is_op = s:sw()
      endif
      call cursor(l:lnum, lcol)
      let b_l = s:Nat(s:IsContOne(is_op) - (!is_op && l:line =~ '^{')) * s:sw()
    endif
  elseif idx == -1 && s:LookingAt() == '(' && &cino =~ '(' &&
        \ (search('\m\S','nbW',num) || s:ParseCino('U'))
    let pval = s:ParseCino('(')
    if !pval
      let [Wval, vcol] = [s:ParseCino('W'), virtcol('.')]
      if search('\m'.get(g:,'javascript_indent_W_pat','\S'),'W',num)
        return s:ParseCino('w') ? vcol : virtcol('.')-1
      endif
      return Wval ? s:Nat(num_ind + Wval) : vcol
    endif
    return s:Nat(num_ind + pval + searchpair('\m(','','\m)','nbrmW',s:skip_expr,num) * s:sw())
  endif

  " main return
  if l:line =~ '^[])}]\|^|}'
    if l:line_s == ')'
      if s:ParseCino('M')
        return indent(l:lnum)
      elseif num && &cino =~# 'm' && !s:ParseCino('m')
        return virtcol('.') - 1
      endif
    endif
    return num_ind
  elseif num
    return s:Nat(num_ind + get(l:,'case_offset',s:sw()) + l:switch_offset + b_l + is_op)
  elseif nest
    return indent(nextnonblank(nest+1)) + b_l + is_op
  endif
  return b_l + is_op
endfunction

let &cpo = s:cpo_save
unlet s:cpo_save

endif
if !exists('g:polyglot_disabled') || index(g:polyglot_disabled, 'jsx') == -1
  
" Vim indent file
" Language: Javascript
" Maintainer: Chris Paul ( https://github.com/bounceme )
" URL: https://github.com/pangloss/vim-javascript
" Last Change: December 1, 2016

" Only load this indent file when no other was loaded.
if exists('b:did_indent')
  finish
endif
let b:did_indent = 1

" Now, set up our indentation expression and keys that trigger it.
setlocal indentexpr=GetJavascriptIndent()
setlocal autoindent nolisp nosmartindent
setlocal indentkeys+=0],0)

let b:undo_indent = 'setlocal indentexpr< smartindent< autoindent< indentkeys<'

" Only define the function once.
if exists('*GetJavascriptIndent')
  finish
endif

let s:cpo_save = &cpo
set cpo&vim

" Get shiftwidth value
if exists('*shiftwidth')
  function s:sw()
    return shiftwidth()
  endfunction
else
  function s:sw()
    return &sw
  endfunction
endif

let s:case_stmt = '\%(\.\s*\)\@<!\<\%(case\>\s*[^ \t:].*\|default\s*\):\C'

" Regex of syntax group names that are or delimit string or are comments.
let s:syng_strcom = 'string\|comment\|regex\|special\|doc\|template'
" Expression used to check whether we should skip a match with searchpair().
let s:skip_expr = "synIDattr(synID(line('.'),col('.'),0),'name') =~? '".s:syng_strcom."'"
function s:skip_func(lnum)
  if !s:free || search('`\|\*\/','nW',a:lnum)
    let s:free = !eval(s:skip_expr)
    let s:looksyn = s:free ? line('.') : s:looksyn
    return !s:free
  endif
  let s:looksyn = line('.')
  return (search('\/','nbW',s:looksyn) || search('[''"\\]','nW',s:looksyn)) && eval(s:skip_expr)
endfunction

if has('reltime')
  function s:GetPair(start,end,flags,skip,time,...)
    return searchpair(a:start,'',a:end,a:flags,a:skip,max([prevnonblank(v:lnum) - 2000,0] + a:000),a:time)
  endfunction
else
  function s:GetPair(start,end,flags,skip,...)
    return searchpair(a:start,'',a:end,a:flags,a:skip,max([prevnonblank(v:lnum) - 1000,get(a:000,1)]))
  endfunction
endif

function s:looking_at()
  return getline('.')[col('.')-1]
endfunction

function s:token()
  return s:looking_at() =~ '\k' ? expand('<cword>') : s:looking_at()
endfunction

" NOTE: moves the cursor
function s:previous_token()
  let l:ln = line('.')
  return search('.\>\|[^[:alnum:][:space:]_$]','bW') ?
        \ (s:looking_at() == '/' || line('.') != l:ln && getline('.') =~ '\/\/') &&
        \ synIDattr(synID(line('.'),col('.'),0),'name') =~? 'comment' ?
        \ search('\_[^/]\zs\/[/*]','bW') ? s:previous_token() : ''
        \ : s:token()
        \ : ''
endfunction

" configurable regexes that define continuation lines, not including (, {, or [.
let s:opfirst = '^' . get(g:,'javascript_opfirst',
      \ '\%([<>=,?^%|*/&]\|\([-.:+]\)\1\@!\|!=\|in\%(stanceof\)\=\>\)')
let s:continuation = get(g:,'javascript_continuation',
      \ '\%([<=,.~!?/*^%|&:]\|+\@<!+\|-\@<!-\|=\@<!>\|\%(\.\s*\)\@<!\<\%(typeof\|delete\|void\|in\|instanceof\)\)') . '$'

" get the line of code stripped of comments. if called with two args, leave
" cursor at the last non-comment char.
function s:Trim(ln,...)
  let pline = substitute(getline(a:ln),'\s*$','','')
  let l:max = max([match(pline,'.*[^/]\zs\/[/*]'),0])
  while l:max && synIDattr(synID(a:ln, strlen(pline), 0), 'name') =~? 'comment\|doc'
    let pline = substitute(strpart(pline, 0, l:max),'\s*$','','')
    let l:max = max([match(pline,'.*[^/]\zs\/[/*]'),0])
  endwhile
  return !a:0 || cursor(a:ln,strlen(pline)) ? pline : pline
endfunction

function s:OneScope(lnum)
  let pline = s:Trim(a:lnum,1)
  if pline[-1:] == ')' && s:GetPair('(', ')', 'bW', s:skip_expr, 100) > 0
    let token = s:previous_token()
    if index(split('await each'),token) + 1
      return s:previous_token() ==# 'for'
    endif
    return index(split('for if let while with'),token) + 1
  endif
  return pline =~# '\%(\%(\.\s*\)\@<!\<\%(else\|do\)\|=>\)$'
endfunction

function s:iscontOne(i,num,cont)
  let [l:i, l:cont, l:num] = [a:i, a:cont, a:num + !a:num]
  let pind = a:num ? indent(l:num) + s:W : 0
  let ind = indent(l:i) + (a:cont ? 0 : s:W)
  let bL = 0
  while l:i >= l:num && (!l:cont || ind > pind)
    if indent(l:i) < ind " first line always true for !a:cont, false for !!a:cont
      if s:OneScope(l:i)
        let bL += s:W
        let [l:cont, l:i] = [0, line('.')]
      elseif !l:cont
        break
      endif
    elseif !a:cont
      break
    endif
    let ind = min([ind, indent(l:i)])
    let l:i = s:PrevCodeLine(l:i - 1)
  endwhile
  return bL
endfunction

" https://github.com/sweet-js/sweet.js/wiki/design#give-lookbehind-to-the-reader
function s:IsBlock()
  let l:ln = line('.')
  let char = s:previous_token()
  let syn = char =~ '[{>/]' ? synIDattr(synID(line('.'),col('.')-(char == '{'),0),'name') : ''
  if syn =~? 'xml\|jsx'
    return char != '{'
  elseif char =~ '\k'
    return index(split('return const let import export yield default delete var void typeof throw new in instanceof')
          \ ,char) < (0 + (line('.') != l:ln)) || s:previous_token() == '.'
  elseif char == '>'
    return getline('.')[col('.')-2] == '=' || syn =~? '^jsflow'
  elseif char == ':'
    return !cursor(0,match(' ' . strpart(getline('.'),0,col('.')),'.*\zs' . s:case_stmt . '$')) &&
          \ (expand('<cword>') !=# 'default' || s:previous_token() !~ '[,{]')
  endif
  return syn =~? 'regex' || char !~ '[-=~!<*+,/?^%|&([]'
endfunction

" Find line above 'lnum' that isn't empty or in a comment
function s:PrevCodeLine(lnum)
  let l:n = prevnonblank(a:lnum)
  while getline(l:n) =~ '^\s*\/[/*]' || synIDattr(synID(l:n,1,0),'name') =~?
        \ 'comment\|doc'
    let l:n = prevnonblank(l:n-1)
  endwhile
  return l:n
endfunction

" Check if line 'lnum' has a balanced amount of parentheses.
function s:Balanced(lnum)
  let l:open = 0
  let l:line = getline(a:lnum)
  let pos = match(l:line, '[][(){}]', 0)
  while pos != -1
    if synIDattr(synID(a:lnum,pos + 1,0),'name') !~? s:syng_strcom
      let l:open += match(' ' . l:line[pos],'[[({]')
      if l:open < 0
        return
      endif
    endif
    let pos = match(l:line, '[][(){}]', pos + 1)
  endwhile
  return !l:open
endfunction

function GetJavascriptIndent()
  let b:js_cache = get(b:,'js_cache',[0,0,0])
  " Get the current line.
  let l:line = getline(v:lnum)
  let syns = synIDattr(synID(v:lnum, 1, 0), 'name')

  " start with strings,comments,etc.
  if syns =~? 'comment\|doc'
    if l:line =~ '^\s*\*'
      return cindent(v:lnum)
    elseif l:line !~ '^\s*\/[/*]'
      return -1
    endif
  elseif syns =~? 'string\|template' && l:line !~ '^[''"]'
    return -1
  endif
  let l:lnum = s:PrevCodeLine(v:lnum - 1)
  if !l:lnum
    return
  endif
  call cursor(v:lnum,1)

  let l:line = l:line[indent('.'):]
  if l:line[:1] == '/*'
    let l:line = substitute(l:line,'^\%(\/\*.\{-}\*\/\s*\)*','','')
  endif
  if l:line =~ '^\/[/*]'
    let l:line = ''
  endif

  " the containing paren, bracket, or curly. Many hacks for performance
  let idx = strlen(l:line) ? stridx('])}',l:line[0]) : -1
  if indent(l:lnum)
    let [s:looksyn,s:free] = [v:lnum - 1,1]
    if b:js_cache[0] >= l:lnum && b:js_cache[0] < v:lnum &&
          \ (b:js_cache[0] > l:lnum || s:Balanced(l:lnum))
      call call('cursor',b:js_cache[1:])
    elseif idx + 1
      call s:GetPair(['\[','(','{'][idx], '])}'[idx],'bW','s:skip_func(s:looksyn)',2000)
    elseif indent(v:lnum) && syns =~? 'block'
      call s:GetPair('{','}','bW','s:skip_func(s:looksyn)',2000)
    else
      call s:GetPair('[({[]','[])}]','bW','s:skip_func(s:looksyn)',2000)
    endif
  else
    call s:GetPair('[({[]','[])}]','bW',s:skip_expr,200,l:lnum)
  endif

  if idx + 1
    if idx == 2 && search('\S','bW',line('.')) && s:looking_at() == ')'
      call s:GetPair('(',')','bW',s:skip_expr,200)
    endif
    return indent('.')
  endif

  let b:js_cache = [v:lnum] + (line('.') == v:lnum ? [0,0] : getpos('.')[1:2])
  let num = b:js_cache[1]

  let [s:W, isOp, bL, switch_offset] = [s:sw(),0,0,0]
  if !num || s:looking_at() == '{' && s:IsBlock()
    let pline = s:Trim(l:lnum)
    if num && s:looking_at() == ')' && s:GetPair('(', ')', 'bW', s:skip_expr, 100) > 0
      let num = line('.')
      if s:previous_token() ==# 'switch'
        let switch_offset = &cino !~ ':' || !has('float') ? s:W :
              \ float2nr(str2float(matchstr(&cino,'.*:\zs[-0-9.]*')) * (&cino =~# '\%(.*:\)\@>[^,]*s' ? s:W : 1))
        if l:line =~# '^' . s:case_stmt
          return indent(num) + switch_offset
        elseif pline =~# s:case_stmt . '$'
          return indent(l:lnum) + s:W
        endif
      endif
    endif
    if pline[-1:] !~ '[{;]'
      let isOp = l:line =~# s:opfirst || pline =~# s:continuation &&
            \ synIDattr(synID(l:lnum,match(' ' . pline,'\/$'),0),'name') !~? 'regex'
      let bL = s:iscontOne(l:lnum,num,isOp)
      let bL -= (bL && l:line[0] == '{') * s:W
    endif
  endif

  " main return
  if isOp
    return (num ? indent(num) : -s:W) + (s:W * 2) + switch_offset + bL
  elseif num
    return indent(num) + s:W + switch_offset + bL
  endif
  return bL
endfunction

let &cpo = s:cpo_save
unlet s:cpo_save

endif
