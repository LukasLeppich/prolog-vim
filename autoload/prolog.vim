if !has("python") && !has("python3")
    echo "Python is required!"
	finish
endif

let s:serverStarted = 0
let s:plugin = expand("<sfile>:p:h:h")
let s:scriptFile = s:plugin . '/script/client_wrapper.py'
let s:projectDir = expand("%:p:h")

if !exists("g:prolog#command")
	let g:prolog#command = [
				\ "swipl",
				\ "-f", s:plugin. "/script/server/lsp_language_server.pl",
				\ "-t", "start('test.log')",
				\ "--nodebug",
				\ "--nosignal",
				\ "-q"
		        \ ]
endif

function! prolog#enable()
	" echom "start file"
	let b:prologCompletionList=[]
	let b:prologCompletionStart=-2

	if s:serverStarted == 0
		call prolog#prologStartup()
		let s:serverStarted = 1
	endif

	augroup PrologAutoCmd
		autocmd! * <buffer>
		autocmd BufNewFile,BufRead <buffer> :call s:prologOnOpen()
		autocmd BufDelete <buffer> :call s:prologOnClose()
		autocmd CursorMovedI <buffer> :call s:prologOnChange()
	augroup END

	" echom "Set omnifunc"
	setlocal omnifunc=prolog#omnifunc

	call s:prologOnOpen()
endfunction

augroup PrologShoutdown
	autocmd VimLeave * call prolog#prologShutdown()
augroup END

function! prolog#omnifunc(findStart, complWord)
	if a:findStart
		call prolog#prologComplete()
		return b:prologCompletionStart
	else
		return b:prologCompletionList
	endif
endfunction

function! prolog#prologStartup()
	" startup prolog server"
	if has("python3")
		execute 'py3file' . fnameescape(s:scriptFile)
		python3 init_project(vim.eval('s:projectDir'))
		if !prolog#isServerRunning()
			python3 start_server(vim.eval('g:prolog#command'))
		endif
	else
		execute 'pyfile' . fnameescape(s:scriptFile)
		python init_project(vim.eval('s:projectDir'))
		if !prolog#isServerRunning()
			python3 start_server(vim.eval('g:prolog#command'))
		endif
	endif
endfunction

function! prolog#isServerRunning()
	" echom "is server running"
	if has("python3")
		python3 is_server_running()
	else
		python is_server_running()
	endif
endfunction


function! prolog#prologShutdown()
	" echom "prolog shutdown"
	if has("python3")
		python3 stop_server()
	else
		python stop_server()
	endif
endfunction

function! s:prologOnChange()
	let l:file = expand('%:p')
	let l:content = join(getline(1,'$'), "\n")
	if has("python3")
		python3 did_change(vim.eval('l:file'), vim.eval('l:content'))
	else
		python did_change(vim.eval('l:file'), vim.eval('l:content'))
	endif
endfunction

function! s:prologOnOpen()
	let l:file = expand('%:p')
	let l:content = join(getline(1,'$'), "\n")
	" echom "OnOpen"
	if has("python3")
		python3 did_open(vim.eval('l:file'), vim.eval('l:content'))
	else
		python did_open(vim.eval('l:file'), vim.eval('l:content'))
	endif
endfunction

function! prolog#prologComplete()
	let l:file = expand('%:p')
	let l:line = line('.')
	let l:col = col('.') 
	if has("python3")
		python3 completion(vim.eval('l:file'), vim.eval('l:line'), vim.eval('l:col'))
	else
		python completion(vim.eval('l:file'), vim.eval('l:line'), vim.eval('l:col'))
	endif
endfunction
	

function! s:prologOnClose()
	" echom "onClose"
endfunction
