
" File: closepair.vim
" Author: meese
" Version: 0.42
"
" Description: Automatically inserts closing pairs of (, [, ', etc.
"              when appropriate. Enjoy :)
if exists('s:closepair')
	fini
en
let s:closepair = 1

ino <expr> ( <SID>OpenPair('(', ')')
ino <expr> [ <SID>OpenPair('[', ']')
ino <expr> { <SID>OpenPair('{', '}')
ino <expr> < <SID>OpenPair('<', '>')
ino <expr> ) <SID>ClosePair(')')
ino <expr> ] <SID>ClosePair(']')
ino <expr> } <SID>ClosePair('}')
ino <expr> > <SID>ClosePair('>')
ino <expr> " <SID>QuoteDelim('"')
ino <expr> ' <SID>QuoteDelim("'")
ino <expr> ` <SID>QuoteDelim('`')
ino <expr> <silent> <bs> <SID>RemovePair()

" disable " completion in .vim files
au FileType vim ino <buffer> " "

" Only autocompletes only if no characters are in front of the cursor
fun s:OpenPair(char, char2)
	" Don't autocomplete in a string
	if getline('.')[col('.') - 1] =~ '\w'
		retu a:char
	el
		retu a:char.a:char2."\<left>"
	en
endf

" allows you to escape out of an autocompletion by simply typing
" the closing pair
fun s:ClosePair(char)
	if getline('.')[col('.') - 1] == a:char
		retu "\<right>"
	el
		retu a:char
	en
endf

" automatically remove the entire autocompletion when the opening character
" is deleted & there are no characters in between
fun s:RemovePair()
	let line = getline('.')
	let col = col('.') - 1
	let chars = line[col - 1].line[col]

 	if chars =~ '\(()\|{}\|\[\]\|<>\|""\|''''\)'
		retu "\<right>\<bs>\<bs>"
	el
		retu "\<bs>"
	en
endf

fun! s:Count(haystack, needle)
    let counter = 0
    let index = stridx(a:haystack, a:needle)
    wh index > -1
        let counter += 1
        let index = stridx(a:haystack, a:needle, index+1)
    endw
    retu counter
endf

" Like OpenPair/ClosePair but for quotes
fun s:QuoteDelim(char)
	let col = col('.')-1

	" Don't autocomplete single quotes inside comments; they could
	" be apostrophes
	if a:char == "'" && synIDattr(synID(line('.'), col, 0), 'name') =~ 'omment'
		retu a:char
	en

	let line = getline('.')
	let currentChar = line[col]

	let c = s:Count(line, a:char)
	if currentChar == a:char && &ft != 'python'
		" Escape out of the string
		retu "\<right>"
	" fix python strings
	elsei &ft == 'python' && c && !(c % 3) && line[col] == '"'
		retu "\<right>\<right>\<right>"
	elsei currentChar =~ '\w' || c % 2 || line[col - 1] == '\'
		" Matching quote, inserting into string, or escaping
		retu a:char
	el
		" Start a string
		retu a:char.a:char."\<esc>i"
	en
endf
