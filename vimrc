" General Settings

"" Not compatible with the old-fashion vi mode
set nocompatible

execute pathogen#infect()

"" Allow backspacing over everything in insert mode
set bs=2
"" Keep 50 lines of command line history
set history=50
"" Show the cursor position all the time
set ruler
"" Auto read when file is changed from outside
set autoread

"" Necessary to make ftdetect work on Linux
filetype off
"" syntax highlight
syntax on
"" Enable filetype detection
filetype on
"" Enable filetype-specific indenting
filetype indent on
"" Enable filetype-specific plugins
filetype plugin on
"" Search highlighting
set hlsearch

set number

set background=dark
"colorscheme solarized

"" Auto reload vimrc when editing it
autocmd! bufwritepost .vimrc source ~/.vimrc

"" Cursor shows matching ) and }
set showmatch
"" Show current mode
set showmode
set wildchar=<TAB>	" start wild expansion in the command line using <TAB>
set wildmenu            " wild char completion menu
"" Ignore these files while expanding wild chars
set wildignore=*.o,*.class,*.pyc

"" Auto indentation
set autoindent
"" Copy the previous indentation on autoindenting
set copyindent

"" No *~ backup files
set nobackup

"" Incremental search
set incsearch
"" Ignore case when searching
set ignorecase
"" Ignore case if search pattern is all lowercase,case-sensitive otherwise
set smartcase

"" Insert tabs on the start of a line according to context
set smarttab
set smartindent
set tabstop=4
set shiftwidth=4

" disable sound on errors
set noerrorbells
set novisualbell
set t_vb=
set tm=500

""  What to save on exit and restore on start.
""  |%|   Save and restore the buffers list.
""  |'20| Remember marks for last 20 files.
""  |/20| Remember 20 items in search pattern history.
""  |:20| Remember 20 items in the command-line history.
""  |s10| Save registers that are < 10kb of text.
set viminfo=%,'20,/20,:20,s10

"Restore cursor to file position in previous editing session
set viminfo='10,\"100,:20,%,n~/.viminfo
au BufReadPost * if line("'\"") > 0|if line("'\"") <= line("$")|exe("norm '\"")|else|exe "norm $"|endif|endif

match ErrorMsg '\s\+$'

" status line {
set laststatus=2
set statusline=\ %{HasPaste()}%<%-15.25(%f%)%m%r%h\ %w\ \
set statusline+=\ \ \ [%{&ff}/%Y]
set statusline+=\ \ \ %<%20.30(%{hostname()}:%{CurDir()}%)\
set statusline+=%=%-10.(%l,%c%V%)\ %p%%/%L

function! CurDir()
    let curdir = substitute(getcwd(), $HOME, "~", "")
    return curdir
endfunction

function! HasPaste()
    if &paste
        return '[PASTE]'
    else
        return ''
    endif
endfunction

"}

" C/C++ specific settings
autocmd FileType c,cpp,cc  set cindent comments=sr:/*,mb:*,el:*/,:// cino=>s,e0,n0,f0,{0,}0,^-1s,:0,=s,g0,h1s,p2,t0,+2,(2,)20,*30

" Ctags recursive search
set tags=./tags;/
au BufWritePost *.c,*.cpp,*.h, silent! !ctags -R * &
au BufWritePost *.go silent! !gotags -R -f=tags . &

" Autoformat C/C++ files on save with vim-autoformat
au BufWrite *.c,*.cpp,*.h :Autoformat

set colorcolumn=81
highlight OverLength ctermbg=red ctermfg=white guibg=#592929
highlight ExtraWhitespace ctermbg=red guibg=red
highlight TabAndSpaces ctermbg=blue guibg=blue

:au BufWinEnter * let w:moverlength=matchadd('OverLength', '\%>80v.\+', -1)
:au BufWinEnter * let w:wtrailspaces=matchadd('ExtraWhitespace', '\s\+$', -1)
:au BufWinEnter * let w:mtabbeforesp=matchadd('TabAndSpaces', '\v(\t+)\ze( +)', -1)
:au BufWinEnter * let w:mtabaftersp=matchadd('TabAndSpaces', '\v( +)\zs(\t+)', -1)


"---------------------------------------------------------------------------
"" Key bindings
"---------------------------------------------------------------------------
" Leader key
let mapleader="\\"

" toogle paste button
set pastetoggle=<F10>
" TagbarToggle

nmap <F8> :TagbarToggle<CR>

let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'

" allow multiple indentation/deindentation in visual mode
vnoremap < <gv
vnoremap > >gv

" Commenting blocks of code.
autocmd FileType go,c,cpp,java,scala let b:comment_leader = '// '
autocmd FileType sh,ruby,python   let b:comment_leader = '# '
autocmd FileType conf,fstab       let b:comment_leader = '# '
autocmd FileType tex              let b:comment_leader = '% '
autocmd FileType mail             let b:comment_leader = '> '
autocmd FileType vim              let b:comment_leader = '" '
noremap <silent> ,cc :<C-B>silent <C-E>s/^/<C-R>=escape(b:comment_leader,'\/')<CR>/<CR>:nohlsearch<CR>
noremap <silent> ,cu :<C-B>silent <C-E>s/^\V<C-R>=escape(b:comment_leader,'\/')<CR>//e<CR>:nohlsearch<CR>

"---------------------------------------------------------------------------
" ENCODING SETTINGS
"---------------------------------------------------------------------------
set encoding=utf-8
set termencoding=utf-8
set fileencoding=utf-8
set fileencodings=ucs-bom,utf-8,big5,gb2312,latin1

fun! ViewUTF8()
	set encoding=utf-8
	set termencoding=big5
endfun

fun! UTF8()
	set encoding=utf-8
	set termencoding=big5
	set fileencoding=utf-8
	set fileencodings=ucs-bom,big5,utf-8,latin1
endfun

fun! Big5()
	set encoding=big5
	set fileencoding=big5
endfun

" Nerd Tree
map <C-n> :NERDTreeToggle<CR>

" air-line

"let g:airline_enable_fugitive=0
let g:airline#extensions#bufferline#enabled = 1
let g:airline_powerline_fonts = 1
let g:airline#extensions#tabline#enabled = 1

