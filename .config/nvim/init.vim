let g:mapleader = "\<Space>"

call plug#begin('~/.vim/plugged')

Plug 'scrooloose/nerdtree'
  let NERDTreeIgnore = ['\.pyc$', '\.hi$', '\.o$', '\.dyn_hi$', '\.dyn_o$']
  let NERDTreeQuitOnOpen = 1
  nnoremap <BS> :NERDTreeToggle <CR>
  nnoremap <S-BS> :TagbarToggle <CR>

Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'
  let g:fzf_nvim_statusline = 0 " disable statusline overwriting
  nnoremap <silent> <leader><space> :Files<CR>
  nnoremap <silent> <leader>bf :Buffers<CR>
  nnoremap <silent> <leader>wf :Windows<CR>
  imap <C-x><C-f> <plug>(fzf-complete-file-ag)
  imap <C-x><C-l> <plug>(fzf-complete-line)

Plug 'justinmk/vim-sneak'

Plug 'tpope/vim-surround'

Plug 'lambdatoast/elm.vim'

Plug 'udalov/kotlin-vim'

Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
  let g:deoplete#enable_at_startup = 1
  inoremap <expr><C-g> deoplete#manual_complete()

  if !exists('g:deoplete#omni#input_patterns')
    let g:deoplete#omni#input_patterns = {}
  endif
  " let g:deoplete#disable_auto_complete = 1
  autocmd InsertLeave,CompleteDone * if pumvisible() == 0 | pclose | endif

  " deoplete tab-complete
  inoremap <expr><tab> pumvisible() ? "\<c-n>" : "\<tab>"

  " omnifuncs
  augroup omnifuncs
    autocmd!
    autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
    autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
    autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
    autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
    autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags
  augroup end

  Plug 'morhetz/gruvbox'

call plug#end()


colorscheme chiptheme
"set termguicolors
"set background=dark

" Hide buffers instead of closing them
set hidden

" New horizontal splits appear below current window
set splitbelow

" New vertical splits appear to the right of current window
set splitright

" Set the minimum amount of visible lines above/under
" the cursor to 3 (default is 0)
set scrolloff=3

" Highlight the current cursor line
set cursorline

" Show line and character number
set ruler

" Set relativenumber when not in insert mode - numbers otherwise
set relativenumber
set number

" Set tab options
set expandtab
set shiftwidth=2
set softtabstop=2
set tabstop=2


" Window navigation keybindings
:tnoremap <C-h> <C-\><C-n><C-w>h
:tnoremap <C-j> <C-\><C-n><C-w>j
:tnoremap <C-k> <C-\><C-n><C-w>k
:tnoremap <C-l> <C-\><C-n><C-w>l
:nnoremap <C-h> <C-w>h
:nnoremap <C-j> <C-w>j
:nnoremap <C-k> <C-w>k
:nnoremap <C-l> <C-w>l
:tnoremap <C-Space> <C-\><C-n>

" Window manipulation keybindings
:nnoremap <leader>wv <C-w>v
:nnoremap <leader>ws <C-w>s
:nnoremap <leader>wd <C-w>c
:nnoremap <leader>wts <C-w>s:terminal<CR>
:nnoremap <leader>wtv <C-w>v:terminal<CR>

" Buffer manipulation keybindings
:nnoremap <leader>bd :bd<CR>
:tnoremap <C-c> <C-\><C-n>:bd!<CR>
