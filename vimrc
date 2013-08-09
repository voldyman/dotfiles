" Use `ii` instead of Esc
" `i` => insert
" `ii` => cmd mode
inoremap ii <Esc>

" Automatic reloading of .vimrc
autocmd! bufwritepost .vimrc source %

" Vala syntax highlighting
autocmd BufRead *.vala,*.vapi set efm=%f:%l.%c-%[%^:]%#:\ %t%[%^:]%#:\ %m
au BufRead,BufNewFile *.vala,*.vapi setfiletype vala

" Better copy & paste
" When you want to paste large blocks of code into vim, press F2 before you
" paste. At the bottom you should see ``-- INSERT (paste) --``.

set pastetoggle=<F2>
" set clipboard=unnamedplus

" To paste from system clipboard use <leader> p
nnoremap <Leader>p "+p

" Mouse and backsp ace
set mouse=a  " on OSX press ALT and click
set bs=2     " make backspace behave like normal again

" insert blank lines without going into insert mode
nmap t o<ESC>k
nmap T O<ESC>j

" Rebind <Leader> key
" I like to have it here becuase it is easier to reach than the default and
" it is next to ``m`` and ``n`` which I use for navigating between tabs.
let mapleader = "\\"

" easily quit
map <Leader>k :q<cr>
" Bind nohl
" Removes highlight of your last search
" ``<C>`` stands for ``CTRL`` and therefore ``<C-n>`` stands for ``CTRL+n``
noremap <C-n> :nohl<CR>
vnoremap <C-n> :nohl<CR>
inoremap <C-n> :nohl<CR>


" Quicksave command
noremap <C-Z> :update<CR>
vnoremap <C-Z> <C-C>:update<CR>
inoremap <C-Z> <C-O>:update<CR>


" Quick quit command
noremap <Leader>e :quit<CR>  " Quit current window
noremap <Leader>E :qa!<CR>   " Quit all windows

" Remove all trailing white space
nnoremap <leader>W :%s/\s\+$//<cr>:let @/=''<cr>

" select all
map <Leader>a ggVG

" bind Ctrl+<movement> keys to move around the windows, instead of using Ctrl+w + <movement>
" Every unnecessary keystroke that can be saved is good for your health :)
map <c-j> <c-w>j
map <c-k> <c-w>k
map <c-l> <c-w>l
map <c-h> <c-w>h


" New tab shortcut
nnoremap <Leader>t :tabnew<cr>

" easier moving between tabs
map <Leader>n <esc>:tabprevious<CR>
map <Leader>m <esc>:tabnext<CR>

" remap U to <C-r> for easier redo
nnoremap U <C-r>

" map sort function to a key
vnoremap <Leader>s :sort<CR>


" easier moving of code blocks
" Try to go into visual mode (v), thenselect several lines of code here and
" then press ``>`` several times.
vnoremap < <gv  " better indentation
vnoremap > >gv  " better indentation


" Show whitespace
" MUST be inserted BEFORE the colorscheme command
autocmd ColorScheme * highlight ExtraWhitespace ctermbg=red guibg=red
au InsertLeave * match ExtraWhitespace /\s\+$/

" Auto reload file when changed outside vim
set autoread

" display space charsi
set list
set listchars=tab:▸~,trail:⋅,extends:❯,precedes:❮
set showbreak=↪"

" Color scheme
" mkdir -p ~/.vim/colors && cd ~/.vim/colors
" wget -O wombat256mod.vim http://www.vim.org/scripts/download_script.php?src_id=13400
" color wombat256mod
set t_Co=256
" color scheme
" mkdir -p ~/.vim/colors && cd ~/.vim/colors
" curl -O https://github.com/nanotech/jellybeans.vim
color jellybeans
" Color scheme
" mkdir -p ~/.vim/colors && cd ~/.vim/colors
" wget -O vombato.vim https://raw.github.com/molok/vim-vombato-colorscheme/master/colors/vombato.vim
" color vombato

" Enable syntax highlighting
" You need to reload this file for the change to apply
filetype off
filetype plugin indent on
syntax on
set autoindent
set smartindent

" Showing line numbers and length
set number  " show line numbers
set tw=79   " width of document (used by gd)
set nowrap  " don't automatically wrap on load
set fo-=t   " don't automatically wrap text when typing
set colorcolumn=80
highlight ColorColumn ctermbg=233


" easier formatting of paragraphs
vmap Q gq
nmap Q gqap


" Useful settings
set history=700
set undolevels=700


" Real programmers don't use TABs but spaces
set tabstop=4
set softtabstop=4
set shiftwidth=4
set shiftround
set expandtab


" Make search case insensitive
set hlsearch
set incsearch
set ignorecase
set smartcase


" Disable stupid backup and swap files - they trigger too many events
" for file system watchers
set nobackup
set nowritebackup
set noswapfile


" Setup Pathogen to manage your plugins
" mkdir -p ~/.vim/autoload ~/.vim/bundle
" curl -so ~/.vim/autoload/pathogen.vim https://raw.github.com/tpope/vim-pathogen/HEAD/autoload/pathogen.vim
" Now you can install any plugin into a .vim/bundle/plugin-name/ folder
call pathogen#infect()
