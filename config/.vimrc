execute pathogen#infect()

" column and line
:set ruler    
" show command as it's being typed
:set showcmd   
" visual, insert, etc.
:set showmode  
" line numbers
:set number    

:set smartindent
:set shiftwidth=4
:set softtabstop=4
:set tabstop=4

" incremental search
:set incsearch
" highlight all search results
:set hlsearch

" hit space to clear search results
:nnoremap <silent> <Space> :nohlsearch<Bar>:echo<CR> 

" add mouse support
:set mouse=a

" syntax highlighting
:syntax enable

" break on spaces
:set linebreak

" percent through file
set statusline+=\ %P
