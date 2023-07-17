vim.g.mapleader = " "
vim.g.maplocalleader = ","

local opt = vim.opt

opt.termguicolors = true  -- Enables 24-bit RGB color
opt.hidden        = true  -- background buffers without writing them. saves marks/undo as well
opt.confirm       = false -- I don't really want to see confirm popups
opt.conceallevel  = 3     -- Hide markup for things like links, bold, and italics

opt.shortmess     = opt.shortmess
                    + "f" --use "(3 of 5)" instead of "(file 3 of 5)"
                    + "i" --use "[noeol]" instead of "[Incomplete last line]"
                    + "l" --use "999L, 888C" instead of "999 lines, 888 characters"
                    + "m" --use "[+]" instead of "[Modified]"
                    + "n" --use "[New]" instead of "[New File]"
                    + "r" --use "[RO]" instead of "[readonly]"
                    + "w" --use "[w]" instead of "written" for file write message
                          --and "[a]" instead of "appended" for ':w >> file' command
                    + "x" --use "[dos]" instead of "[dos format]", "[unix]" instead of
                          --"[unix format]" and "[mac]" instead of "[mac format]".
                    + "t" --truncate file message at the start if it is too long to fit
                          --on the command-line, "<" will appear in the left most column.
                          --Ignored in Ex mode.
                    + "T" --truncate other messages in the middle if they are too long to
                          --fit on the command line.  "..." will appear in the middle.
                          --Ignored in Ex mode.
                    + "o" --overwrite message for writing a file with subsequent message
                          --for reading a file (useful for ":wn" or when 'autowrite' on)
                    + "O" --message for reading a file overwrites any previous message.
                          --Also for quickfix message (e.g., ":cn").
                    + "C" --don't give messages while scanning for ins-completion items
opt.number        = true  -- Absolute line numbers
opt.ruler         = true  -- line and column number of cursor
opt.hlsearch      = true  -- Highlight searches
opt.incsearch     = true  -- Incrementally search
opt.ignorecase    = true  -- Ignore case when searching...
opt.smartcase     = true  -- ...unless the query is case sensitive
opt.cursorline    = true  -- Highlight the current line of the cursor
opt.report        = 0     -- Always report number of lines changed by ex commands
opt.wrap          = false -- Don't wrap long lines for display by default.
opt.modelines     = 5     -- Look for five modelines around the beginning and end of a file
opt.visualbell    = true  -- Don't beep
opt.expandtab     = true  -- Spaces by default
opt.shiftwidth    = 2     -- Two space indent
opt.softtabstop   = 2     -- ... and here ...
opt.tabstop       = 2     -- ... and here ...
opt.shiftround    = true  -- Round indent to a multiple of shiftwidth
opt.breakindent   = true  -- Make nice paragraphs out of comments
opt.timeoutlen    = 300   -- Give me more time for complex mappings
opt.ttimeoutlen   = 10    -- Make Escape work faster
opt.backup        = false -- No backups
opt.swapfile      = false -- No swap files. I too like to live...dangerously.
opt.list          = true  -- Mark up my buffers
vim.opt.listchars = {   -- And use fancy characters to do it
  nbsp = "%",
  extends = "»",
  precedes = "«",
  trail = "•",
  tab = "▸-",
}
opt.splitbelow    = true  -- New windows on the bottom
opt.splitright    = true  -- New windows on the right
opt.equalalways   = false -- I'll handle the window sizes.
opt.startofline   = false -- Attempt to keep the cursor in the same column
opt.scrolloff     = 3     -- 3 lines of context when scrolling
opt.sidescrolloff = 8     -- 8 columns of context when side scrolling
opt.showmatch     = true  -- Show matching pairs
opt.showmode      = false -- Don't show the mode since we have a statusline
opt.updatetime    = 100   -- Update faster
opt.mouse         = ""    -- Disable mouse by default
opt.pumblend      = 10    -- 10% pseudo-transparency for the popup-menu
opt.pumheight     = 10    -- Maximum number of entries in the popup-menu
opt.signcolumn    = "yes" -- Always display the sign column otherwise the UI moves
opt.fillchars = {
    vert = "▕",
    fold = " ",
    eob = " ",
    diff = "─",
    msgsep = "‾",
    foldopen = "▾",
    foldclose = "▸",
    foldsep = "│",
}
-- popup menu even with only one match and force me to make a selection
opt.completeopt = { "menu", "menuone", "noselect" }

-- see `:help fo-table` for all the options
opt.formatoptions = opt.formatoptions
                  + "t" -- auto wrap using textwidth
                  + "c" -- auto wrap comments with textwidth
                  + "r" -- auto insert comment leader after <Enter> in insert mode
                  + "o" -- auto insert comment leader after using `o` or `O` in normal mode
                  + "q" -- allow formatting of comments with "gq" <3
                  + "n" -- recognized numbered lists when formatting
                  + "l" -- no automatic reformatting of existing long lines
                  + "j" -- remove comment leaders when joining lines where possible

opt.inccommand = "nosplit" -- show incremental changes of command inline

-- configure ripgrep for vimgrep
opt.grepprg = [[rg --hidden --glob "!.git" --no-heading --smart-case --vimgrep --follow $*]]
opt.grepformat = "%f:%l:%c:%m"

-- keep text on the same line for horizontal splits
opt.splitkeep = "screen"

-- Fix markdown indentation settings
vim.g.markdown_recommended_style = 0
