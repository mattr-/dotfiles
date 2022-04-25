local opt = vim.opt

vim.g.mapleader = " "

opt.hidden        = true  -- background buffers without writing them. saves marks/undo as well
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
opt.tabstop       = 2     -- Two space indent...
opt.softtabstop   = 2     -- ... and here ...
opt.shiftwidth    = 2     -- ... and here ...
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
opt.scrolloff     = 3     -- 3 linues of context when scrolling
opt.showmatch     = true  -- Show matching pairs
opt.updatetime    = 100   -- Update faster
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

if vim.fn.executable("rg") then
  opt.grepprg = [[rg --hidden --glob "!.git" --no-heading --smart-case --vimgrep --follow $*]]
  opt.grepformat = opt.grepformat ^ { "%f:%l:%c:%m" }
end
