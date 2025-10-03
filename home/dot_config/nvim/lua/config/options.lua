-- Options are automatically loaded before lazy.nvim startup
-- Default options that are always set: https://github.com/LazyVim/LazyVim/blob/main/lua/lazyvim/config/options.lua
-- Add any additional options here

-- Disable auto format
vim.g.autoformat = false

-- Disable Snacks animations
vim.g.snacks_animate = false

-- Custom options
local opt = vim.opt
opt.confirm = false -- I don't really want to see confirm popups
opt.fillchars = {
    diff = "╱",
    msgsep = "‾",
    foldopen = "▾",
    foldclose = "▸",
    foldsep = "│",
}
opt.mouse = ""    -- Disable mouse by default
opt.updatetime = 100   -- Trigger CursorHold faster
