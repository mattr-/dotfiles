-- Colorscheme name:    hackthebox
-- Description:         Port of VSCode's HackTheBox colorscheme for NeoVim
-- Author:              https://github.com/mattr-
-- Forked from:         audibleblink/hackthebox.vim
-- Most of this code is copied from https://github.com/shaunsingh/moonlight.nvim
-- and renamed to fix this theme.
local util = require('mattr-.theme.util')

-- Load the theme
local set = function ()
    util.load()
end

return { set = set }
