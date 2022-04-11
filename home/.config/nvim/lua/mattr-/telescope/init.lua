-- Inspired by and partially copied from tjdevries' telescope config
-- https://github.com/tjdevries/config_manager/blob/master/xdg_config/nvim/lua/tj/telescope/init.lua

local themes = require "telescope.themes"

local M = {}

function M.dotfiles()
  local opts_with_preview

  opts_with_preview = {
    prompt_title = " dotfiles ",
    shorten_path = false,
    preview = true,
    cwd = "~/.homesick/repos/dotfiles",
  }

  require("telescope.builtin").git_files(opts_with_preview)
end

function M.find_files()
  local opts = {
    prompt_title = "🔎 Files 🔍",
    preview = true,
  }
  local ok = pcall(require'telescope.builtin'.git_files, opts)
  if not ok then require'telescope.builtin'.find_files(opts) end
end

return setmetatable({}, {
  __index = function(_, k)

  if M[k] then
    return M[k]
  else
    return require("telescope.builtin")[k]
  end
end
})

