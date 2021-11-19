-- Inspired by and partially copied from tjdevries' telescope config
-- https://github.com/tjdevries/config_manager/blob/master/xdg_config/nvim/lua/tj/telescope/init.lua

local themes = require "telescope.themes"

local M = {}

function M.dotfiles()
  local opts_with_preview

  opts_with_preview = themes.get_ivy {
    prompt_title = " dotfiles ",
    shorten_path = false,
    cwd = "~/.homesick/repos/dotfiles",
    theme = "ivy"
  }

  require("telescope.builtin").git_files(opts_with_preview)
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

