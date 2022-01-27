local status_ok, telescope = pcall(require, "telescope")
if not status_ok then
  return
end

local actions = require("telescope.actions")
local action_layout = require("telescope.actions.layout")

telescope.setup({
  defaults = {
    prompt_prefix = "❯ ",
    selection_caret = "❯ ",
    winblend = 0,
    preview = false,
    color_devicons = true,
  }
})

telescope.load_extension("fzf")
telescope.load_extension("ui-select")
