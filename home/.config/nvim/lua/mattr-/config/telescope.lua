return function()
  local telescope = require("telescope")
  local actions = require("telescope.actions")
  local action_layout = require("telescope.actions.layout")

  telescope.setup({
    defaults = {
      prompt_prefix = " ",
      selection_caret = " ",
      winblend = 10,
      color_devicons = true,
    }
  })

  telescope.load_extension("fzf")
  telescope.load_extension("ui-select")
  telescope.load_extension("notify")
  telescope.load_extension("mapper")
end
