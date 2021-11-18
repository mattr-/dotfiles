local actions = require("telescope.actions")
local action_layout = require("telescope.actions.layout")

require("telescope").setup({
  defaults = {
    prompt_prefix = "❯ ",
    selection_caret = "❯ ",
    winblend = 0,
    preview = false,
    color_devicons = true,
    mappings = {
      i = {
        ["<Esc>"] = actions.close  --Close when hitting escape
      }
    }
  }
})

require("telescope").load_extension("fzf")
