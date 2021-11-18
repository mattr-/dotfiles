local actions = require("telescope.actions")
local action_layout = require("telescope.actions.layout")

require("telescope").setup({
  defaults = {
    mappings = {
      i = {
        ["<Esc>"] = actions.close  --Close when hitting escape
      }
    }
  }
})
