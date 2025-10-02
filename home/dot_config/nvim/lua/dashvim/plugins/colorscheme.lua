return {
  -- install some colorschemes because I can't stick to just one
  -- tokyonight
  {
    "folke/tokyonight.nvim",
    lazy = true,
    opts = { style = "moon" },
  },

  -- catppuccin
  {
    "catppuccin/nvim",
    lazy = true,
    name = "catppuccin",
    opts = {
      lsp_styles = {
        underlines = {
          errors = { "undercurl" },
          hints = { "undercurl" },
          warnings = { "undercurl" },
          information = { "undercurl" },
        },
      },
      integrations = {
        blink_cmp = true,
        gitsigns = true,
        lsp_trouble = true,
        mason = true,
        mini = true,
        navic = { enabled = true },
        neotest = true,
        noice = true,
        notify = true,
        neotree = true,
        semantic_tokens = true,
        snacks = true,
        treesitter = true,
        which_key = true,
      },
    },
  },
}
