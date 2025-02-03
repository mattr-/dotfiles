return
{
  {
    "mattr-/DashVim",
    import = "dashvim.plugins",
    dir = "~/Code/mattr-/DashVim",
  },

  {
    "folke/tokyonight.nvim",
    lazy = true
  },
  -- quick and dirty file search with fzf
  {
    "ibhagwan/fzf-lua",
    lazy = true,
    keys = {
      { "<space>ff", "<cmd>FzfLua files<cr>", desc = "Find Files", },
      { "<space>/", "<cmd>FzfLua live_grep<cr>", desc = "Live Grep", },
      { "<space>sh", "<cmd>FzfLua highlights<cr>", desc = "Search Highlight Groups", },
    },
  },
}
