return
{
  {
    "mattr-/DashVim",
    import = "dashvim.plugins",
    dir = "~/Code/mattr-/DashVim",
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
  -- nerd font icons selection
  {
    "2KAbhishek/nerdy.nvim",
    cmd = "Nerdy",
  },

  {
    "iamcco/markdown-preview.nvim",
    ft = { "markdown" },
    cmd = { "MarkdownPreviewToggle", "MarkdownPreview", "MarkdownPreviewStop" },
    build = function()
      require("lazy").load({ plugins = { "markdown-preview.nvim" } })
      vim.fn["mkdp#util#install"]()
    end,
    config = function()
      vim.cmd([[do FileType]])
    end,
  },

  -- play with snacks
  {
    "folke/snacks.nvim",
    opts = {
      explorer = { replace_netrw = true, },
    },
    keys = {
      { "<leader>fr", function() Snacks.explorer.reveal() end, desc = "Reveal file in Explorer" },
      { "<leader>fe", function() Snacks.explorer.open() end, desc = "Toggle the File Explorer" },
    },
  },

}
