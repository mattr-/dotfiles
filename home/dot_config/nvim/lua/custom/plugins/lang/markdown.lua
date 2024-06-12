-- Markdown stuff goes here

return {
  {
    "nvim-treesitter/nvim-treesitter",
    opts = {
      ensure_installed = {
        "markdown",
        "markdown_inline",
      },
    },
  },
  {
    "iamcco/markdown-preview.nvim",
    cmd = { "MarkdownPreview", "MarkdownPreviewToggle" },
    ft = { "markdown" },
    keys = {
      { "<localleader>p", "<cmd>MarkdownPreviewToggle<cr>", desc = "Preview Markdown", ft = "markdown" },
    },
    build = function()
      vim.fn["mkdp#util#install"]()
    end,
  },
  {
    "williamboman/mason.nvim",
    opts = {
      ensure_installed = {
        "marksman",
      },
    },
  },
}
