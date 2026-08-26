-- Markdown stuff goes here

return {
  {
    "nvim-treesitter/nvim-treesitter",
    opts = {
      ensure_installed = {
        "markdown",
        "markdown_inline",
        "html"
      },
    },
  },
  {
    "iamcco/markdown-preview.nvim",
    cmd = { "MarkdownPreview", "MarkdownPreviewToggle" },
    ft = { "markdown" },
    keys = {
      { "<localleader>p", "<cmd>MarkdownPreviewToggle<cr>", ft = "markdown", desc = "Preview Markdown" },
    },
    init = function()
      vim.g.mkdp_markdown_css = vim.fn.stdpath("data") .. "/site/markdown_preview.css"
    end,
    build = function()
      require("lazy").load({ plugins = { "markdown-preview.nvim" } })
      vim.fn["mkdp#util#install"]()
    end,
  },
  {
    "mason-org/mason.nvim",
    opts = {
      ensure_installed = {
        "marksman",
      },
    },
  },
}
