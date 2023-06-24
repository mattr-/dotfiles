return {
  {
    "nvim-treesitter/nvim-treesitter",
    opts = function(_, opts)
      vim.list_extend(opts.ensure_installed, {
        "rust",
      })
    end,
  },
  {
    "williamboman/mason.nvim",
    opts = function(_, opts)
      vim.list_extend(opts.ensure_installed, { "rust-analyzer" })
    end,
  },
  {
    "simrat39/rust-tools.nvim",
    event = "InsertEnter",
  },
  {
    "hrsh7th/nvim-cmp",
    dependencies = { "saecki/crates.nvim" },
    opts = function(_, opts)
      local cmp = require("cmp")
      opts.sources = cmp.config.sources(vim.list_extend(opts.sources, { { name = "crates" } }))
    end,
  },
}
