return {
  {
    "nvim-treesitter/nvim-treesitter",
    opts = {
      ensure_installed = {
        "ruby"
      },
    },
  },

  {
    "neovim/nvim-lspconfig",
    opts = {
      ---@type table<string, fun(server:string, opts:_.lspconfig.options):boolean?>
      setup = {
        sorbet = function(_, opts)
          local features = require("custom.features")
          local lspconfig = require("lspconfig")
          if features.sorbet.available() then
            opts.cmd = features.sorbet.command()
            lspconfig.sorbet.setup(opts)
          end
          return true
        end,
        rubocop = function(_, opts)
          local features = require("custom.features")
          local lspconfig = require("lspconfig")
          if features.rubocop.available() then
            opts.cmd = features.rubocop.command()
            lspconfig.rubocop.setup(opts)
          end
          return true
        end,
      },
    },
  },

  -- Install Ruby tools
  {
    "williamboman/mason.nvim",
    opts = function(_, opts)
      local features = require("custom.features")
      opts.ensure_installed = opts.ensure_installed or {}

      if features.sorbet.available() then
        vim.list_extend(opts.ensure_installed, { "sorbet" })
      end

      if features.rubocop.available() then
        vim.list_extend(opts.ensure_installed, { "rubocop" })
      end
    end,
  },

  {
    "nvim-neotest/neotest",
    optional = true,
    dependencies = {
      "zidhuss/neotest-minitest",
    },
    opts = {
      adapters = {
        ["neotest-minitest"] = {},
      }
    },
  }
}
