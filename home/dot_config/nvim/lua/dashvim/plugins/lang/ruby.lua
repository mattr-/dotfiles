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
          local lspconfig = require("lspconfig")
          if DashVim.util.tools.sorbet_configured() then
            lspconfig.sorbet.setup(opts)
          end
          return true
        end,
        rubocop = function(_, opts)
          local lspconfig = require("lspconfig")
          if DashVim.util.tools.rubocop_configured() then
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
      opts.ensure_installed = opts.ensure_installed or {}

      if DashVim.util.tools.sorbet_configured() then
        vim.list_extend(opts.ensure_installed, { "sorbet" })
      end

      if DashVim.util.tools.rubocop_configured() then
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
