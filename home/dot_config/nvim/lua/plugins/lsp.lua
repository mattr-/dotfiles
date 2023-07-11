return {
  {
    "neovim/nvim-lspconfig",
    event = { "BufReadPre", "BufNewFile" },
    dependencies = {
      { "folke/neoconf.nvim", cmd = "Neoconf", config = true },
      { "folke/neodev.nvim", opts = {} },
      "williamboman/mason.nvim",
      "williamboman/mason-lspconfig.nvim",
    },
    opts = {
      --options for vim.diagnostic.config()
      diagnostics = {
        underline = true,
        virtual_text = {
          spacing = 4, -- 4 spaces of padding before diag text
          update_in_insert = true, --allow diag updates in insert mode
          source = "if_many", -- only show source if there are multiple
          -- configure prefix to show icons. requires nvim >= 0.10
          prefix = function(diagnostic)
            local icons = require("mattr-.config").icons.diagnostics
            for d, icon in pairs(icons) do
              if diagnostic.severity == vim.diagnostic.severity[d:upper()] then
                return icon
              end
            end
          end,

        },
        severity_sort = true, --highest severity first. why is this not the default?
      },
      -- Options for vim.lsp.buf.format
      format = {
        formatting_options = nil,
        timeout_ms = nil,
      },
      -- LSP Server Settings
      servers = {
        lua_ls = {
          settings = {
            Lua = {
              workspace = {
                checkThirdParty = false,
              },
              completion = {
                callSnippet = "Replace",
              },
            },
          },
        },
      },
    },
    setup = {},
    config = function(_, opts)
      Util = require("mattr-.lsp.util")

      -- override the defaults for diagnostics so we get pretty icons
      for name, icon in pairs(require("mattr-.config").icons.diagnostics) do
        name = "DiagnosticSign" .. name
        vim.fn.sign_define(name, { text = icon, texthl = name, numhl = "" })
      end

      local inlay_hint = vim.lsp.buf.inlay_hint or vim.lsp.inlay_hint
      if inlay_hint then
        Util.on_attach(function(client, buffer)
          if client.server_capabilities.inlayHintProvider then
            inlay_hint(buffer, true)
          end
        end)
      end

      vim.diagnostic.config(vim.deepcopy(opts.diagnostics))
    end,
  },
}
