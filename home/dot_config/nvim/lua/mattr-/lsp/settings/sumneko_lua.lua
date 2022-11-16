local lspconfig = require("lspconfig")
local config = require("mattr-.lsp.config").defaults()

config.settings = {
  Lua = {
    runtime = {
      version = "LuaJIT",
    },
    diagnostics = {
      globals = {
        "vim",
        "packer_plugins"
      },
    },
    workspace = {
      library = {
        [vim.fn.expand("$VIMRUNTIME/lua")] = true,
        [vim.fn.stdpath("config") .. "/lua"] = true,
      },
    },
  },
}

config.on_attach = function(client, bufnr)
    -- let null-ls handle document formatting for lua
    client.server_capabilities.documentFormattingProvider = false
    client.server_capabilities.documentRangeFormattingProvider = false

    require("mattr-.lsp.config").on_attach(client, bufnr)
  end,

lspconfig.sumneko_lua.setup(config)
