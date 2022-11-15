local lspconfig = require("lspconfig")
local config = require("mattr-.lsp.handlers")

lspconfig.sumneko_lua.setup({
  on_attach = function(client, bufnr)
    -- let null-ls handle document formatting for lua
    client.server_capabilities.documentFormattingProvider = false
    client.server_capabilities.documentRangeFormattingProvider = false

    config.on_attach(client, bufnr)
  end,
  capabilities = config.capabilities(),
  settings = {
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
  },
})
