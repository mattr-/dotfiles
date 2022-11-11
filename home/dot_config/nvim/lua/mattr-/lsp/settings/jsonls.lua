local lspconfig = require("lspconfig")
local config = require("mattr-.lsp.handlers")

lspconfig.jsonls.setup({
  on_attach = config.on_attach,
  capabilities = config.capabilities(),
  settings = {
    json = {
      schemas = require('schemastore').json.schemas(),
    },
  },
})
