local lspconfig = require("lspconfig")
local config = require("mattr-.lsp.config")

config.settings = {
  json = {
    schemas = require('schemastore').json.schemas(),
  },
}

lspconfig.jsonls.setup(config)
