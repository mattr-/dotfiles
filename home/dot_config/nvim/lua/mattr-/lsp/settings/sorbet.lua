local lspconfig = require("lspconfig")
local config = require("mattr-.lsp.config")
local features = require("mattr-.features")

if features.sorbet.available then
  config.cmd = features.sorbet.command
  lspconfig.sorbet.setup(config)
end
