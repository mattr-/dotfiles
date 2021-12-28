local status_ok, lsp_installer = pcall(require, "nvim-lsp-installer")
if not status_ok then
  return
end

-- Configure settings first, per the readme
lsp_installer.settings({
  ui = {
    icons = {
      server_installed = "✓",
      server_pending = "➜",
      server_uninstalled = "✗"
    }
  }
})

lsp_installer.on_server_ready(function(server)
  local opts = {
    on_attach = require("mattr-.lsp.handlers").on_attach,
    capabilities = require("mattr-.lsp.handlers").capabilities,
  }

  server:setup(opts)
end)
