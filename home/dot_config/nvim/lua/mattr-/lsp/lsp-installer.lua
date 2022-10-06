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

  -- If we've defined an options file, let's use that
  local status_ok, server_config = pcall(require, "mattr-.lsp.settings." .. server.name)
  if status_ok then
    opts = vim.tbl_deep_extend("force", server_config, opts)
  end

  server:setup(opts)
end)
