return function()
  local mason = require("mason")
  local mason_lspconfig = require("mason-lspconfig")
  mason.setup({
    ui = {
      border = "rounded",
      icons = {
        package_installed = " ",
        package_pending = " ",
        package_uninstalled = " ",
      },
    },
  })

  mason_lspconfig.setup({
    ensure_installed = {
      "marksman",
      "sumneko_lua"
    },
  })

  require("mattr-.config.null-ls")
  require("mattr-.lsp")
end
