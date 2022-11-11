return function()
  local mason = require("mason")
  local mason_lspconfig = require("mason-lspconfig")
  mason.setup({
    ui = {
      border = "single",
      icons = {
        package_installed = " ",
        package_pending = " ",
        package_uninstalled = " ",
      },
    },
  })

  mason_lspconfig.setup()

  -- Auto install LSP servers, leveraging my previous nvim-lsp-installer config
  -- TODO Switch to a more declarative style setup
  mason_lspconfig.setup_handlers({
    function(server_name) -- default handler
      -- check for my own options file and use it
      local opts = {
        on_attach = require("mattr-.lsp.handlers").on_attach,
        capabilities = require("mattr-.lsp.handlers").capabilities(),
      }
      local server_config_file = "mattr-.lsp.settings." .. server_name
      local status_ok, server_config = pcall(require, server_config_file)
      if status_ok then
        opts = vim.tbl_deep_extend("force", opts, server_config)
      end

      require("lspconfig")[server_name].setup(opts)
    end,
  })
  local null_ls_setup = require("mattr-.config.null-ls")
  null_ls_setup()
end
