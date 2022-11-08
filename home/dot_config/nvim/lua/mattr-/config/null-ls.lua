local null_ls = require("null-ls")
local features = require("mattr-.features")
local mason_utils = require("mattr-.mason_utils")

local code_actions = null_ls.builtins.code_actions
local diagnostics = null_ls.builtins.diagnostics
local formatting = null_ls.builtins.formatting

mason_utils.ensure_installed({
  "luacheck",
  "stylua",
})

local sources = {
  code_actions.gitsigns,
  diagnostics.luacheck.with({ command = mason_utils.path_for("luacheck") }),
  formatting.stylua.with({ command = mason_utils.path_for("stylua") }),
}

if features.rubocop.available then
  table.insert(sources, formatting.rubocop.with({ command = features.rubocop.command }))
  table.insert(sources, diagnostics.rubocop.with({ command = features.rubocop.command }))
end

return function()
  null_ls.setup({
    debug = false,
    sources = sources,
    on_attach = function(client)
      if client.server_capabilities.documentFormattingProvider then
        local id = vim.api.nvim_create_augroup("lsp_formatting", { clear = false })
        vim.api.nvim_clear_autocmds({ buffer = 0, group = id })
        vim.api.nvim_create_autocmd("BufWritePre", {
          buffer = 0,
          group = id,
          callback = function()
            vim.lsp.buf.format({ sync = true })
          end,
        })
      end
    end,
  })
end
