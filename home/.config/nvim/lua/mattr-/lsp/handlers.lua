local M = {}

local function define_signs()
  local signs = {
    { name = "DiagnosticSignError", text = "ﱥ" },
    { name = "DiagnosticSignWarn", text = "" },
    { name = "DiagnosticSignInfo", text = ""},
    { name = "DiagnosticSignHint", text = ""},
  }

  for _, sign in ipairs(signs) do
    vim.fn.sign_define(sign.name, { texthl = sign.name, text = sign.text, numhl = "" })
  end
end

local function lsp_keymaps(bufnr)
  local opts = { noremap = true, silent = true }
  vim.api.nvim_buf_set_keymap(bufnr, "n", "gD", "<cmd>lua vim.lsp.buf.declaration()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "gd", "<cmd>lua vim.lsp.buf.definition()<CR>", opts)
  vim.api.nvim_buf_set_keymap(bufnr, "n", "K", "<cmd>lua vim.lsp.buf.hover()<CR>", opts)
  vim.api.nvim_buf_set_keymap(
    bufnr,
    "n",
    "gl",
    '<cmd>lua vim.diagnostic.open_float(0, { scope = "line", border = "rounded" })<CR>',
    opts)
  vim.cmd [[ command! Format execute 'lua vim.lsp.buf.formatting()' ]]
end

local function lsp_highlight_document(client)
  -- Highlight references when the cursor stops somewhere
  if client.resolved_capabilities.document_highlight then
    vim.api.nvim_exec(
      [[
      augroup lsp_document_highlight
        autocmd! * <buffer>
        autocmd CursorHold <buffer> lua vim.lsp.buf.document_highlight()
        autocmd CursorMoved <buffer> lua vim.lsp.buf.clear_references()
      augroup END
    ]],
    false)
  end
end

-- One time set up for vim's diagnostics configuration
M.setup = function()
  define_signs()

  local config = {
    underline = true,
    virtual_text = false,
    signs = true,
    float = {
      style = "minimal",
      border = "rounded",
      focusable = false,
      header = "",
      source = "always",
      prefix = "",
    },
    update_in_insert = true,
    severity_sort = true,
  }

  vim.diagnostic.config(config)

  -- Use rounded borders for some of the existing handlers
  vim.lsp.handlers["textDocument/hover"] = vim.lsp.with(vim.lsp.handlers.hover, {
    border = "rounded"
  })

  vim.lsp.handlers["textDocument/signatureHelp"] = vim.lsp.with(vim.lsp.handlers.signature_help, {
    border = "rounded"
  })
end

-- LSP configuration when attached to a new buffer
M.on_attach = function(client, bufnr)
  lsp_keymaps(bufnr)
  lsp_highlight_document(client)
end

-- nvim-cmp supports additional completion capabilities and we want to add those to LSP
local capabilities = vim.lsp.protocol.make_client_capabilities()
M.capabilities = require('cmp_nvim_lsp').update_capabilities(capabilities)

return M
