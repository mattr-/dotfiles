---@class dashvim.util.lsp
local M = {}

-- on_attach is a function that takes a client and a buffer
function M.on_attach(on_attach)
  vim.api.nvim_create_autocmd("LspAttach", {
    callback = function(event)
      local buffer = event.buf
      local client = vim.lsp.get_client_by_id(event.data.client_id)
      on_attach(client, buffer)
    end,
  })
end

function M.diagnostic_goto(direction, severity)
  local go = direction == "next" and vim.diagnostic.goto_next or vim.diagnostic.goto_prev
  local diagnostic_severity = severity and vim.diagnostic.severity[severity] or nil
  return function()
    go({ severity = diagnostic_severity })
  end
end

function M.setup_highlight_local_references(client, buffer)
  -- Highlight references when the cursor stops somewhere
  if client.server_capabilities.documentHighlightProvider then
    local augroup = vim.api.nvim_create_augroup("mattr_lsp_document_highlight", { clear = false })
    vim.api.nvim_clear_autocmds({ buffer = buffer })
    vim.api.nvim_create_autocmd({ "CursorHold" }, {
      group = augroup,
      buffer = buffer,
      callback = function()
        vim.lsp.buf.document_highlight()
      end,
    })
    vim.api.nvim_create_autocmd({ "CursorMoved" }, {
      group = augroup,
      buffer = buffer,
      callback = function()
        vim.lsp.buf.clear_references()
      end,
    })
  end
end

return M
