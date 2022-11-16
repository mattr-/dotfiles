local M = {}

-- sets up fancy icons for signs
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

-- One time set up for vim's diagnostics configuration
M.setup = function()
  define_signs()

  local config = {
    virtual_text = false, -- default is true
    update_in_insert = true, -- default is false
    severity_sort = true, -- default is false
  }

  vim.diagnostic.config(config)

end

return M
