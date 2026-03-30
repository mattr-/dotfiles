--- @class dashvim.config
local M = {}

--- @class DashVimOptions
local defaults = {
  colorscheme = "catppuccin",
  icons = {
    dap = {
      Stopped = { "󰁕 ", "DiagnosticWarn", "DapStoppedLine" },
      Breakpoint = " ",
      BreakpointCondition = " ",
      BreakpointRejected = { " ", "DiagnosticError" },
      LogPoint = ".>",
    },
    diagnostics = {
      Error = " ",
      Warn = " ",
      Hint = " ",
      Info = " ",
      Debug = " ",
      Trace = " ",
    },
    git = {
      added = " ",
      modified = " ",
      removed = " ",
    },
    kinds = {
      Array = " ",
      Boolean = " ",
      Class = " ",
      Color = " ",
      Constant = " ",
      Constructor = " ",
      Copilot = " ",
      Enum = " ",
      EnumMember = " ",
      Event = " ",
      Field = " ",
      File = " ",
      Folder = " ",
      Function = " ",
      Interface = " ",
      Key = " ",
      Keyword = " ",
      Method = " ",
      Module = " ",
      Namespace = " ",
      Null = " ",
      Number = " ",
      Object = " ",
      Operator = " ",
      Package = " ",
      Property = " ",
      Reference = " ",
      Snippet = " ",
      String = " ",
      Struct = " ",
      Text = " ",
      TypeParameter = " ",
      Unit = " ",
      Value = " ",
      Variable = " ",
    },
  },
}

local options
setmetatable(M, {
  __index = function(_, key)
    if options == nil then
      return vim.deepcopy(defaults)[key]
    end

    ---@cast options dashvim.config
    return options[key]
  end,
})

local config = {}

function M.get(library, ldefaults, ...)
  local merge = { vim.deepcopy(ldefaults), vim.deepcopy(config[library] or {}) }
  for i = 1, select("#", ...) do
    local value = select(i, ...)
    if value then
      table.insert(merge, vim.deepcopy(value))
    end
  end

  return vim.tbl_deep_extend("force", unpack(merge))
end

return M
