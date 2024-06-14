---@class custom.util.ui
local M = {
}

---@param group_name string # The name of the highlight group to lookup
---@return vim.api.keyset.hl_info|nil
local highlight_group = function(group_name)
  return vim.api.nvim_get_hl(0, { name = group_name, link = false }) or nil
end


---@param group_name string # The name of the highlight group to get the foreground for
---@return vim.api.keyset.hl_info|nil
M.fg = function(group_name)
  local hl_group = highlight_group(group_name)
  return hl_group and { fg = string.format("#%06x", hl_group.fg) } or nil
end

return M
