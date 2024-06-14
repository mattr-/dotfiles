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

-- Display an error in the UI. Uses vim.notify under the hood
--
---@param title string The title for the notification
---@param message string The message to display
---@param opts table Options for customizing the display
---
M.error = function(title, message, opts)
  opts = vim.tbl_extend("force", {
    title = title,
    icon = Custom.config.icons.diagnostics.Error
  }, opts or {})

  vim.notify(message, vim.log.levels.ERROR, opts)
end

-- Display a warning in the UI. Uses vim.notify under the hood
--
---@param title string The title for the notification
---@param message string The message to display
---@param opts table Options for customizing the display
---
M.warn = function(title, message, opts)
  opts = vim.tbl_extend("force", {
    title = title,
    icon = Custom.config.icons.diagnostics.Warn
  }, opts or {})

  vim.notify(message, vim.log.levels.WARN, opts)
end

-- Display a informational message in the UI. Uses vim.notify under the hood
--
---@param title string The title for the notification
---@param message string The message to display
---@param opts table Options for customizing the display
---
M.info = function(title, message, opts)
  opts = vim.tbl_extend("force", {
    title = title,
    icon = Custom.config.icons.diagnostics.Info
  }, opts or {})

  vim.notify(message, vim.log.levels.INFO, opts)
end

return M
