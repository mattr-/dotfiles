---@class custom.lualine
local M = {}

-- Escape text so vim doesn't think it's a statusline substitution
---@param str string
function M.statusline_escape(str)
  if type(str) ~= "string" then
    return str
  else
    return str:gsub("%%", "%%%%")
  end
end

---@param component any
---@param text string the text to format
---@param highlight_group string the highlight group to use to formatting
function M.format(component, text, highlight_group)
  text = M.statusline_escape(text)
  if not highlight_group or highlight_group == "" then
    return text
  end

  local utils = require("lualine.utils.utils")
  ---@type string[]
  local gui = vim.tbl_filter(function(x)
    return x
  end, {
    utils.extract_highlight_colors(highlight_group,"bold") and "bold",
    utils.extract_highlight_colors(highlight_group, "italic") and "italic",
  })
  ---@type table<string, string>
  component.highlight_cache = component.highlight_cache or {}
  local lualine_highlight_group = component.highlight_cache[highlight_group]
  if not lualine_highlight_group then
    lualine_highlight_group = component:create_hl({
      fg = Custom.ui.fg(highlight_group),
      gui = #gui > 0 and table.concat(gui, ",") or nil,
    }, "Custom_" .. highlight_group) --[[@as string]]
    component.highlight_cache[highlight_group] = lualine_highlight_group
  end

  return component:format_hl(lualine_highlight_group) .. text .. component:get_default_hl()
end

function M.fancy_path(opts)
   opts = vim.tbl_extend("force", {
    modified_hl = "MatchParen",
    directory_hl = "",
    filename_hl = "Bold",
    readonly_hl = "Error",
    modified_sign = "  ",
    readonly_sign = " 󰌾 ",
  }, opts or {})

  return function(self)
    local path = vim.fn.expand('%:~:.')
    path = Custom.util.norm(path)
    if path == "" then
      return ""
    end
    local path_parts = vim.split(path, "[\\/]")
    local sep = package.config:sub(1,1)


    if opts.modified_hl and vim.bo.modified then
      path_parts[#path_parts] = M.format(self, path_parts[#path_parts], opts.modified_hl)
      path_parts[#path_parts] = path_parts[#path_parts] .. M.format(self, opts.modified_sign, opts.modified_hl)
    else
      path_parts[#path_parts] = M.format(self, path_parts[#path_parts], opts.filename_hl)
    end

    local readonly = ""
    if vim.bo.readonly then
      readonly = M.format(self, opts.readonly_sign, opts.readonly_hl)
    end

    return table.concat(path_parts, sep) .. readonly
  end
end

return M
