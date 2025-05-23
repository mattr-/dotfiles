---@class dashvim.lualine
local M = {}

---@param str string
function M.statusline_escape(str)
  if type(str) ~= "string" then
    return str
  end

  return str:gsub("%%", "%%%%")
end

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
    utils.extract_highlight_colors(highlight_group, "bold") and "bold",
    utils.extract_highlight_colors(highlight_group, "italic") and "italic",
  })
  ---@type table<string, string>
  component.highlight_cache = component.highlight_cache or {}
  local lualine_highlight_group = component.highlight_cache[highlight_group]
  if not lualine_highlight_group then
    lualine_highlight_group = component:create_hl({
      fg = utils.extract_highlight_colors(highlight_group, "fg"),
      gui = #gui > 0 and table.concat(gui, ",") or nil,
    }, "DashVim_" .. highlight_group) --[[@as string]]
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
    path = DashVim.util.norm(path)
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
