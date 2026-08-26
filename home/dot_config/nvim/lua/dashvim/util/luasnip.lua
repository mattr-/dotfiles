---@class dashvim.util.luasnip

local M = {}

M.inside = function(trigger)
  local ls = require("luasnip")
  return function()
    local buf = vim.api.nvim_get_current_buf()
    local node = ls.session.current_nodes[buf]
    if not node then
      return false
    end

    local outer_snip = node.parent.snippet
    while outer_snip.parent do
      outer_snip = outer_snip.parent.snippet
    end

    return outer_snip.trigger == trigger
  end
end

return M
