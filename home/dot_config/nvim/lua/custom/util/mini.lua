---@class custom.util.mini

local M = {}

function M.pairs(opts)
  -- Configure the toggle for pairs
  Custom.toggle({
    name = "Mini Pairs",
    get = function()
      return not vim.g.minipairs_disable
    end,
    set = function(state)
      vim.g.minipairs_disable = not state
    end,
  }):map("<leader>vp")

  local pairs = require("mini.pairs")
  pairs.setup(opts)
end

return M
