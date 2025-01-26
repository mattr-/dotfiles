---@class custom.util.mini

local M = {}

function M.pairs(opts)
  -- Configure the toggle for pairs
  Snacks.toggle({
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

function M.ai(opts)
  Snacks.toggle({
    name = "Mini TextObjects",
    get = function ()
      return not vim.g.miniai_disable
    end,
    set = function (state)
      vim.g.miniai_disable = not state
    end,
  }):map("<leader>va")

  local ai = require("mini.ai")
  ai.setup(opts)
end

return M
