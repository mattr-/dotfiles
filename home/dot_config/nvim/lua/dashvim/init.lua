---@class DashVim
---@field config dashvim.config
---@field lazy dashvim.lazy
---@field lualine dashvim.lualine
---@field ui dashvim.ui
---@field util dashvim.util

local M = {}

setmetatable(M, {
  __index = function(t, k)
    ---@diagnostic disable-next-line: no-unknown
    t[k] = require("dashvim." .. k)
    return t[k]
  end,
})

_G.DashVim = M
