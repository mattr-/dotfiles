---@class Custom
---@field config custom.config
---@field toggle custom.toggle
---@field lazy custom.lazy
---@field lualine custom.lualine
---@field ui custom.ui
---@field util custom.util

local M = {}

setmetatable(M, {
  __index = function(t, k)
    ---@diagnostic disable-next-line: no-unknown
    t[k] = require("custom." .. k)
    return t[k]
  end,
})

_G.Custom = M
