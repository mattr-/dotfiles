---@class custom.util.mini

local M = {}

function M.pairs(opts)
  local pairs = require("mini.pairs")
  pairs.setup(opts)
end

return M
