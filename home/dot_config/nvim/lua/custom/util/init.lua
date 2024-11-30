local LazyUtil = require("lazy.core.util")

--- @class custom.util: LazyUtilCore
--- @field lsp custom.util.lsp
--- @field telescope custom.util.telescope
--- @field tools custom.util.tools
local M = {}

setmetatable(M, {
  __index = function(table, key)
   -- Delegate to lazy's core utilities if a key matches a name in that class
    if LazyUtil[key] then
      return LazyUtil[key]
    end

    -- Otherwise, load a module from our util namespace
    table[key] = require("custom.util." .. key)
    return table[key]
  end,
})

return M
