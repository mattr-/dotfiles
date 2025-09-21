local LazyUtil = require("lazy.core.util")

--- @class dashvim.util: LazyUtilCore
--- @field lsp dashvim.util.lsp
--- @field mini dashvim.util.mini
--- @field telescope dashvim.util.telescope
--- @field tools dashvim.util.tools
local M = {}

setmetatable(M, {
  __index = function(table, key)
   -- Delegate to lazy's core utilities if a key matches a name in that class
    if LazyUtil[key] then
      return LazyUtil[key]
    end

    -- Otherwise, load a module from our util namespace
    table[key] = require("dashvim.util." .. key)
    return table[key]
  end,
})

return M
