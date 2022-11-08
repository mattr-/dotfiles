local scan = require("plenary.scandir")
local Path = require("plenary.path")
-- default tooling prototype
local prototype = {
  available = function()
    return false
  end,
  cmd = function()
    return nil
  end,
}

local Features = setmetatable({}, {
  __index = function(table, key)
    return {
      available = function()
        return false
      end,
      cmd = function()
        return nil
      end,
    }
  end,
})

local path = Path:new({ vim.fn.stdpath("config"), "lua", "mattr-", "features", "tools" })
local tools = scan.scan_dir(tostring(path), {
  respect_gitignore = false,
  hidden = false,
  depth = 1,
  add_dirs = false,
})

for _, entry in ipairs(tools) do
  local entry_path = Path:new(entry)
  local tool = loadfile(tostring(entry_path))
  local resolved = tool()

  Features[resolved.tool] = resolved.definition
end

return Features
