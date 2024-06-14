-- Set up some dynamic feature sets, mostly for languages that support using more than one tool.
-- (Lookin' at you Ruby)

local scan = require("plenary.scandir")
local Path = require("plenary.path")

local Features = setmetatable({}, {
  __index = function(_, _)
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

local path = Path:new({ vim.fn.stdpath("config"), "lua", "custom", "features", "tools" })
local tools = scan.scan_dir(tostring(path), {
  respect_gitignore = false,
  hidden = false,
  depth = 1,
  add_dirs = false,
})

for _, entry in ipairs(tools) do
  local entry_path = Path:new(entry)
  local tool = loadfile(tostring(entry_path))
  local resolved = tool and tool()

  if resolved then
    Features[resolved.tool] = resolved.definition
  end
end

return Features
