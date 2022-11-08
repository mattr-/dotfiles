local Path = require("plenary.path")
local registry = require("mason-registry")

local M = {}

M.path_for = function(package_name)
  local path = Path:new({vim.fn.stdpath("data"), "mason", "bin", package_name})
  return tostring(path)
end

-- Takes a table of package specifiers and ensures they're installed
M.ensure_installed = function(packages)
  local Package = require("mason-core.package")
  for _, package in ipairs(packages) do
    local name, version = Package.Parse(package)
    local pkg = registry.get_package(name)
    if not pkg:is_installed() then
      pkg:install({ version = version })
    end
  end
end

return M
