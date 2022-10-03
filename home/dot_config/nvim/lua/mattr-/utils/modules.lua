-- Check if a lua module is loaded.
-- Pulled from the readme of lazytanuki/nvim-mapper

local modules = {}

modules.is_plugin_available = function(plugin)
  if package.loaded[plugin] then
    return true
  else
    for _, searcher in ipairs(package.searchers or package.loaders) do
      local loader = searcher(plugin)
      if type(loader) == 'function' then
        package.preload[plugin] = loader
        return true
      end
    end
    return false
  end
end

return modules
