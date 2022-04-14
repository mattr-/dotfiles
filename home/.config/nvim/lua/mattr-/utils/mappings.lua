
local mappings = {}
local module_utils = require("mattr-.utils.modules")
local wk = require("which-key")

if module_utils.is_plugin_available("nvim-mapper") then
  local mapper = require("nvim-mapper")

  mappings.map = function(mode, keys, cmd, options, category, unique_identifier, description)
    mapper.map(mode, keys, cmd, options, category, unique_identifier, description)
    wk.register({[keys] = {description}}, { mode = mode })
  end

  mappings.map_buf = function(bufnr, mode, keys, cmd, options, category, unique_identifier, description)
    mapper.map_buf(bufnr, mode, keys, cmd, options, category, unique_identifier, description)
    wk.register({[keys] = {description}}, { mode = mode, buffer = bufnr })
  end

  mappings.map_virtual = function(mode, keys, cmd, options, category, unique_identifier, description)
    mapper.map_virtual(mode, keys, cmd, options, category, unique_identifier, description)
  end

  mappings.map_buf_virtual = function(mode, keys, cmd, options, category, unique_identifier, description)
    mapper.map_buf_virtual(mode, keys, cmd, options, category, unique_identifier, description)
  end

  mappings.nmap = function(keys, cmd, options, category, unique_indentifer, description)
    mappings.map("n", keys, cmd, options, category, unique_indentifer, description)
  end

  mappings.vmap = function(keys, cmd, options, category, unique_indentifer, description)
    mappings.map("v", keys, cmd, options, category, unique_indentifer, description)
  end
else
  mappings.map = function(mode, keys, cmd, _, _, _, description)
    wk.register({[keys] = {cmd, description}}, { mode = mode })
  end
  mappings.map_buf = function(mode, keys, cmd, _, _, _, description)
    wk.register({[keys] = {cmd, description}}, { mode = mode, buffer = 0 })
  end
  mappings.map_virtual = function(_, _, _, _, _, _, _) end
  mappings.map_buf_virtual = function(_, _, _, _, _, _, _) end

  mappings.nmap = function(keys, cmd, options, category, unique_indentifer, description)
    mappings.map("n", keys, cmd, options, category, unique_indentifer, description)
  end

  mappings.vmap = function(keys, cmd, options, category, unique_indentifer, description)
    mappings.map("v", keys, cmd, options, category, unique_indentifer, description)
  end

end

return mappings

