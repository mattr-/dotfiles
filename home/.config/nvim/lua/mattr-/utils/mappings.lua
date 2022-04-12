
local mappings = {}
local module_utils = require("lua.mattr-.utils.modules")

if module_utils.is_plugin_available("nvim-mapper") then
      local mapper = require("nvim-mapper")

    mappings.map = function(mode, keys, cmd, options, category, unique_identifier,
                     description)
        mapper.map(mode, keys, cmd, options, category, unique_identifier,
                   description)
    end
    mappings.map_buf = function(bufnr, mode, keys, cmd, options, category, unique_identifier,
                         description)
        mapper.map_buf(bufnr, mode, keys, cmd, options, category, unique_identifier,
                       description)
    end
    mappings.map_virtual = function(mode, keys, cmd, options, category,
                             unique_identifier, description)
        mapper.map_virtual(mode, keys, cmd, options, category,
                           unique_identifier, description)
    end
    mappings.map_buf_virtual = function(mode, keys, cmd, options, category,
                                 unique_identifier, description)
        mapper.map_buf_virtual(mode, keys, cmd, options, category,
                               unique_identifier, description)
    end
else
    mappings.map = function(mode, keys, cmd, options, _, _, _)
        vim.api.nvim_set_keymap(mode, keys, cmd, options)
    end
    mappings.map_buf = function(mode, keys, cmd, options, _, _, _)
        vim.api.nvim_buf_set_keymap(mode, keys, cmd, options)
    end
    mappings.map_virtual = function(_, _, _, _, _, _, _) end
    mappings.map_buf_virtual = function(_, _, _, _, _, _, _) end

end

return mappings

