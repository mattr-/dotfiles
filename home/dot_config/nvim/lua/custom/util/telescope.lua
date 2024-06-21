---@class custom.util.telescope
local M = {}
M.config_files = function()
  local Telescope = require("telescope.builtin")
  Telescope.find_files({cwd = vim.fn.stdpath("config")})
end

return M
