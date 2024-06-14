---@class custom.util.tools
local M = {
}

local has_rubocop_file = function()
  return (
    vim.fn.filereadable(".rubocop.yml") == 1
    or vim.fn.filereadable(".rubocop_todo.yml") == 1
    or vim.fn.filereadable(".rubocop.yaml") == 1
    or vim.fn.filereadable(".rubocop_todo.yaml") == 1
  )
end

local rubocop_in_gemfile = function()
  if vim.fn.filereadable("Gemfile") then
    local file = io.open("Gemfile")
    if file then
      for line in file:lines() do
        if line:match("rubocop") then
          file:close()
          return true
        end
      end
    end
  end
end

M.rubocop_configured = function()
  return has_rubocop_file() or rubocop_in_gemfile()
end

M.sorbet_configured = function()
  return vim.fn.filereadable("sorbet/config") == 1
end

return M
