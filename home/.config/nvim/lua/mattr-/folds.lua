
local folds = {}

folds.decorate_folds = function()
  local start_line = vim.fn.getline(vim.v.foldstart):gsub("\t", ("\t"):rep(vim.opt.tabstop:get()))
  return string.format("%s (%d lines)", start_line, vim.v.foldend - vim.v.foldstart + 1)
end

return folds
