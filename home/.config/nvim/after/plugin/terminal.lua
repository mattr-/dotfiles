local status_ok, toggleterm = pcall(require, "toggleterm")
if not status_ok then
  return
end

toggleterm.setup({
  open_mapping = [[<c-\>]],
  direction = 'float',
  hide_numbers = true,
  insert_mappings = true,
  start_in_insert = true,
  float_opts = {
    border = "single",
  }
})

-- For terminals, don't display line numbers
vim.cmd [[
augroup terminal_customizations
  autocmd!
  autocmd TermOpen * setl nonumber norelativenumber
augroup END
]]

