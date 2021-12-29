local status_ok, toggleterm = pcall(require, "toggleterm")
if not status_ok then
  return
end

toggleterm.setup({
  hide_numbers = true,
  insert_mappings = false,
  float_opts = {
    border = "single",
  }
})
