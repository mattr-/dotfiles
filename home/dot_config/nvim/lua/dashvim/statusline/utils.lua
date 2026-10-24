  -- normal  = utils.extract_color_from_hllist('bg', { 'PmenuSel', 'PmenuThumb', 'TabLineSel' }, '#000000'),
  -- insert  = utils.extract_color_from_hllist('fg', { 'String', 'MoreMsg' }, '#000000'),
  -- replace = utils.extract_color_from_hllist('fg', { 'Number', 'Type' }, '#000000'),
  -- visual  = utils.extract_color_from_hllist('fg', { 'Special', 'Boolean', 'Constant' }, '#000000'),
  -- command = utils.extract_color_from_hllist('fg', { 'Identifier' }, '#000000'),
  -- back1   = utils.extract_color_from_hllist('bg', { 'Normal', 'StatusLineNC' }, '#000000'),
  -- fore    = utils.extract_color_from_hllist('fg', { 'Normal', 'StatusLine' }, '#000000'),
  -- back2   = utils.extract_color_from_hllist('bg', { 'StatusLine' }, '#000000'),

local M = {}

function M.extract_color_from_highlight_list(opts)
  default_opts = { scope = "bg", highlights = { "StatusLine" }, default = "#000000" }
  opts = vim.tbl_extend("force", default_opts, opts or {})

  for _, highlight in ipairs(opts.highlights) do
    hlgroup = vim.api.nvim_get_hl(0, { name = highlight })
    if hlgroup[opts.scope] then
      return string.format("#%06x", hlgroup[opts.scope])
    end
  end

  return opts.default
end


return M
