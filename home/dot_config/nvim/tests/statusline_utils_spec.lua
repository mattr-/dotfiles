
slu = require("dashvim.statusline.utils")
describe("StatusLine Utilities", function()
  before_each(function()
    vim.api.nvim_set_hl(0, "bg_test", { bg = "#123456" })
  end)
  
  it("falls back on defaults when needed", function()
    assert.are.same("#000000", slu.extract_color_from_highlight_list({highlights = { "Nonexistent" }}))
  end)

  it("returns colors based on scope and highlights", function()
    assert.are.same("#123456", slu.extract_color_from_highlight_list({ scope = "bg", highlights = { "bg_test" } }))
  end)

  after_each(function()
    vim.api.nvim_set_hl(0, "bg_test", {})
  end)
end)
