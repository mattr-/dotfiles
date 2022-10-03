vim.bo.textwidth = 0
vim.wo.wrap = true
vim.wo.list = false
vim.wo.foldlevel = 1
vim.wo.foldmethod = "syntax"

vim.g.mkdp_markdown_css = string.format("%s/site/markdown_preview.css", vim.fn.stdpath("data"))
