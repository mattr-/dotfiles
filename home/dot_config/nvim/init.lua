-- bootstrap lazy.nvim, DashVim and your plugins
local dash = require("config.dash")
dash.ensure_lazy_installed()
dash.load({
  dev = {
    path = "~/Code/mattr-",
    patterns = { "mattr-" },
  }
})

vim.opt.mouse = ""
vim.opt.laststatus = 3
vim.opt.expandtab = true
vim.opt.softtabstop = 2
vim.opt.shiftwidth = 2
vim.opt.tabstop = 90

vim.cmd([[colorscheme catppuccin]])

-- vim.opt.statusline = "%#MiniIconsAzure#%-02{v:lua.MiniIcons.get('filetype','lua')}%#NormalFloat#%f"
