-- bootstrap lazy.nvim, DashVim and your plugins
local dash = require("config.dash")
dash.ensure_lazy_installed()
dash.load({
  dev = {
    path = "~/Code/mattr-",
    patterns = { "mattr-" },
  }
})

vim.opt.laststatus = 3

vim.cmd([[colorscheme catppuccin]])
vim.cmd([[:nnoremap j gj]])
vim.cmd([[:nnoremap k gk]])

-- vim.opt.statusline = "%#MiniIconsAzure#%-02{v:lua.MiniIcons.get('filetype','lua')}%#NormalFloat#%f"
