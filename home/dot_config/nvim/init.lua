-- **LazyDone**: when lazy has finished starting up and loaded your config
-- **LazySync**: after running sync
-- **LazyInstall**: after an install
-- **LazyUpdate**: after an update
-- **LazyClean**: after a clean
-- **LazyCheck**: after checking for updates
-- **LazyLog**: after running log
-- **LazyLoad**: after loading a plugin. The `data` attribute will contain the plugin name.
-- **LazySyncPre**: before running sync
-- **LazyInstallPre**: before an install
-- **LazyUpdatePre**: before an update
-- **LazyCleanPre**: before a clean
-- **LazyCheckPre**: before checking for updates
-- **LazyLogPre**: before running log
-- **LazyReload**: triggered by change detection after reloading plugin specs
-- **VeryLazy**: triggered after `LazyDone` and processing `VimEnter` auto commands
-- **LazyVimStarted**: triggered after `UIEnter` when `require("lazy").stats().startuptime` has been calculated.
--    Useful to update the startuptime on your dashboard.

local lazy_group = vim.api.nvim_create_augroup("LazyTesting", {})
local events_to_track = {
  "LazyDone",
  "LazySync",
  "LazyInstall",
  "LazyUpdate",
  "LazyClean",
  "LazyCheck",
  "LazyLog",
  "LazyLoad",
  "LazySyncPre",
  "LazyInstallPre",
  "LazyUpdatePre",
  "LazyCleanPre",
  "LazyCheckPre",
  "LazyLogPre",
  "LazyReload",
  "VeryLazy",
  "LazyVimStarted",
}
for _, group in ipairs(events_to_track) do
  vim.api.nvim_create_autocmd("User", {
    group = lazy_group,
    pattern = group,
    callback = function(ev)
      vim.notify(string.format("event fired: %s %s", ev.event, ev.file))
    end,
  })
end

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
vim.cmd([[:nnoremap j gj]])
vim.cmd([[:nnoremap k gk]])

-- vim.opt.statusline = "%#MiniIconsAzure#%-02{v:lua.MiniIcons.get('filetype','lua')}%#NormalFloat#%f"
