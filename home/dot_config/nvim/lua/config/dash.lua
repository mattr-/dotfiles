local M = {}

function M.ensure_lazy_installed()
  local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
  if not (vim.uv or vim.loop).fs_stat(lazypath) then
    vim.api.nvim_echo({{"Installing lazy.nvim 💤", "MoreMsg"}}, false, {})
    local lazyrepo = "https://github.com/folke/lazy.nvim.git"
    local out = vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })
    if vim.v.shell_error ~= 0 then
      vim.api.nvim_echo({
        { "Failed to clone lazy.nvim:\n", "ErrorMsg" },
        { out, "WarningMsg" },
        { "\nPress any key to exit..." },
      }, true, {})
      vim.fn.getchar()
      os.exit(1)
    end
    vim.api.nvim_echo({{"", "MoreMsg"}}, false, {})
  end

  vim.opt.rtp:prepend(lazypath)
end

function M.load(opts)
  opts = vim.tbl_deep_extend("force", {
    spec = {
      -- add LazyVim and import its plugins
      { "mattr-/DashVim", import = "dashvim.plugins" },
      -- import/override with your plugins
      { import = "plugins" },
    },
    defaults = {
      lazy = true,
      version = false, -- always use the latest git commit
    },
    dev = {
      path = "~/Code/mattr-",
      patterns = { "mattr-" },
    },
    install = { colorscheme = { "tokyonight", "habamax" } },
    checker = {
      enabled = true, -- check for plugin updates periodically
      notify = false, -- notify on update
    }, -- automatically check for plugin updates
    performance = {
      rtp = {
        -- disable some rtp plugins
        disabled_plugins = {
          "gzip",
          "tarPlugin",
          "tohtml",
          "tutor",
          "zipPlugin",
        },
      },
    },
  }, opts or {})

  require("lazy").setup(opts)
end

return M
