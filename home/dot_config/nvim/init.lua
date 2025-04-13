require("dashvim")
-- The `DashVim` global is now available
-- Make sure lazy.nvim is installed
DashVim.lazy.ensure_installed()

-- Do stuff before loading lazy
DashVim.lazy.pre_init()

-- plugins 🎉
require("lazy").setup({
  spec = {
    { import = "dashvim.plugins" }, -- the base set of plugins
    { import = "dashvim.plugins.lang" }, -- language plugins
  },
  defaults = {
    -- Explicitly require plugins to be non-lazy loaded
    lazy = true,
    version = false, -- always use the latest git commit.
  },
  install = { colorscheme = { "catppuccin", "habamax" } },
  checker = { enabled = true }, -- automatically check for plugin updates
  performance = {
    rtp = {
      -- disable some rtp plugins
      disabled_plugins = {
        "gzip",
        "tarPlugin",
        "tohtml",
        "tutor",
        "vimballPlugin",
        "zipPlugin",
      },
    },
  },
})

-- post plugin install initialization
DashVim.lazy.post_install()
