local lazy = require("mattr-.util.lazy")

-- Make sure lazy.nvim is installed
lazy.ensure_installed()

-- Do stuff before loading lazy
require("mattr-.config").init()

-- plugins 🎉
require("lazy").setup({
  spec = {
    { import = "mattr-.plugins" },
  },
  defaults = {
    -- Explicitly require plugins to be lazy loaded
    lazy = false,
    version = false, -- always use the latest git commit
    -- version = "*", -- try installing the latest stable version for plugins that support semver
  },
  install = { colorscheme = { "catppuccin", "habamax" } },
  checker = { enabled = true }, -- automatically check for plugin updates
  performance = {
    rtp = {
      -- disable some rtp plugins
      disabled_plugins = {
        "gzip",
        "matchit",
        "matchparen",
        "netrwPlugin",
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
require("mattr-.config").setup()
