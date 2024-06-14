local lazy = require("custom.util.lazy")

-- Make sure lazy.nvim is installed
lazy.ensure_installed()

-- Do stuff before loading lazy
-- The `Custom` global is now available
require("custom.config").init()

-- plugins 🎉
require("lazy").setup({
  spec = {
    { import = "custom.plugins" }, -- the base set of plugins
    { import = "custom.plugins.lang" }, -- language specific extensions
  },
  defaults = {
    -- Explicitly require plugins to be lazy loaded
    lazy = false,
    version = false, -- always use the latest git commit
    -- version = "*", -- try installing the latest stable version for plugins that support semver
  },
  install = { colorscheme = { "tokyonight", "habamax" } },
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
Custom.config.setup()
