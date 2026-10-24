return {
  -- lazy.nvim and other common libraries are going here
  { "folke/lazy.nvim", version = "*" },

  -- Snacks
  {
    "folke/snacks.nvim",
    priority = 1000,
    lazy = false,
    --use semver. snacks gets released often, thankfully.
    version = "*",
    opts = {
      toggle = {
        map = DashVim.lazy.integrated_map, -- make Snacks aware of lazy.nvim's key handlers
      },
    },
  },

  -- library used by other plugins
  { "nvim-lua/plenary.nvim", lazy = true },

  -- mini.icons for fancy icons
  -- replaces nvim-web-devicons
  {
    "echasnovski/mini.icons",
    lazy = true,
    init = function()
      package.preload["nvim-web-devicons"] = function()
        require("mini.icons").mock_nvim_web_devicons()
        return package.loaded["nvim-web-devicons"]
      end
    end,
  },

  -- nui is used by a bunch of other things
  { "MunifTanjim/nui.nvim", lazy = true },

}
