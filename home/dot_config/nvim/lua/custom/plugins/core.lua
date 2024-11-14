return {
  -- lazy.nvim and other common libraries are going here
  { "folke/lazy.nvim", version = "*" },

  -- library used by other plugins
  { "nvim-lua/plenary.nvim", lazy = true },

  -- mini.icons for fancy icons
  -- replaces nvim-web-devicons
  {
    "echasnovski/mini.icons",
    lazy = true,
    opts = {},
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
