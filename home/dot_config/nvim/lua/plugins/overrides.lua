return {
  --disable bufferline
  {
    "akinsho/bufferline.nvim",
    enabled = false,
  },
  --disable indentline
  {
    "snacks.nvim",
    opts = {
      indent = { enabled = false },
    },
  },
  --reconfig noice
  {
    "folke/noice.nvim",
    opts = {
      cmdline = {
        view = "cmdline",
        format = {
          -- fix icons with new nerd fonts
          search_down = { kind = "search", pattern = "^/", icon = "󱁴 ", lang = "regex" },
          search_up = { kind = "search", pattern = "^%?", icon = "󱁴 ", lang = "regex" },
        },
      },
      routes = {
        {
          filter = {
            event = "msg_show",
            any = {
              { find = "%d+L, %d+B" },
              { find = "; after #%d+" },
              { find = "; before #%d+" },
              { find = "%d+ more line" },
              { find = "%d+ fewer line" },
              { find = "%d lines?" },
              { find = "Already at oldest change" },
              { find = "^E486" }, -- The couldn't find it error
            },
          },
          view = "mini",
        },
      },
    },
  },
}
