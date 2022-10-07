return function()
  local wk = require("which-key")
  wk.setup({
    window = {
      winblend = 10,
    },
    layout = {
      align = "center",
    },
  })

  local normal_group_mappings = {
    c = {
      name = "code",
      t = {
        name = "test",
      }
    },
    f = {
      name = "files",
      n = {
        name = "new",
      }
    },
    g = {
      name = "git",
      h = {
        name = "github",
        i = {
          name = "issues",
        },
        p = {
          name = "pull pequests"
        }
      },
      s = {
        name = "stage",
      },
      u = {
        name = "unstage",
      },
    },
    h = {
      name = "help",
      p = {
        name = "packer",
      }
    },
    t = {
      name = "toggle"
    },
    w = {
      name = "window"
    },
  }

  local visual_group_mappings = {
    g = {
      name = "git",
      s = {
        name = "stage",
      },
    },
  }
  wk.register(normal_group_mappings, { prefix = "<leader>" })
  wk.register(visual_group_mappings, { prefix = "<leader>", mode = "v" })

  -- Set up the keybindings here, since they rely on which-key being loaded
  require "mattr-.keybindings"
end