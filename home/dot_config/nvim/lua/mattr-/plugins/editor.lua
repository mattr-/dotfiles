return {
  {
    "nvim-telescope/telescope.nvim",
    version = false, -- telescope doesn't do releases
    cmd = "Telescope",
    keys = {
      { "<leader>,", "<cmd>Telescope buffers show_all_buffers=true<cr>", desc = "Switch Buffer" },
      { "<leader>/", "<cmd>Telescope live_grep<cr>", desc = "Grep (root dir)" },
      { "<leader>:", "<cmd>Telescope command_history<cr>", desc = "Command History" },
      { "<leader>ff", "<cmd>Telescope find_files<cr>", desc = "Find Files (root dir)" },
      { "<leader>gs", "<cmd>Telescope git_status<cr>", desc = "status" },
    },
    opts = {
      defaults = {
        prompt_prefix = " ",
        selection_caret = " ",
        mappings = {
          i = {
            ["<C-f>"] = function(...)
              return require("telescope.actions").preview_scrolling_down(...)
            end,
            ["<C-b>"] = function(...)
              return require("telescope.actions").preview_scrolling_up(...)
            end,
          },
          n = {
            ["q"] = function(...)
              return require("telescope.actions").close(...)
            end,
          },
        },
      },
    },
    dependencies = {
      "nvim-telescope/telescope-fzf-native.nvim",
      build = "make",
      config = function()
        require("telescope").load_extension("fzf")
      end,
    },
  },

  -- which-key
  {
    "folke/which-key.nvim",
    event = "VeryLazy",
    opts = {
      plugins = { spelling = true },
      defaults = {
        normal_mode = {
          c = {
            name = "code",
            t = { name = "test", }
          },
          f = {
            name = "+files",
            n = { name = "new", }
          },
          g = {
            name = "git",
            h = {
              name = "github",
              i = { name = "issues", },
              p = { name = "pull requests" }
            },
            s = { name = "stage", },
            u = { name = "unstage", },
          },
          h = {
            name = "help",
            p = { name = "packer", }
          },
          t = { name = "toggle" },
          w = { name = "window" },
        },
        visual_mode = {
          g = {
            name = "git",
            s = { name = "stage", },
          },
        },
      },
    },
    config = function(_, opts)
      local wk = require("which-key")
      wk.setup(opts)
      wk.register(opts.defaults.normal_mode, { prefix = vim.g.mapleader })
      wk.register(opts.defaults.visual_mode, { prefix = "<leader>", mode = "v" })
    end,
  },
}
