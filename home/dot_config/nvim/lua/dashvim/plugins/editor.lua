return {
  -- neo-tree to replace most of the things i used to do with netrw
  {
    "nvim-neo-tree/neo-tree.nvim",
    cmd = "Neotree",
    keys = {
      { "<leader>fe",
        function()
          require("neo-tree.command").execute({toggle = true})
        end,
        desc = "Explorer NeoTree (root dir)",
      },
    },
    deactivate = function()
      vim.cmd([[Neotree close]])
    end,
    init = function()
      -- use an autocmd for lazy-loading neo-tree instead of directly requiring it
      vim.api.nvim_create_autocmd("BufEnter", {
        group = vim.api.nvim_create_augroup("neotree_start_directory", { clear = true }),
        desc = "Start neo-tree with a directory",
        once = true,
        callback = function()
          if package.loaded["neo-tree"] then
            return
          else
            ---@diagnostic disable-next-line assign-type-mismatch
            local stats = vim.uv.fs_stat(vim.fn.argv(0))
            if stats and stats.type == "directory" then
              require("neo-tree")
            end
          end
        end,
      })
    end,
    opts = {
      sources = { "filesystem", "buffers" },
      open_files_do_not_replace_types = { "terminal", "Trouble", "trouble", "qf", "toggleterm", "edgy" },
      filesystem = {
        bind_to_cwd = false,
        follow_current_file = { enabled = true },
        use_libuv_file_watcher = true,
      },
      buffers = {
        terminals_first = true,
      },
    }
  },
  -- telescope for all the searching
  {
    "nvim-telescope/telescope.nvim",
    version = false, -- telescope doesn't do releases
    cmd = "Telescope",
    keys = {
      { "<leader>,", "<cmd>Telescope buffers show_all_buffers=true<cr>", desc = "Switch Buffer" },
      { "<leader>/", "<cmd>Telescope live_grep<cr>", desc = "Grep (root dir)" },
      { "<leader>:", "<cmd>Telescope command_history<cr>", desc = "Command History" },
      { "<leader>fc", DashVim.util.telescope.config_files, desc = "Find Config File" },
      { "<leader>ff", "<cmd>Telescope find_files<cr>", desc = "Find Files (root dir)" },
      { "<leader>fF", "<cmd>Telescope git_files<cr>", desc = "Find Files (git)" },
      { "<leader>fp", DashVim.util.telescope.plugin_files, desc = "Find Plugin File" },
      { "<leader>gs", "<cmd>Telescope git_status<cr>", desc = "status" },
    },
    opts = {
      defaults = {
        prompt_prefix = " ",
        selection_caret = " ",
        pickers = {
          find_files = {
            -- `hidden = true` will still show the inside of `.git/` as it's not `.gitignore`d.
            find_command = { "rg", "--files", "--hidden", "--glob", "!.git" },
          },
        },
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

  -- replace vim-surround with mini.surround
  -- and emulate mini.surround
  {
    "echasnovski/mini.surround",
    keys = function(_, keys)
      -- Populate the keys based on the options
      local opts = DashVim.lazy.opts("mini.surround")
      local mappings = {
        { opts.mappings.add, desc = "Add surrounding", mode = { "n", "v" } },
        { opts.mappings.delete, desc = "Delete surrounding" },
        { opts.mappings.find, desc = "Find right surrounding" },
        { opts.mappings.find_left, desc = "Find left surrounding" },
        { opts.mappings.highlight, desc = "Highlight surrounding" },
        { opts.mappings.replace, desc = "Replace surrounding" },
        { opts.mappings.update_n_lines, desc = "Update `MiniSurround.config.n_lines`" },
      }
      mappings = vim.tbl_filter(function(m)
        return m[1] and #m[1] > 0
      end, mappings)
      return vim.list_extend(mappings, keys)
    end,
    opts = {
      mappings = {
        add = "ys", -- Add surrounding in Normal and Visual modes
        delete = "ds", -- Delete surrounding
        find = "", -- Find surrounding (to the right)
        find_left = "", -- Find surrounding (to the left)
        highlight = "", -- Highlight surrounding
        replace = "cs", -- Replace surrounding
        update_n_lines = "", -- Update `n_lines`
      },
      search_method = "cover_or_next",
    },
  },

  -- which-key
  {
    "folke/which-key.nvim",
    event = "VeryLazy",
    opts = {
      preset = "helix",
      plugins = { spelling = true },
      defaults = {
        normal_mode = {
          { "<leader>c", group = "code" },
          { "<leader>ct", group = "test" },
          { "<leader>f", group = "+files" },
          { "<leader>fn", group = "new" },
          { "<leader>g", group = "git" },
          { "<leader>gh", group = "github" },
          { "<leader>ghi", group = "issues" },
          { "<leader>ghp", group = "pull requests" },
          { "<leader>gs", group = "stage", },
          { "<leader>gu", group = "unstage", },
          { "<leader>v", group = "vim" },
          { "<leader>vn", group = "noice" },
          { "<leader>w", group = "window" },
        },
        visual_mode = {
          { "<leader>g", group = "git" },
          { "<leader>gs", group = "stage" },
        },
      },
    },
    config = function(_, opts)
      local wk = require("which-key")
      wk.setup(opts)
      wk.add(opts.defaults.normal_mode, { prefix = vim.g.mapleader, mode = "n" })
      wk.add(opts.defaults.visual_mode, { prefix = "<leader>", mode = "v" })
    end,
  },
}
