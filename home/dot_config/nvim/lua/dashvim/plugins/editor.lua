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
  -- fzf based searching
  {
    "ibhagwan/fzf-lua",
    lazy = true,
    opts = function(_, opts)
      local fzf = require("fzf-lua")
      local config = fzf.config

      -- Move list to the Quickfix window
      config.defaults.keymap.fzf["ctrl-q"] = "select-all+accept"
      config.defaults.keymap.fzf["ctrl-u"] = "half-page-up"
      config.defaults.keymap.fzf["ctrl-d"] = "half-page-down"
      config.defaults.keymap.fzf["ctrl-x"] = "jump"
      config.defaults.keymap.fzf["ctrl-f"] = "preview-page-down"
      config.defaults.keymap.fzf["ctrl-b"] = "preview-page-up"
      config.defaults.keymap.builtin["<c-f>"] = "preview-page-down"
      config.defaults.keymap.builtin["<c-b>"] = "preview-page-up"
    end,
    keys = {
      { "<space>ff", "<cmd>FzfLua files<cr>", desc = "Find Files", },
      { "<space>/", "<cmd>FzfLua live_grep<cr>", desc = "Live Grep", },
      { "<space>sh", "<cmd>FzfLua highlights<cr>", desc = "Search Highlight Groups", },
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
