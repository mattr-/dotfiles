return {
  -- Treesitter git support
  {
    "nvim-treesitter/nvim-treesitter",
    opts = { ensure_installed = { "git_config", "gitcommit", "git_rebase", "gitignore", "gitattributes" } },
  },

  -- Give ourselves a nicer commit window when commiting from the command line
  {
    "rhysd/committia.vim",
    lazy = false
  },

  -- Inline git diff signs
  {
    "lewis6991/gitsigns.nvim",
    event = "VeryLazy",
    opts = {
      current_line_blame = true,
      preview_config = {
        border = "rounded",
      },
      on_attach = function(bufnr)
        local gitsigns = require("gitsigns")

        vim.keymap.set("n", "]c", function()
          if vim.wo.diff then
            vim.cmd.normal({ "]c", bang = true })
          else
            gitsigns.nav_hunk("next", { preview = false })
          end
        end, { buffer = bufnr, desc = "Next hunk"})

        vim.keymap.set("n", "[c", function()
          if vim.wo.diff then
            vim.cmd.normal({ "[c", bang = true })
          else
            gitsigns.nav_hunk("prev", { preview = false })
          end
        end, { buffer = bufnr, desc =  "Previous hunk"})

        vim.keymap.set("n", "<leader>gh", gitsigns.preview_hunk_inline, { buffer = bufnr, desc = "Preview hunk inline"})
        vim.keymap.set("n", "<leader>gr", gitsigns.preview_hunk, { buffer = bufnr, desc = "Preview hunk"})
        vim.keymap.set("n", "<leader>gR", gitsigns.blame, { buffer = bufnr, desc = "Preview blame"})
        vim.keymap.set("n", "<leader>gH", gitsigns.blame_line, { buffer = bufnr, desc = "Preview blame line"})
      end,
    },
  },
}
