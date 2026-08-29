return {
  {
    "nvim-treesitter/nvim-treesitter",
    version = false, -- treesitter doesn't do releases
    branch = "main",
    build = ":TSUpdate",
    lazy = false,
    opts_extend = { "ensure_installed" },
    ---@type TSConfig
    ---@diagnostic disable-next-line: missing-fields
    opts = {
      ensure_installed = {
        "bash",
        "c",
        "html",
        "javascript",
        "json",
        "markdown",
        "markdown_inline",
        "python",
        "query",
        "regex",
        "tsx",
        "typescript",
        "vim",
        "vimdoc",
        "yaml",
      },
    },
    config = function(_, opts)
      if type(opts.ensure_installed) == "table" then
        ---@type table<string, boolean>
        local added = {}
        opts.ensure_installed = vim.tbl_filter(function(lang)
          if added[lang] then
            return false
          end
          added[lang] = true
          return true
        end, opts.ensure_installed)
      end

      require("nvim-treesitter").install(opts.ensure_installed)

      vim.api.nvim_create_autocmd("FileType", {
        callback = function(args)
          local lang = vim.treesitter.language.get_lang(args.match) or args.match
          if not vim.treesitter.language.add(lang) then
            return
          end
          vim.treesitter.start(args.buf, lang)
          vim.bo[args.buf].indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"
        end,
      })

      -- Neovim 0.12 built-in: an (parent node), in (child node), ]n/[n (siblings)
      vim.keymap.set("n", "<C-space>", "van", { remap = true, desc = "Increment selection" })
      vim.keymap.set("x", "<C-space>", "an", { remap = true, desc = "Increment selection" })
      vim.keymap.set("x", "<bs>", "in", { remap = true, desc = "Decrement selection" })
    end,
  },

  {
    "nvim-treesitter/nvim-treesitter-textobjects",
    branch = "main",
    dependencies = { "nvim-treesitter/nvim-treesitter" },
    config = function()
      require("nvim-treesitter-textobjects").setup({
        move = { set_jumps = true },
      })

      local move = require("nvim-treesitter-textobjects.move")
      local map = function(lhs, fn, desc)
        vim.keymap.set({ "n", "x", "o" }, lhs, fn, { desc = desc })
      end
      map("]f", function() move.goto_next_start("@function.outer", "textobjects") end, "Next function start")
      map("]c", function() move.goto_next_start("@class.outer", "textobjects") end, "Next class start")
      map("]F", function() move.goto_next_end("@function.outer", "textobjects") end, "Next function end")
      map("]C", function() move.goto_next_end("@class.outer", "textobjects") end, "Next class end")
      map("[f", function() move.goto_previous_start("@function.outer", "textobjects") end, "Prev function start")
      map("[c", function() move.goto_previous_start("@class.outer", "textobjects") end, "Prev class start")
      map("[F", function() move.goto_previous_end("@function.outer", "textobjects") end, "Prev function end")
      map("[C", function() move.goto_previous_end("@class.outer", "textobjects") end, "Prev class end")
    end,
  },
}
