return {
  -- snippets
  {
    "L3MON4D3/LuaSnip",
    build = (not jit.os:find("Windows"))
      and "echo 'NOTE: jsregexp is optional, so not a big deal if it fails to build'; make install_jsregexp"
      or nil,
    opts = function()
      local types = require("luasnip.util.types")
      return {
        history = true,
        delete_check_events = "TextChanged",
        ext_opts = {
          [types.choiceNode] = {
            active = { virt_text = {{ "●", require("catppuccin.palettes").get_palette("mocha")["pink"] }} }
          },
        },
      }
    end,
    -- stylua: ignore
    keys = {
      {
        "<tab>",
        function()
          return require("luasnip").jumpable(1) and "<Plug>luasnip-jump-next" or "<tab>"
        end,
        expr = true, silent = true, mode = "i",
      },
      { "<tab>", function() require("luasnip").jump(1) end, mode = "s" },
      { "<s-tab>", function() require("luasnip").jump(-1) end, mode = { "i", "s" } },
    },
    config = function(_, opts)
      local ls = require("luasnip")
      ls.setup(opts)

      -- Dynamically load any custom snippets I've put in place
      for _, snippet_path in ipairs(vim.api.nvim_get_runtime_file("lua/custom/snippets/*.lua", true)) do
        loadfile(snippet_path)()
      end
    end,
  },

  -- autocompletion
  {
    "hrsh7th/nvim-cmp",
    event = "InsertEnter",
    dependencies = {
      "hrsh7th/cmp-buffer", -- Completion from other buffers
      "hrsh7th/cmp-path", -- Completion for filesystem paths
      "saadparwaiz1/cmp_luasnip", -- Completion from LuaSnip snippets
      "hrsh7th/cmp-cmdline",
    },
    opts = function()
      vim.api.nvim_set_hl(0, "CmpGhostText", { link = "Comment", default = true })
      local cmp = require("cmp")
      local defaults = require("cmp.config.default")()
      return {
        completion = {
          completeopt = vim.o.completeopt
        },
        snippet = {
          expand = function(args)
            require("luasnip").lsp_expand(args.body)
          end,
        },
        mapping = cmp.mapping.preset.insert({
          ["<C-n>"] = cmp.mapping.select_next_item({ behavior = cmp.SelectBehavior.Insert }),
          ["<C-p>"] = cmp.mapping.select_prev_item({ behavior = cmp.SelectBehavior.Insert }),
          ["<C-u>"] = cmp.mapping.scroll_docs(-4),
          ["<C-d>"] = cmp.mapping.scroll_docs(4),
          ["<C-Space>"] = cmp.mapping.complete(),
          ["<C-e>"] = cmp.mapping.abort(),
          ["<CR>"] = cmp.mapping.confirm({ select = true }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
          ["<S-CR>"] = cmp.mapping.confirm({
            behavior = cmp.ConfirmBehavior.Replace,
            select = true,
          }), -- Accept currently selected item. Set `select` to `false` to only confirm explicitly selected items.
        }),
        sources = cmp.config.sources({
          -- { name = "nvim_lsp" },
          { name = "luasnip" },
          { name = "path" },
        }),
        formatting = {
          format = function(_, item)
            local icons = Custom.config.icons.kinds
            if icons[item.kind] then
              item.kind = icons[item.kind] .. item.kind
            end
            return item
          end,
        },
        experimental = {
          ghost_text = {
            hl_group = "CmpGhostText",
          },
        },
        sorting = defaults.sorting,
        view = {
          entries = {
            follow_cursor = true,
          },
        },
      }
    end,
  },

  {
    "echasnovski/mini.pairs",
    event = "VeryLazy",
    opts = {
      mappings = {
        -- disable the backtick mapping. Didn't care for it in my testing
        ["`"] = false
      },
    },
    keys = {
      {
        "<leader>vp",
        function()
          vim.g.minipairs_disable = not vim.g.minipairs_disable
          if vim.g.minipairs_disable then
            Custom.ui.warn("Auto Pairs", "Disabled", { render = "compact" })
          else
            Custom.ui.info("Auto Pairs", "Enabled", { render = "compact" })
          end
        end,
        desc = "Toggle Auto Pairs",
      },
    },
  },

  {
    "echasnovski/mini.ai",
    event = "VeryLazy",
    opts = function()
      -- TODO hook up which key bindings
      local ai = require("mini.ai")
      return {
        n_lines = 100,
        custom_textobjects = {
          -- code block
          o = ai.gen_spec.treesitter({
            a = { "@block.outer", "@conditional.outer", "@loop.outer" },
            i = { "@block.inner", "@conditional.inner", "@loop.inner" },
          }),
          -- function/method
          f = ai.gen_spec.treesitter({ a = "@function.outer", i = "@function.inner" }),
          -- class
          c = ai.gen_spec.treesitter({ a = "@class.outer", i = "@class.inner" }),
        },
      }
    end,
    keys = {
      {
        "<leader>va",
        function()
          vim.g.miniai_disable = not vim.g.miniai_disable
          if vim.g.miniai_disable then
            Custom.ui.warn("Mini TextObjects", "Disabled", { render = "compact" })
          else
            Custom.ui.info("Mini TextObjects", "Enabled", { render = "compact" })
          end
        end,
        desc = "Toggle Mini TextObjects",
      },
    },
  },

  -- testing core support
  {
    "nvim-neotest/neotest",
    opts = {
      adapters = {},
      status = { virtual_text = true },
      output = { open_on_run = true },
      quickfix = {
        open = function()
          require("trouble").open({ mode = "quickfix", focus = false })
        end,
      },
    },
    config = function(_, opts)
      local adapters = {}
      for name, config in pairs(opts.adapters or {}) do
        if type(name) == "number" then
          if type(config) == "string" then
            config = require(config)
          end
          adapters[#adapters + 1] = config
        elseif config ~= false then
          local adapter = require(name)
          if type(config) == "table" and not vim.tbl_isempty(config) then
            local meta = getmetatable(adapter)
            if adapter.setup then
              adapter.setup(config)
            elseif meta and meta.__call then
              adapter(config)
            else
              error("Adapter " .. name .. " does not support setup")
            end
          end
          adapters[#adapters + 1] = adapter
        end
      end

      opts.adapters = adapters

      require("neotest").setup(opts)
    end,
    keys = {
      { "<leader>ctt", function() require("neotest").run.run(vim.fn.expand("%")) end, desc = "Run File" },
      { "<leader>ctT", function() require("neotest").run.run(vim.uv.cwd()) end, desc = "Run All Test Files" },
      { "<leader>ctr", function() require("neotest").run.run() end, desc = "Run Nearest" },
      { "<leader>cts", function() require("neotest").summary.toggle() end, desc = "Toggle Summary" },
      { "<leader>cto", function() require("neotest").output.open({ enter = true, auto_close = true }) end, desc = "Show Output" },
      { "<leader>ctO", function() require("neotest").output_panel.toggle() end, desc = "Toggle Output Panel" },
      { "<leader>ctS", function() require("neotest").run.stop() end, desc = "Stop" },
      { "<localleader>tt", function() require("neotest").run.run(vim.fn.expand("%")) end, desc = "Run File" },
      { "<localleader>tT", function() require("neotest").run.run(vim.uv.cwd()) end, desc = "Run All Test Files" },
      { "<localleader>tn", function() require("neotest").run.run() end, desc = "Run Nearest" },
    },
  }
}
