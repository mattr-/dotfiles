local packer_bootstrap = false

local packer_path = string.format("%s/site/pack/packer/opt/packer.nvim", vim.fn.stdpath("data"))
if vim.fn.empty(vim.fn.glob(packer_path)) > 0 then
  packer_bootstrap = true
  print("Bootstrapping packer. Please wait...")
  vim.fn.mkdir(packer_path, "p")
  vim.fn.system({
    "git",
    "clone",
    "https://github.com/wbthomason/packer.nvim",
    packer_path,
  })
end

vim.cmd([[packadd packer.nvim]])

local packer = require("packer")

packer.startup({
  function(use)
    use({
      "wbthomason/packer.nvim",
      opt = true,
    })

    -- UI and Colors
    use({
      "norcalli/nvim-colorizer.lua",
      config = require("mattr-.config.colorizer"),
    })
    use("kyazdani42/nvim-web-devicons")
    use({
      "folke/tokyonight.nvim",
      config = function()
        require("tokyonight").setup({
          styles = {
            comments = { italic = true },
            keywords = { italic = false },
          },
          lualine_bold = true
        })
      end,
    })

    use({
      "folke/which-key.nvim",
      config = require("mattr-.config.which-key"),
    })
    use("folke/trouble.nvim") -- a problem list plugin
    use({
      "glepnir/dashboard-nvim",
      commit = "a36b3232c98616149784f2ca2654e77caea7a522",
      config = require("mattr-.config.dashboard"),
    })
    use({
      "nvim-lualine/lualine.nvim",
      config = require("mattr-.config.lualine"),
      requires = { "kyazdani42/nvim-web-devicons", opt = true }
    })

    -- Libraries
    use({
      "nvim-lua/plenary.nvim",
    })

    -- Fuzzy Finding
    use({
      "nvim-telescope/telescope.nvim", -- Fuzzy finding
      config = require("mattr-.config.telescope"),
      wants = {
        "plenary.nvim",
        "telescope-fzf-native.nvim",
        "telescope-ui-select.nvim",
        "nvim-notify"
      },
      requires = {
        {
          "nvim-telescope/telescope-fzf-native.nvim", --fzf based searching
          run = "make",
        },
        {
          "nvim-telescope/telescope-ui-select.nvim", -- Use telescope as a backend for vim.ui.select (NeoVim 0.6)
        },
        {
          "rcarriga/nvim-notify",
          config = require("mattr-.config.notify"),
        },
      }
    })
    use({
      "lazytanuki/nvim-mapper", -- keymapping search
      config = function()
        require("nvim-mapper").setup({
          no_map = true,
          action_on_enter = "execute",
        })
      end,
    })

    -- -- UI and Colors
    -- use({
    --   "onsails/lspkind.nvim",
    -- })


    -- Language Support Plugins
    -- Tree Sitter
    -- use({
    --   "nvim-treesitter/nvim-treesitter",
    --   config = require("mattr-.config.treesitter"),
    -- })
    -- use("nvim-treesitter/playground")
    -- use("JoosepAlviste/nvim-ts-context-commentstring") -- Context aware comment strings

    -- -- LSP
    use({
      "neovim/nvim-lspconfig",
      after = "mason-lspconfig.nvim",
    })
    use({
      "williamboman/mason.nvim",
      config = require("mattr-.config.mason"),
    })
    use({
      "williamboman/mason-lspconfig.nvim",
      requires = { 'williamboman/mason.nvim' }
    })

    -- -- Completion and Snippets
    -- use({
    --   "L3MON4D3/LuaSnip", -- Snippets plugin
    -- --   event = "BufReadPre",
    --   config = require("mattr-.config.luasnip"),
    -- })
    -- use({
    --   "hrsh7th/nvim-cmp", -- Autocompletion plugin
    --   config = require("mattr-.config.nvim-cmp"),
    --   wants = { "LuaSnip" },
    --   event = "InsertEnter",
    -- })
    -- use({"hrsh7th/cmp-buffer", -- Completion from other buffers
    --   after = "nvim-cmp",
    -- })
    -- use({"hrsh7th/cmp-path", -- Completion for filesystem paths
    --   after = "nvim-cmp",
    -- })
    -- use({"hrsh7th/cmp-nvim-lsp", -- Completion from LSP servers
    --   after = "nvim-cmp",
    -- })
    -- use({"hrsh7th/cmp-nvim-lua", -- Completion for neovim's API
    --   after = "nvim-cmp",
    -- })
    -- use({
    --   "saadparwaiz1/cmp_luasnip", -- Completion from LuaSnip snippets
    --   after = "nvim-cmp",
    -- })

    -- -- Language specific additions for LSP
    -- use("b0o/schemastore.nvim") -- Schemas for jsonls

    -- -- Non LSP editing support. Markdown in particular
    -- use({
    --   "iamcco/markdown-preview.nvim",
    --   run = function() vim.fn["mkdp#util#install"]() end
    -- })

    -- Utilities
    use("tpope/vim-surround")
    use("tpope/vim-repeat") -- Repeating plugin maps
    use("tpope/vim-abolish") -- Abbreviate, substitution, and coercion
    use("tpope/vim-commentary") -- Comment all the things!
    use({
      "tpope/vim-dispatch",
      cmd = { "Dispatch", "Make" },
    })
    use("tpope/vim-projectionist")
    -- use({
    --   "akinsho/toggleterm.nvim", -- Terminal improvements
    --   config = require("mattr-.config.toggleterm"),
    -- })
    -- use({
    --   "vim-test/vim-test"
    -- })

    -- -- Git & GitHub
    use("TimUntersberger/neogit")
    use("rhysd/committia.vim") -- Change the formatting and layout of the commit window
    use({
      "lewis6991/gitsigns.nvim", -- Asynchronous Signs!
      config = require("mattr-.config.gitsigns")
    })
    use({
      "mattr-/gitlinker.nvim", -- Line aware links to GitHub
      branch = "use-false-for-no-mappings",
      config = function()
        require("gitlinker").setup({
          mappings = false,
        })
      end,
    })
    use("tpope/vim-fugitive")
    use("tpope/vim-rhubarb")

    use({
      "f-person/git-blame.nvim", -- Git blame virtual text a la GitLens
      setup = function()
        vim.g.gitblame_date_format = "%r"
        vim.g.gitblame_ignored_filetypes = {'packer', 'TelescopePrompt', 'NeogitStatus', 'NeogitPopup', 'Trouble', 'gitcommit', 'octo'}
      end,
    })

    if vim.fn.executable("gh") == 1 then
      use({
        "pwntester/octo.nvim", -- GitHub in NeoVim! :tada:
        config = function()
          local octo = require("octo")
          octo.setup()
        end,
      })
    end
  end,
  config = {
    display = {
      open_fn = function()
        return require("packer.util").float({ border = "single" })
      end,
    },
  },
})

if packer_bootstrap then
  packer.sync()
end
