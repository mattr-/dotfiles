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

    use({
      "folke/which-key.nvim",
      config = require("mattr-.config.which-key"),
    })

    -- Use my fork of plenary.nvim until nvim-lua/plenary.nvim#290 gets merged
    use({
      "mattr-/plenary.nvim",
      branch = "fix-dashes-at-end-of-usernames",
    })

    -- UI and Colors
    use("norcalli/nvim-colorizer.lua")
    use("kyazdani42/nvim-web-devicons")
    use({
      "glepnir/dashboard-nvim",
      config = require("mattr-.config.dashboard")
    })

    -- notifications in vim. let's try it!
    use({
      "rcarriga/nvim-notify",
      event = VimEnter,
      config = require("mattr-.config.notify"),
    })
    use("folke/trouble.nvim") -- a problem list plugin
    use({
      "NTBBloodbath/galaxyline.nvim", -- Fancy statusline
      config = require("mattr-.config.galaxyline"),
    })

    -- Tree Sitter
    use({
      "nvim-treesitter/nvim-treesitter",
      run = ":TSUpdate",
      config = require("mattr-.config.treesitter"),
    })
    use("nvim-treesitter/playground")
    use("JoosepAlviste/nvim-ts-context-commentstring") -- Context aware comment strings

    -- LSP
    use("neovim/nvim-lspconfig")
    use("williamboman/nvim-lsp-installer")

    -- Completion and Snippets
    use({
      "hrsh7th/nvim-cmp", -- Autocompletion plugin
      config = require("mattr-.config.nvim-cmp"),
      event = "InsertEnter",
      wants = { "LuaSnip" },
      requires = {
        "L3MON4D3/LuaSnip", -- Snippets plugin
        config = require("mattr-.config.luasnip"),
        event = "BufReadPre"
      }
    })
    use({"hrsh7th/cmp-buffer", -- Completion from other buffers
      after = "nvim-cmp",
    })
    use({"hrsh7th/cmp-path", -- Completion for filesystem paths
      after = "nvim-cmp",
    })
    use({"hrsh7th/cmp-nvim-lsp", -- Completion from LSP servers
      after = "nvim-cmp",
    })
    use({"hrsh7th/cmp-nvim-lua", -- Completion for neovim's API
      after = "nvim-cmp",
    })
    use({
      "saadparwaiz1/cmp_luasnip", -- Completion from LuaSnip snippets
      after = "nvim-cmp",
    })

    -- Utilities
    use("tpope/vim-surround") -- TODO: Look at https://github.com/machakann/vim-sandwich at some point
    use("tpope/vim-repeat") -- Repeating plugin maps
    use("tpope/vim-abolish") -- Abbreviate, substitution, and coercion
    use("tpope/vim-commentary") -- Comment all the things!
    use({
      "tpope/vim-dispatch",
      cmd = { "Dispatch", "Make" },
    })
    use("tpope/vim-projectionist")
    use({
      "akinsho/toggleterm.nvim", -- Terminal improvements
      config = require("mattr-.config.toggleterm"),
    })

    -- Git & GitHub
    use("TimUntersberger/neogit")
    use("rhysd/committia.vim") -- Change the formatting and layout of the commit windo
    use("lewis6991/gitsigns.nvim") -- Asynchronous Signs!
    use({
      "mattr-/gitlinker.nvim", -- Line aware links to GitHub
      branch = "use-false-for-no-mappings",
      config = function()
        require("gitlinker").setup({
          mappings = false,
        })
      end,
    })

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

          -- The mappings that are set up by octo.nvim are buffer local, so we'll add some global customizations below

          -- Add a mapping to list issues in the current repo (letting octo.nvim figure that out for us)
          vim.api.nvim_set_keymap("n", ",ghil", ":Octo issue list<CR>", { noremap = true, silent = true })

          -- Add a mapping to list PRs in the current repo (letting octo.nvim figure that out for us)
          vim.api.nvim_set_keymap("n", ",ghpl", ":Octo pr list<CR>", { noremap = true, silent = true })
        end,
      })
    end

    -- Fuzzy Finding
    use({
      "nvim-telescope/telescope.nvim", -- Fuzzy finding
      opt = true,
      cmd = "Telescope",
      module = "telescope",
      config = require("mattr-.config.telescope"),
      wants = {
        "plenary.nvim",
        "telescope-fzf-native.nvim",
        "telescope-ui-select.nvim",
      },
      requires = {
        {
          "nvim-telescope/telescope-fzf-native.nvim", --fzf based searching
          run = "make",
        },
        {
          "nvim-telescope/telescope-ui-select.nvim", -- Use telescope as a backend for vim.ui.select (NeoVim 0.6)
        }
      }
    })

    -- Language specific additions for LSP
    use("b0o/schemastore.nvim") -- Schemas for jsonls

    -- Non LSP editing support. Markdown in particular
    use({
      "iamcco/markdown-preview.nvim",
      run = "cd app && yarn install",
    })
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
