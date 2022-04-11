packer_bootstrap = false

local packer_path = string.format("%s/site/pack/packer/opt/packer.nvim", vim.fn.stdpath("data"))
if vim.fn.empty(vim.fn.glob(packer_path)) > 0 then

  packer_bootstrap = true
  print("Bootstrapping packer. Please wait...")
  local packer_path = string.format("%s/site/pack/packer/opt/packer.nvim", vim.fn.stdpath("data"))
  vim.fn.mkdir(packer_path, "p")
  vim.fn.system({
    "git",
    "clone",
    "https://github.com/wbthomason/packer.nvim",
    packer_path})
end

vim.cmd [[packadd packer.nvim]]

-- Autocommand that reloads neovim whenever you save the plugins.lua file
vim.cmd [[
  augroup packer_user_config
    autocmd!
    autocmd BufWritePost plugins.lua source <afile> | PackerSync
  augroup end
]]

local packer = require("packer")

packer.startup({
  function(use)
    local local_use = function(first, second, opts)
      opts = opts or {}
      local plugin_path, home
      if second == nil then
        plugin_path = first
        home = "mattr-"
      else
        plugin_path = second
        home = first
      end


      local final_path = string.format("%s/%s", home, plugin_path)
      if vim.fn.isdirectory(vim.fn.expand("~/Code/" .. final_path)) == 1 then
        opts[1] = "~/Code/" .. final_path
      else
        opts[1] = final_path
      end

      use(opts)
    end

    use({
      "wbthomason/packer.nvim",
      opt = true
    })
    use "lewis6991/impatient.nvim"
    local_use "plenary.nvim"

    -- UI and Colors
    use "norcalli/nvim-colorizer.lua"
    use "kyazdani42/nvim-web-devicons"

    use "rcarriga/nvim-notify" -- notifications in vim. let's try it!
    use "folke/trouble.nvim" -- a problem list plugin
    use "NTBBloodbath/galaxyline.nvim" -- Fancy statusline
    use "kevinhwang91/nvim-hlslens" -- interesting search match information

    -- Tree Sitter
    use {
      "nvim-treesitter/nvim-treesitter",
      run = ':TSUpdate'
    }
    use "nvim-treesitter/playground"
    use "JoosepAlviste/nvim-ts-context-commentstring" -- Context aware comment strings

    -- LSP
    use "neovim/nvim-lspconfig"
    use "williamboman/nvim-lsp-installer"

    -- Completion and Snippets
    use 'hrsh7th/nvim-cmp'         -- Autocompletion plugin
    use 'hrsh7th/cmp-buffer'       -- Completion from other buffers
    use 'hrsh7th/cmp-path'         -- Completion for filesystem paths
    use 'hrsh7th/cmp-cmdline'      -- Completion for vim's cmdline
    use 'hrsh7th/cmp-nvim-lsp'     -- Completion from LSP servers
    use 'hrsh7th/cmp-nvim-lua'     -- Completion for neovim's API
    use 'L3MON4D3/LuaSnip'         -- Snippets plugin
    use 'saadparwaiz1/cmp_luasnip' -- Completion from LuaSnip snippets

    -- Utilities
    use "tpope/vim-surround"      -- TODO: Look at https://github.com/machakann/vim-sandwich at some point
    use "tpope/vim-repeat"        -- Repeating plugin maps
    use "tpope/vim-abolish"       -- Abbreviate, substitution, and coercion
    use "tpope/vim-commentary"    -- Comment all the things!
    use "junegunn/vim-easy-align" -- Alignment stuff
    use {
      "tpope/vim-dispatch",
      cmd = { "Dispatch", "Make" }
    }
    use "tpope/vim-projectionist"
    use "akinsho/toggleterm.nvim" -- Terminal improvements

    -- Git & GitHub
    use "TimUntersberger/neogit"
    use "rhysd/committia.vim"     -- Change the formatting and layout of the commit windo
    use "lewis6991/gitsigns.nvim" -- Asynchronous Signs!
    use "ruifm/gitlinker.nvim"    -- Line aware links to GitHub
    use "f-person/git-blame.nvim" -- Git blame virtual text a la GitLens

    if vim.fn.executable "gh" == 1 then
      use "pwntester/octo.nvim"   -- GitHub in NeoVim! :tada:
    end

    -- Fuzzy Finding
    use "nvim-telescope/telescope.nvim"
    use {
      "nvim-telescope/telescope-fzf-native.nvim",
      run = 'make'
    }
    -- Use telescope as a backend for vim.ui.select (NeoVim 0.6)
    use { "nvim-telescope/telescope-ui-select.nvim" }

    -- Language specific additions for LSP
    use "b0o/schemastore.nvim" -- Schemas for jsonls

    -- Non LSP editing support. Markdown in particular
    use {
      "iamcco/markdown-preview.nvim",
      run = "cd app && yarn install",
    }

  end,
  config = {
    display = {
      open_fn = function()
        return require('packer.util').float { border = "rounded" }
      end,
    }
  }
})

if packer_bootstrap then
    packer.sync()
end
