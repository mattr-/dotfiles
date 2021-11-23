_ = vim.cmd [[packadd packer.nvim]]

return require("packer").startup({
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


      final_path = string.format("%s/%s", home, plugin_path)
      if vim.fn.isdirectory(vim.fn.expand("~/Code/" .. final_path)) == 1 then
        opts[1] = "~/Code/" .. final_path
      else
        opts[1] = final_path
      end

      use(opts)
    end

    use "wbthomason/packer.nvim"
    use "lewis6991/impatient.nvim"

    -- UI and Colors
    local_use "hackthebox.vim"
    use "norcalli/nvim-colorizer.lua"
    use "kyazdani42/nvim-web-devicons"

    use "rcarriga/nvim-notify" -- notifications in vim. let's try it!
    use "folke/trouble.nvim" -- a problem list plugin
    use "tjdevries/cyclist.vim" -- Multiple sets of listchars
    use "NTBBloodbath/galaxyline.nvim" -- Fancy statusline

    -- Tree Sitter
    use {
      "nvim-treesitter/nvim-treesitter",
      run = ':TSUpdate'
    }
    use "nvim-treesitter/playground"

    -- LSP
    use "neovim/nvim-lspconfig"

    -- Completion
    use { "ms-jpq/coq_nvim", branch = 'coq' }
    use { "ms-jpq/coq.artifacts", branch = 'artifacts' }
    use { "ms-jpq/coq.thirdparty", branch = '3p' }

    use 'hrsh7th/nvim-cmp' -- Autocompletion plugin
    use 'hrsh7th/cmp-nvim-lsp'
    use 'saadparwaiz1/cmp_luasnip'
    use 'L3MON4D3/LuaSnip' -- Snippets plugin

    -- Utilities
    use "tpope/vim-surround" -- TODO: Look at https://github.com/machakann/vim-sandwich at some point
    use "tpope/vim-repeat" -- Repeating plugin maps
    use "tpope/vim-abolish" -- Abbreviate, substitution, and coercion
    use "junegunn/vim-easy-align" -- Alignment stuff
    use {
      "tpope/vim-dispatch",
      cmd = { "Dispatch", "Make" }
    }

    -- Git & GitHub

    use "TimUntersberger/neogit"
    use "rhysd/committia.vim" -- Change the formatting and layout of the commit windo
    use "lewis6991/gitsigns.nvim" -- Asynchronous Signs!
    use "ruifm/gitlinker.nvim" -- Line aware links to GitHub

    if vim.fn.executable "gh" == 1 then
      use "pwntester/octo.nvim" -- GitHub in NeoVim! :tada:
    end

    -- Fuzzy Finding
    use {
      "nvim-telescope/telescope.nvim",
      requires = { {'nvim-lua/plenary.nvim'} },
    }
    use {
      "nvim-telescope/telescope-fzf-native.nvim",
      run = 'make'
    }

  end,
  config = {
    display = {
      open_fn = require('packer.util').float,
    }
  }
})

-- vim: et sts=2 sw=2 foldmethod=marker