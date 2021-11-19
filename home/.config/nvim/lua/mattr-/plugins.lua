_ = vim.cmd [[packadd packer.nvim]]

return require("packer").startup({
  function(use)
    use "wbthomason/packer.nvim"
    use "lewis6991/impatient.nvim"

    -- UI and Colors
    use {
      "audibleblink/hackthebox.vim",
      config = function()
        vim.opt.termguicolors = true
        vim.cmd [[colorscheme hackthebox]]
      end
    }

    use "kyazdani42/nvim-web-devicons"

    -- Tree Sitter
    use {
      "nvim-treesitter/nvim-treesitter",
      run = ':TSUpdate'
    }

    -- Utilities
    use "tpope/vim-surround" -- TODO: Look at https://github.com/machakann/vim-sandwich at some point
    use "tpope/vim-repeat" -- Repeating plugin maps
    use "tpope/vim-abolish" -- Abbreviate, substitution, and coercion
    use {
      "tpope/vim-dispatch",
      cmd = { "Dispatch", "Make" }
    }

    -- Git & GitHub

    use "TimUntersberger/neogit"
    use "rhysd/committia.vim" -- Change the formatting and layout of the commit windo
    use "lewis6991/gitsigns.nvim" -- Asynchronous Signs!

    if vim.fn.executable "gh" == 1 then
      use "pwntester/octo.nvim"
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
