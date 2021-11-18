_ = vim.cmd [[packadd packer.nvim]]

return require("packer").startup({
  function(use)
    use "wbthomason/packer.nvim"
    use "lewis6991/impatient.nvim"

    -- {{{ UI and Colors
    use {
      "audibleblink/hackthebox.vim",
      config = function()
        vim.opt.termguicolors = true
        vim.cmd [[colorscheme hackthebox]]
      end
    }
    --- }}}

    -- Fuzzy Finding
    use({
      "nvim-telescope/telescope.nvim",
      requires = { {'nvim-lua/plenary.nvim'} },
    })
    use({
      "nvim-telescope/telescope-fzf-native.nvim",
      run = 'make'
    })

  end,
  config = {
    display = {
      open_fn = require('packer.util').float,
    }
  }
})

-- vim: et sts=2 sw=2 foldmethod=marker
