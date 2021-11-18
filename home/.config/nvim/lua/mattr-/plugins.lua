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

  end,
  config = {
    display = {
      open_fn = require('packer.util').float,
    }
  }
})

-- vim: et sts=2 sw=2 foldmethod=marker
