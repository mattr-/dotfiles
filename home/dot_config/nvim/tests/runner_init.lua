#!/usr/bin/env -S nvim -l

vim.env.LAZY_STDPATH = vim.fn.stdpath("cache") .. "/.config_tests"
load(vim.fn.system("curl -s https://raw.githubusercontent.com/folke/lazy.nvim/main/bootstrap.lua"))()

-- Setup lazy.nvim
require("lazy.minit").setup({
  spec = {
    { dir = vim.uv.cwd() },
  },
})
