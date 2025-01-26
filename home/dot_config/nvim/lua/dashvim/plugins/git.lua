return {
  -- Treesitter git support
  {
    "nvim-treesitter/nvim-treesitter",
    opts = { ensure_installed = { "git_config", "gitcommit", "git_rebase", "gitignore", "gitattributes" } },
  },

  -- Give ourselves a nicer commit window when commiting from the command line
  {
    "rhysd/committia.vim",
    lazy = false
  },
}
