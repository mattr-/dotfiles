return {
  {
    "pwntester/octo.nvim",
    event = "VeryLazy",
    config = function(_, opts)
      require("octo").setup(opts)
    end,
  },
}
