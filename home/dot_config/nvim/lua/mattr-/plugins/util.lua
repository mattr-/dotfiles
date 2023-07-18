return {

    -- Repeating plugin maps
    {
      "tpope/vim-repeat",
      event = "VeryLazy"
    },

    -- Abbreviate, substitution, and coercion
    {
      "tpope/vim-abolish",
      event = { "BufReadPost", "BufNewFile" }
    },

    -- Comment all the things!
    {
      "tpope/vim-commentary",
      event = { "BufReadPost", "BufNewFile" }
    },

    {
      "tpope/vim-dispatch",
      cmd = { "Dispatch", "Make" },
    },
}
