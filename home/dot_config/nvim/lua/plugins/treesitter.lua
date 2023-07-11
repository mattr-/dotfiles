return {
  {
    "nvim-treesitter/nvim-treesitter",
    version = false,
    build = ":TSUpdate",
    event = { "BufReadPost", "BufNewFile" },
    cmd = { "TSUpdateSync" },
    -- keys = {
    --   {},
    --   {},
    -- },
    opts = {
      highlight = {
        enable = true,
        additional_vim_regex_highlighting = false
      },
      indent = {
        enable = true,
      },
      -- install a base set of tree sitter grammars and have language specs do the rest
      ensure_installed = {
        "bash",
        "lua",
        "luadoc",
        "luap",
        "regex",
        "vim",
        "vimdoc",
        "yaml",
      },
      incremental_selection = {
        enable = true
      },
      move = {
        enable = true,
        set_jumps = true, -- whether to set jumps in the jumplist
        goto_next_start = {
          [']m'] = '@function.outer',
          [']]'] = '@class.outer',
        },
        goto_next_end = {
          [']M'] = '@function.outer',
          [']['] = '@class.outer',
        },
        goto_previous_start = {
          ['[m'] = '@function.outer',
          ['[['] = '@class.outer',
        },
        goto_previous_end = {
          ['[M'] = '@function.outer',
          ['[]'] = '@class.outer',
        },
      },
    },
  },
}
