return {
  {
    "nvim-treesitter/nvim-treesitter",
    opts = {
      ensure_installed = {
        "diff",
        "markdown"
      },
    },
  },
  {
    "CopilotC-Nvim/CopilotChat.nvim",
    lazy = false,
    dependencies = {
      { "zbirenbaum/copilot.lua" },
    },
    build = "make tiktoken", -- Only on MacOS or Linux
    opts = function()
      local user = vim.env.USER or "User"
      return {
        auto_insert_mode = true,
        question_header = "  " .. user .. " ",
        answer_header = "  Copilot ",
        window = {
          width = 0.5,
        },
      }
      -- See Configuration section for options
    end,
    -- See Commands section for default commands if you want to lazy load on them
  },
}
