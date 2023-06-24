return {
  {
    "tpope/vim-dispatch",
    cmd = { "Dispatch", "Make" },
  },

  {
    "lvimuser/lsp-inlayhints.nvim",
    event = "LspAttach",
    init = function()
      require("lazyvim.util").on_attach(function(client, buffer)
        require("lsp-inlayhints").on_attach(client, buffer)
      end)
    end,
  },
}
