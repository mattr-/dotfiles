return {
  settings = {
    Lua = {
      diagnostics = {
        globals = { "vim", "packer_plugins" },
      },
      workspace = {
        library = {
          [vim.fn.expand("$VIMRUNTIME/lua")] = true,
          [vim.fn.stdpath("config") .. "/lua"] = true,
        },
      },
    },
  },
}
