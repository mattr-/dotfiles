return function()
  require("lualine").setup({
    theme = "tokyonight",
    sections = {
      lualine_a = {
        {
          "mode",
          fmt = function(str) return str:sub(1,1) end,
        },
      },
      lualine_c = {
        {
          "filename",
          file_status = true,
          newfile_status = true,
          path = 1,
          symbols = {
            modified = " ",
            readonly = " ",
            newfile = " "
          },
        }
      },
      lualine_x = { "fileformat", "filetype" },
    },
  })
end
