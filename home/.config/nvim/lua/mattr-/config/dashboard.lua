return function()
  vim.g.dashboard_default_executive = "telescope"
  vim.g.dashboard_preview_pipeline = "lolcat"
  vim.g.dashboard_enable_session = 0
  vim.g.dashboard_disable_statusline = 0

  vim.g.dashboard_custom_header = {
    " ███╗   ██╗ ███████╗ ██████╗  ██╗   ██╗ ██╗ ███╗   ███╗",
    " ████╗  ██║ ██╔════╝██╔═══██╗ ██║   ██║ ██║ ████╗ ████║",
    " ██╔██╗ ██║ █████╗  ██║   ██║ ██║   ██║ ██║ ██╔████╔██║",
    " ██║╚██╗██║ ██╔══╝  ██║   ██║ ╚██╗ ██╔╝ ██║ ██║╚██╔╝██║",
    " ██║ ╚████║ ███████╗╚██████╔╝  ╚████╔╝  ██║ ██║ ╚═╝ ██║",
    " ╚═╝  ╚═══╝ ╚══════╝ ╚═════╝    ╚═══╝   ╚═╝ ╚═╝     ╚═╝",
  }

  vim.g.dashboard_custom_footer = {
    "Neovim loaded in " .. vim.fn.printf(
      "%.3f",
      vim.fn.reltimefloat(vim.fn.reltime(vim.g.start_time))
    ) .. " seconds.",
  }
end
