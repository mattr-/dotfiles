local mappings = require("mattr-.utils.mappings")
local opts = { silent = true }

mappings.nmap(
  "<leader>:",
  "<cmd>Telescope command_history<CR>",
  opts,
  "Editor",
  "command_history",
  "Command history"
)

mappings.nmap(
  "<leader>/",
  "<cmd>Telescope live_grep<CR>",
  opts,
  "Editor",
  "live_grep",
  "Interactive Grep"
)

-- {{{ File group mappings
mappings.nmap(
  "<leader>ff",
  '<cmd>lua require("mattr-.telescope").find_files()<CR>',
  opts,
  "File",
  "find_files",
  "Find files"
)
-- }}}

-- {{{ Git group mappings
mappings.nmap(
  "<leader>gl",
  '<cmd>lua require("gitlinker").get_buf_range_url("n", {add_current_line_on_normal_mode = false})<CR>',
  opts,
  "Git",
  "gitlinker_normal",
  "Link to current file"
)
mappings.vmap(
  "<leader>gl",
  '<cmd>lua require("gitlinker").get_buf_range_url("v")<CR>',
  opts,
  "Git",
  "gitlinker_visual",
  "Link to current file"
)
mappings.nmap(
  "<leader>gs",
  "<cmd>Neogit<CR>",
  opts,
  "Git",
  "git_status",
  "Status"
)
-- }}}

-- {{{ Help group mappings
mappings.nmap(
  "<leader>ht",
 "<cmd>:Telescope builtin<cr>",
 opts,
 "Help",
 "help_telescope",
 "Telescope"
)
mappings.nmap(
  "<leader>hc",
 "<cmd>:Telescope commands<cr>",
 opts,
 "Help",
 "help_commands",
 "Commands"
)
mappings.nmap(
  "<leader>hh",
 "<cmd>:Telescope help_tags<cr>",
 opts,
 "Help",
 "help_helppages",
 "Help Pages"
)
mappings.nmap(
  "<leader>hm",
 "<cmd>:Telescope man_pages<cr>",
 opts,
 "Help",
 "help_manpages",
 "Man Pages"
)
mappings.nmap(
  "<leader>hk",
 "<cmd>:Telescope keymaps<cr>",
 opts,
 "Help",
 "help_keymaps",
 "Key Maps"
)
mappings.nmap(
  "<leader>hs",
 "<cmd>:Telescope highlights<cr>",
 opts,
 "Help",
 "help_highlightgroups",
 "Search Highlight Groups"
)
mappings.nmap(
  "<leader>hl",
 [[<cmd>TSHighlightCapturesUnderCursor<cr>]],
 opts,
 "Help",
 "help_groups_under_cursor",
 "Highlight Groups at cursor"
)
mappings.nmap(
  "<leader>hf",
 "<cmd>:Telescope filetypes<cr>",
 opts,
 "Help",
 "help_filetypes",
 "File Types"
)
mappings.nmap(
  "<leader>ho",
 "<cmd>:Telescope vim_options<cr>",
 opts,
 "Help",
 "help_options",
 "Options"
)
mappings.nmap(
  "<leader>ha",
 "<cmd>:Telescope autocommands<cr>",
 opts,
 "Help",
 "help_autocommands",
 "Auto Commands"
)
-- {{{ Packer mappings
mappings.nmap(
  "<leader>hpc",
  "<cmd>PackerCompile<CR>",
  opts,
  "Plugins",
  "plugins_compile",
  "Compile"
)
mappings.nmap(
  "<leader>hpi",
  "<cmd>PackerInstall<CR>",
  opts,
  "Plugins",
  "plugins_install",
  "Install"
)
mappings.nmap(
  "<leader>hps",
  "<cmd>PackerStatus<CR>",
  opts,
  "Plugins",
  "plugins_status",
  "Status"
)
mappings.nmap(
  "<leader>hpp",
  "<cmd>PackerSync<CR>",
  opts,
  "Plugins",
  "plugins_sync",
  "Sync"
)
-- }}}
-- }}}

-- {{{ Open group mappings
mappings.nmap("<leader>ta",
  "<cmd>:set nu!<CR>",
  opts,
  "Toggles",
  "toggle_absolute_line_numbering",
  "Absolute Line Numbering"
)
mappings.nmap("<leader>tr",
  "<cmd>:set rnu!<CR>",
  opts,
  "Toggles",
  "toggle_relative_line_numbering",
  "Relative Line Numbering"
)
mappings.nmap("<leader>tt",
  '<cmd>execute v:count . "ToggleTerm"<CR>',
  opts,
  "Toggles",
  "toggle_terminal",
  "Terminal"
)
-- }}}

-- {{{ Window group mappings
--
mappings.nmap("<leader>ww",
  "<C-W>w",
  opts,
  "Window",
  "other_window",
  "Goto other window"
)
mappings.nmap("<leader>w=",
  "<C-W>=",
  opts,
  "Window",
  "same_size_window",
  "Make all windows the same size"
)

-- }}}

-- vim: fdm=marker
