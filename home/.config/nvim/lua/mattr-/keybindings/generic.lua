local mappings = require("mattr-.utils.mappings")

mappings.map(
  "n",
  "<CR>",
  "<cmd>nohlsearch<CR>",
  { silent = true, noremap = true},
  "Generic",
  "disable_search_highlight",
  "Disable search highlighting"
)

--{{{ System clipboard niceties (from Mastering Vim Quickly)
mappings.map(
  "n",
  ",y",
  '"+y',
  {},
  "Generic",
  "system_clipboard_yank",
  "Yank to the system clipboard"
)
mappings.map(
  "n",
  ",d",
  '"+d',
  {},
  "Generic",
  "system_clipboard_delete",
  "Delete to the system clipboard"
)
mappings.map(
  "n",
  ",p",
  '"+p',
  {},
  "Generic",
  "system_clipboard_paste_after",
  "Paste from the system clipboard"
)
mappings.map(
  "n",
  ",P",
  '"+P',
  {},
  "Generic",
  "system_clipboard_paste_before",
  "Paste (before) from the system clipboard"
)
--}}}
-- vim: fdm=marker
