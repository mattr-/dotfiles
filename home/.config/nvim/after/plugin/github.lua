require("octo").setup()

-- The mappings that are set up by octo.nvim are buffer local, so we'll add some global customizations below

-- Add a mapping to list issues in the current repo (letting octo.nvim figure that out for us)
vim.api.nvim_set_keymap("n", ",ghil", ":Octo issue list<CR>", { noremap = true, silent = true })

-- Add a mapping to list PRs in the current repo (letting octo.nvim figure that out for us)
vim.api.nvim_set_keymap("n", ",ghpl", ":Octo pr list<CR>", { noremap = true, silent = true })
