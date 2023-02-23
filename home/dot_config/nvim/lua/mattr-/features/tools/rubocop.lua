local has_rubocop_file = function()
	return (
		vim.fn.filereadable(".rubocop.yml") == 1
		or vim.fn.filereadable(".rubocop_todo.yml") == 1
		or vim.fn.filereadable(".rubocop.yaml") == 1
		or vim.fn.filereadable(".rubocop_todo.yaml") == 1
	)
end

local rubocop_command = function()
	local cmd = nil
	-- If there is a binstub, prefer that
	-- If there is a rubocop yaml file default to bare rubocop
	-- If there is a Gemfile, look for a bundle binstub and use that or use `bundle exec`
	if vim.fn.executable("bin/rubocop") == 1 then
		cmd = { "bin/rubocop" }
	elseif has_rubocop_file() then
		cmd = { "rubocop" }
		if vim.fn.filereadable("Gemfile") == 1 then
			if vim.fn.executable("bin/bundle") == 1 then
				cmd = { "bin/bundle", "exec", "rubocop" }
			else
				cmd = { "bundle", "exec", "rubocop" }
			end
		end
	end

	return cmd
end

return {
	tool = "rubocop",
	definition = {
		available = not not rubocop_command(),
		command = rubocop_command(),
	},
}
