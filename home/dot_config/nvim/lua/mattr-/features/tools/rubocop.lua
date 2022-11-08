local has_rubocop_file = function()
  return (
    vim.fn.filereadable(".rubocop.yml")
    or vim.fn.filereadable(".rubocop_todo.yml")
    or vim.fn.filereadable(".rubocop.yaml")
    or vim.fn.filereadable(".rubocop_todo.yaml")
  )
end

local rubocop_command = function()
  local cmd = nil
  -- If there is a binstub, prefer that
  -- If there is a rubocop yaml file default to bare rubocop
  -- If there is a Gemfile, look for a bundle binstub and use that or use `bundle exec`
  if vim.fn.executable("bin/rubocop") then
    cmd = "bin/rubocop"
  elseif has_rubocop_file() then
    cmd = "rubocop"
    if vim.fn.filereadable("Gemfile") then
      local bundler_prefix = "bundle exec"
      if vim.fn.executable("bin/bundle") then
        bundler_prefix = "bin/" .. bundler_prefix
      end
      cmd = bundler_prefix .. cmd
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
