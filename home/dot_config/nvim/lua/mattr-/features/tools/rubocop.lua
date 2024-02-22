local has_rubocop_file = function()
  return (
    vim.fn.filereadable(".rubocop.yml") == 1
    or vim.fn.filereadable(".rubocop_todo.yml") == 1
    or vim.fn.filereadable(".rubocop.yaml") == 1
    or vim.fn.filereadable(".rubocop_todo.yaml") == 1
  )
end

local rubocop_in_gemfile = function()
  if vim.fn.filereadable("Gemfile") then
    local file = io.open("Gemfile")
    if file then
      for line in file:lines() do
        if line:match("rubocop") then
          file:close()
          return true
        end
      end
    end
  end
end

local rubocop_command = function()
  local cmd = nil
  -- Do nothing if there is not a rubocop file
  if has_rubocop_file() then
    -- If there is a binstub, prefer that
    -- If there is a Gemfile, ensure rubocop is in the gemfile.
    -- Then look for a bundle binstub and use that or use `bundle exec`
    if vim.fn.executable("bin/rubocop") == 1 then
      cmd = { "bin/rubocop" }
    elseif rubocop_in_gemfile() then
      if vim.fn.executable("bin/bundle") == 1 then
        cmd = { "bin/bundle", "exec", "rubocop" }
      else
        cmd = { "bundle", "exec", "rubocop" }
      end
    else
      cmd = { "rubocop" }
    end
  end

  return cmd
end

return {
  tool = "rubocop",
  definition = {
    available = function() return not not rubocop_command() end,
    command = function() return rubocop_command() end,
  },
}
