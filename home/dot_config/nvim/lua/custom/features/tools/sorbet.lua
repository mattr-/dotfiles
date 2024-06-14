local has_sorbet_config = function()
  return vim.fn.filereadable("sorbet/config") == 1
end

local sorbet_command = function()
  local cmd = nil
  -- If there is a binstub, prefer that
  -- If there is a sorbet config file then use plain `srb`
  -- If there is a Gemfile, look for a bundle binstub and use that or use `bundle exec`
  if vim.fn.executable("bin/srb") == 1 then
    cmd = { "bin/srb" }
  elseif has_sorbet_config() then
    cmd = { "srb" }
    if vim.fn.filereadable("Gemfile") == 1 then
      if vim.fn.executable("bin/bundle") == 1 then
        cmd = { "bin/bundle", "exec", "srb" }
      else
        cmd = { "bundle", "exec", "srb" }
      end
    end
  end

  -- append the necessary options to the command table for sorbet
  if type(cmd) == "table" then
    table.insert(cmd, "tc")
    table.insert(cmd, "--lsp")
  end

  return cmd
end

return {
  tool = "sorbet",
  definition = {
    available = function() return not not sorbet_command() end,
    command = function() return sorbet_command() end,
  },
}
