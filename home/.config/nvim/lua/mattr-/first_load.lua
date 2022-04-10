-- Bootstrap packer.nvim
-- Inspired by xdg_config/nvim/lua/tj/first_load.lua in tjdevries/config_manager on GitHub
local download_packer = function()
  print("Packer wasn't found. Downloading...")
  
  local directory = string.format("%s/site/pack/packer/start/", vim.fn.stdpath "data")

  vim.fn.mkdir(directory, "p")

  local out = vim.fn.system(
    string.format("git clone %s %s", "https://github.com/wbthomason/packer.nvim", directory .. "/packer.nvim")
  )

  print(out)
end

return function()
  if not pcall(require, "packer") then
    download_packer()
    return true
  end

  return false
end
