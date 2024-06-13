local LazyUtil = require("lazy.core.util")

--- @class dashvim.util: LazyUtilCore
--- @field config DashVimConfig
local M = {}

setmetatable(M, {
  __index = function(table, key)
    -- Delegate to lazy's core utilities if a key matches a name in that class
    if LazyUtil[key] then
      return LazyUtil[key]
    end

    -- Otherwise, load a module from our util namespace
    table[key] = require("mattr-.util." .. key)
    return table[key]
  end,
})

-- delay notifications till vim.notify was replaced or after 500ms
function M.lazy_notify()
  local notifs = {}
  local function temp(...)
    table.insert(notifs, vim.F.pack_len(...))
  end

  local orig = vim.notify
  vim.notify = temp

  local timer = vim.uv.new_timer()
  local check = vim.uv.new_check()

  local replay = function()
    timer:stop()
    check:stop()
    if vim.notify == temp then
      vim.notify = orig -- put back the original notify if needed
    end
    vim.schedule(function()
      ---@diagnostic disable-next-line: no-unknown
      for _, notif in ipairs(notifs) do
        vim.notify(vim.F.unpack_len(notif))
      end
    end)
  end

  -- wait till vim.notify has been replaced
  check:start(function()
    if vim.notify ~= temp then
      replay()
    end
  end)
  -- or if it took more than 500ms, then something went wrong
  timer:start(500, 0, replay)
end

return M
