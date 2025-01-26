-- Taken from folke/snacks.nvim/lua/snacks/toggle.lua at revision 82ea3e6
-- Modified to remove Snacks notification integration because I want a
-- more compact display

---@class custom.toggle
---@field opts custom.toggle.Opts
---@overload fun(... :custom.toggle.Opts): custom.toggle
local M = setmetatable({}, {
  __call = function(t, ...)
    return t.new(...)
  end,
})

---@class custom.toggle.Config
---@field icon? string|{ enabled: string, disabled: string }
---@field color? string|{ enabled: string, disabled: string }
local defaults = {
  map = Custom.lazy.integrated_map,
  icon = {
    enabled = " ",
    disabled = " ",
  },
  -- colors for enabled/disabled states
  color = {
    enabled = "green",
    disabled = "yellow",
  },
}

---@class custom.toggle.Opts : custom.toggle.Config
---@field name string
---@field get fun():boolean
---@field set fun(state:boolean)

---@param ... custom.toggle.Opts
---@return custom.toggle
function M.new(...)
  local self = setmetatable({}, { __index = M })
  self.opts = Custom.config.get("toggle", defaults, ...)
  return self
end

function M:get()
  local ok, ret = pcall(self.opts.get)
  if not ok then
    Custom.ui.notify({
      "Failed to get state for `" .. self.opts.name .. "`:\n",
      ret --[[@as string]],
    }, { title = self.opts.name, once = true, level = vim.log.levels.ERROR })
    return false
  end
  return ret
end

---@param state boolean
function M:set(state)
  local ok, err = pcall(self.opts.set, state) ---@type boolean, string?
  if not ok then
    Custom.ui.notify({
      "Failed to set state for `" .. self.opts.name .. "`:\n",
      err --[[@as string]],
    }, { title = self.opts.name, once = true, level = vim.log.levels.ERROR })
  end
end

function M:toggle()
  local state = not self:get()
  self:set(state)
  Custom.ui.notify(
    (state and "Enabled" or "Disabled"),
    { title = self.opts.name,
      level = state and vim.log.levels.INFO or vim.log.levels.WARN,
      render = "compact"
    }
  )
end

---@param keys string
---@param opts? vim.keymap.set.Opts | { mode: string|string[]}
function M:map(keys, opts)
  opts = opts or {}
  local mode = opts.mode or "n"
  opts.mode = nil
  opts.desc = opts.desc or ("Toggle " .. self.opts.name)
  self.opts.map(mode, keys, function()
    self:toggle()
  end, opts)
  if pcall(require, "which-key") then
    self:_wk(keys, mode)
  end
end

function M:_wk(keys, mode)
  require("which-key").add({
    {
      keys,
      mode = mode,
      icon = function()
        local key = self:get() and "enabled" or "disabled"
        return {
          icon = type(self.opts.icon) == "string" and self.opts.icon or self.opts.icon[key],
          color = type(self.opts.color) == "string" and self.opts.color or self.opts.color[key],
        }
      end,
      desc = function()
        return (self:get() and "Disable " or "Enable ") .. self.opts.name
      end,
    },
  })
end

---@param option string
---@param opts? custom.toggle.Config | {on?: unknown, off?: unknown}
function M.option(option, opts)
  opts = opts or {}
  local on = opts.on == nil and true or opts.on
  local off = opts.off ~= nil and opts.off or false
  return M.new({
    name = option,
    get = function()
      return vim.opt_local[option]:get() == on
    end,
    set = function(state)
      vim.opt_local[option] = state and on or off
    end,
  }, opts)
end

return M
