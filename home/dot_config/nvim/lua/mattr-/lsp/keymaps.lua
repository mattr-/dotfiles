local M = {}

---@type PluginLspKeys
M._keys = nil

---@return (LazyKeys|{has?:string})[]
function M.get()
  local Util = require("mattr-.lsp.util")
  if not M._keys then
    ---@class PluginLspKeys
    -- stylua: ignore
    M._keys =  {
      -- { "<leader>cd", vim.diagnostic.open_float, desc = "Line Diagnostics" },
      -- { "<leader>cl", "<cmd>LspInfo<cr>", desc = "Lsp Info" },
      { "gd", "<cmd>Telescope lsp_definitions<cr>", desc = "Goto Definition", has = "definition" },
      { "gr", "<cmd>Telescope lsp_references<cr>", desc = "References" },
      { "gD", vim.lsp.buf.declaration, desc = "Goto Declaration", has = "declaration" },
      { "gI", "<cmd>Telescope lsp_implementations<cr>", desc = "Goto Implementation", has = "implementation" },
      { "gy", "<cmd>Telescope lsp_type_definitions<cr>", desc = "Goto T[y]pe Definition" },
      { "K", vim.lsp.buf.hover, desc = "Hover" },
      { "gK", vim.lsp.buf.signature_help, desc = "Signature Help", has = "signatureHelp" },
      { "<c-k>", vim.lsp.buf.signature_help, mode = "i", desc = "Signature Help", has = "signatureHelp" },
      { "]d", Util.diagnostic_goto("next"), desc = "Next Diagnostic" },
      { "[d", Util.diagnostic_goto("previous"), desc = "Prev Diagnostic" },
      { "]e", Util.diagnostic_goto("next", "ERROR"), desc = "Next Error" },
      { "[e", Util.diagnostic_goto("previous", "ERROR"), desc = "Prev Error" },
      { "]w", Util.diagnostic_goto("next", "WARN"), desc = "Next Warning" },
      { "[w", Util.diagnostic_goto("previous", "WARN"), desc = "Prev Warning" },
      { "<localleader>f", vim.lsp.buf.format, desc = "Format Document", has = "formatting" },
      { "<localleader>f", vim.lsp.buf.format, desc = "Format Range", mode = "v", has = "rangeFormatting" },
      { "<leader>ca", vim.lsp.buf.code_action, desc = "Code Action", mode = { "n", "v" }, has = "codeAction" },
      {
        "<leader>cA",
        function()
          vim.lsp.buf.code_action({
            context = {
              only = {
                "source",
              },
              diagnostics = {},
            },
          })
        end,
        desc = "Source Action",
        has = "codeAction",
      },
      { "<leader>cr", vim.lsp.buf.rename, desc = "Rename", has = "rename" },
    }

    -- TODO: Check out the inc-rename.nvim plugin
    -- if require("mattr-.util.lazy").has("inc-rename.nvim") then
    --   M._keys[#M._keys + 1] = {
    --     "<leader>cr",
    --     function()
    --       local inc_rename = require("inc_rename")
    --       return ":" .. inc_rename.config.cmd_name .. " " .. vim.fn.expand("<cword>")
    --     end,
    --     expr = true,
    --     desc = "Rename",
    --     has = "rename",
    --   }
    -- else
    --   M._keys[#M._keys + 1] = { "<leader>cr", vim.lsp.buf.rename, desc = "Rename", has = "rename" }
    -- end
  end

  return M._keys
end

---@param method string
function M.has(buffer, method)
  method = method:find("/") and method or "textDocument/" .. method
  local clients = vim.lsp.get_clients({ bufnr = buffer })
  for _, client in ipairs(clients) do
    if client.supports_method(method) then
      return true
    end
  end
  return false
end

function M.resolve(buffer)
  local Keys = require("lazy.core.handler.keys")
  local keymaps = {} ---@type table<string,LazyKeys|{has?:string}>

  local function add(keymap)
    local keys = Keys.parse(keymap)
    if keys[2] == false then
      keymaps[keys.id] = nil
    else
      keymaps[keys.id] = keys
    end
  end

  -- add the default keymaps
  for _, keymap in ipairs(M.get()) do
    add(keymap)
  end

  -- check if server config has been attached to the nvim-lspconfig
  -- options and if there are keymaps specific to that server, add them here
  local opts = require("mattr-.util.lazy").opts("nvim-lspconfig")
  local clients = vim.lsp.get_clients({ bufnr = buffer })
  for _, client in ipairs(clients) do
    local maps = opts.servers[client.name] and opts.servers[client.name].keys or {}
    for _, keymap in ipairs(maps) do
      add(keymap)
    end
  end
  return keymaps
end

function M.on_attach(_client, buffer)
  local Keys = require("lazy.core.handler.keys")
  local keymaps = M.resolve(buffer)

  for _, keys in pairs(keymaps) do
    if not keys.has or M.has(buffer, keys.has) then
      local opts = Keys.opts(keys)
      ---@diagnostic disable-next-line: no-unknown
      opts.has = nil
      opts.silent = opts.silent ~= false
      opts.buffer = buffer
      vim.keymap.set(keys.mode or "n", keys[1], keys[2], opts)
    end
  end
end


return M
