local status_ok, gl = pcall(require, "galaxyline")
if not status_ok then
  return
end
local condition = require("galaxyline.condition")
local buffer = require("galaxyline.providers.buffer")
local fileinfo = require("galaxyline.providers.fileinfo")
local List = require("plenary.collections.py_list")
local gls = gl.section
local api = vim.api
local M = {}

M.modes = setmetatable({
  ["n"]  = {["short"] = "N", ["long"] = "NORMAL" },
  ["no"] = {["short"] = "N·P", ["long"] = "" },
  ["v"]  = {["short"] = "V", ["long"] = "VISUAL" },
  ["V"]  = {["short"] = "V·L", ["long"] = "VISUAL LINE" },
  [""] = {["short"] = "V·B", ["long"] = "VISUAL BLOCK" },
  ["s"]  = {["short"] = "S", ["long"] = "SELECT" },
  ["S"]  = {["short"] = "S·L", ["long"] = "SELECT LINE" },
  [""] = {["short"] = "S·B", ["long"] = "SELECT BLOCK" },
  ["i"]  = {["short"] = "I", ["long"] = "INSERT" },
  ["ic"] = {["short"] = "I", ["long"] = "INSERT NORMAL" },
  ["R"]  = {["short"] = "R", ["long"] = "REPLACE" },
  ["Rv"] = {["short"] = "V·R", ["long"] = "VIRTUAL REPLACE" },
  ["c"]  = {["short"] = "C", ["long"] = "COMMAND" },
  ["cv"] = {["short"] = "V·E", ["long"] = "" },
  ["ce"] = {["short"] = "E", ["long"] = "" },
  ["r"]  = {["short"] = "P", ["long"] = "" },
  ["rm"] = {["short"] = "M", ["long"] = "" },
  ["r?"] = {["short"] = "C", ["long"] = "COMMAND" },
  ["!"]  = {["short"] = "S", ["long"] = "SHELL" },
  ["t"]  = {["short"] = "T", ["long"] = "TERMINAL" },
}, {
  __index = function()
    return {["short"] = "U", ["long"] = "UNKNOWN"} -- handle edge cases
  end,
})

M.get_current_mode = function(self)
  local current_mode = api.nvim_get_mode().mode
  return self.modes[current_mode]
end

-- Duplicated from NTBBloodbath/galaxyline
M.file_readonly = function()
  if vim.bo.filetype == "help" then
    return true
  end

  return vim.bo.readonly
end

M.file_with_icons = function(file, modified_icon, readonly_icon)
  if vim.fn.empty(file) == 1 then
    return ""
  end

  modified_icon = modified_icon or ""
  readonly_icon = readonly_icon or ""

  if M.file_readonly() then
    file = readonly_icon .. " " .. file
  end

  if vim.bo.modifiable and vim.bo.modified then
    file = file .. " " .. modified_icon
  end

  return " " .. file .. " "
end

gl.short_line_list = { "LuaTree", "vista", "dbui" }

local colors = {
  bg = "#3e4452",
  yellow = "#ffcc5c",
  cyan = "#5cecc6",
  darkblue = "#141d2b",
  green = "#c5f467",
  orange = "#ffaf00",
  purple = "#c16cfa",
  magenta = "#d16d9e",
  grey = "#c0c0c0",
  text = "#A4B1CD",
  blue = "#5cb2ff",
  red = "#ff8484",
  white = "#ffffff",
}

gls.left[1] = {
  FirstElement = {
    provider = function()
      return "▋"
    end,
    highlight = { colors.bg, colors.bg },
  },
}

local modeFunction = function()
  mode = M:get_current_mode()
  if condition.buffer_not_empty() then
    return string.format("%s ", mode["short"])
  else
    return string.format("%s ", mode["long"])
  end
end

gls.left[2] = {
  ViMode = {
    provider = modeFunction,
    separator = " ",
    separator_highlight = {
      colors.text,
      colors.bg,
    },
    highlight = { colors.text, colors.bg, "bold" },
  },
}

gls.left[3] = {
  GitIcon = {
    provider = function()
      return " "
    end,
    condition = condition.buffer_not_empty,
    highlight = { colors.white, colors.bg },
  },
}
gls.left[4] = {
  GitBranch = {
    provider = "GitBranch",
    separator = " " ,
    separator_highlight = { colors.text, colors.bg },
    condition = condition.buffer_not_empty,
    highlight = { colors.text, colors.bg },
  },
}
gls.left[5] = {
  FileName = {
    provider = function()
      --local path_to_file = vim.split(vim.fn.fnamemodify(vim.fn.expand("%"), ":~:."), file_sep)
      local path_to_file = vim.fn.fnamemodify(vim.fn.expand("%"), ":~:.")
      return M.file_with_icons(path_to_file)
    end,
    condition = condition.buffer_not_empty,
    separator = "" ,
    separator_highlight = { colors.text, colors.bg },
    highlight = { colors.text, colors.bg },
  },
}
gls.left[6] = {
  DiagnosticError = {
    provider = "DiagnosticError",
    icon = "  ",
    highlight = { colors.red, colors.bg },
  },
}
gls.left[7] = {
  Space = {
    provider = function()
      return " "
    end,
    highlight = { colors.bg, colors.bg },
  },
}
gls.left[8] = {
  DiagnosticWarn = {
    provider = "DiagnosticWarn",
    icon = "  ",
    highlight = { colors.blue, colors.bg },
  },
}
gls.right[1] = {
  FileIcon = {
    provider = "FileIcon",
    condition = condition.buffer_not_empty,
    highlight = { fileinfo.get_file_icon_color, colors.bg },
  },
}
gls.right[2] = {
  FileTypeName = {
    provider = function()
      return buffer.get_buffer_filetype():lower()
    end,
    separator_highlight = { colors.text, colors.bg },
    condition = condition.buffer_not_empty,
    highlight = { colors.text, colors.bg },
  },
}
gls.right[3] = {
  FileFormat = {
    provider = function()
      format = vim.bo.fileformat
      if format == "unix" then
        return ""
      elseif format == "mac" then
        return ""
      elseif format == "win" then
        return ""
      end
    end,
    separator = " ",
    separator_highlight = { colors.text, colors.bg },
    highlight = { colors.darkblue, colors.text },
  },
}
gls.right[4] = {
  LineInfo = {
    provider = function()
      local line = vim.fn.line(".")
      local lastline = vim.fn.line("$")
      local column = vim.fn.col(".")

      return ":" .. line .. "/" .. lastline .. " :" .. column
    end,
    separator = " | ",
    separator_highlight = { colors.darkblue, colors.text },
    highlight = { colors.darkblue, colors.text },
  },
}
gls.right[5] = {
  PerCent = {
    provider = "LinePercent",
    separator = " ",
    separator_highlight = { colors.darkblue, colors.text },
    highlight = { colors.darkblue, colors.text },
  },
}

gls.short_line_left = gls.left
gls.short_line_right = gls.right
