local gl = require("galaxyline")
local gls = gl.section
local api = vim.api
local M = {}

M.modes = setmetatable({
  ["n"]  = {["short"] = "N", ["long"] = "NORMAL" },
  ["no"] = {["short"] = "N·P", ["long"] = "" },
  ["v"]  = {["short"] = "V", ["long"] = "VISUAL" },
  ["V"]  = {["short"] = "V·L", ["long"] = "VISUAL LINE" },
  [""] = {["short"] = "V·B", ["long"] = "VISUAL BLOCK" },
  ["s"]  = {["short"] = "S", ["long"] = "" },
  ["S"]  = {["short"] = "S·L", ["long"] = "" },
  [""] = {["short"] = "S·B", ["long"] = "" },
  ["i"]  = {["short"] = "I", ["long"] = "INSERT" },
  ["ic"] = {["short"] = "I", ["long"] = "INSERT NORMAL" },
  ["R"]  = {["short"] = "R", ["long"] = "" },
  ["Rv"] = {["short"] = "V·R", ["long"] = "" },
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

local buffer_not_empty = function()
  if vim.fn.empty(vim.fn.expand("%:t")) ~= 1 then
    return true
  end
  return false
end

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
  if buffer_not_empty() then
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
  FileIcon = {
    provider = "FileIcon",
    condition = buffer_not_empty,
    highlight = { require("galaxyline.providers.fileinfo").get_file_icon_color, colors.bg },
  },
}
gls.left[4] = {
  FileName = {
    provider = "FileName",
    condition = buffer_not_empty,
    separator = " " ,
    separator_highlight = { colors.text, colors.bg },
    highlight = { colors.text, colors.bg },
  },
}

gls.left[5] = {
  GitIcon = {
    provider = function()
      return " "
    end,
    condition = buffer_not_empty,
    highlight = { colors.white, colors.bg },
  },
}
gls.left[6] = {
  GitBranch = {
    provider = "GitBranch",
    condition = buffer_not_empty,
    highlight = { colors.text, colors.bg },
  },
}
gls.left[7] = {
  LeftEnd = {
    provider = function()
      return " "
    end,
    condition = buffer_not_empty,
    separator = "" ,
    separator_highlight = { colors.text, colors.bg },
    highlight = { colors.bg, colors.bg },
  },
}
gls.left[8] = {
  DiagnosticError = {
    provider = "DiagnosticError",
    icon = "  ",
    highlight = { colors.red, colors.bg },
  },
}
gls.left[9] = {
  Space = {
    provider = function()
      return " "
    end,
    highlight = { colors.bg, colors.bg },
  },
}
gls.left[10] = {
  DiagnosticWarn = {
    provider = "DiagnosticWarn",
    icon = "  ",
    highlight = { colors.blue, colors.bg },
  },
}
gls.right[1] = {
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
    separator = "",
    separator_highlight = { colors.text, colors.bg },
    highlight = { colors.darkblue, colors.text },
  },
}
gls.right[2] = {
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
gls.right[3] = {
  PerCent = {
    provider = "LinePercent",
    separator = " ",
    separator_highlight = { colors.darkblue, colors.text },
    highlight = { colors.darkblue, colors.text },
  },
}

-- gls.short_line_left[1] = {
  -- FileIcon = {
    -- provider = "FileIcon",
    -- condition = buffer_not_empty,
    -- highlight = { require("galaxyline.providers.fileinfo").get_file_icon_color, colors.bg },
  -- },
-- }
--
-- gls.short_line_left[2] = {
  -- FileName = {
    -- provider = { "FileName"},
    -- condition = buffer_not_empty,
    -- highlight = { colors.text, colors.bg },
  -- },
-- }
--
-- gls.short_line_right[1] = {
  -- FileFormat = {
    -- provider = function()
      -- format = vim.bo.fileformat
      -- if format == "unix" then
        -- return ""
      -- elseif format == "mac" then
        -- return ""
      -- elseif format == "win" then
        -- return ""
      -- end
    -- end,
    -- separator = "",
    -- separator_highlight = { colors.text, colors.bg },
    -- highlight = { colors.darkblue, colors.text },
  -- },
-- }
-- gls.short_line_right[2] = {
  -- LineInfo = {
    -- provider = function()
      -- local line = vim.fn.line(".")
      -- local lastline = vim.fn.line("$")
      -- local column = vim.fn.col(".")
--
      -- return ":" .. line .. "/" .. lastline .. " :" .. column
    -- end,
    -- separator = " | ",
    -- separator_highlight = { colors.darkblue, colors.text },
    -- highlight = { colors.darkblue, colors.text },
  -- },
-- }
-- gls.short_line_right[3] = {
  -- PerCent = {
    -- provider = "LinePercent",
    -- separator = " ",
    -- separator_highlight = { colors.darkblue, colors.text },
    -- highlight = { colors.darkblue, colors.text },
  -- },
-- }
