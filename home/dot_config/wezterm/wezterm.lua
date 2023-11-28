-- Pull in the wezterm API

local wezterm = require "wezterm"

local config = {}

if wezterm.config_builder then
  config = wezterm.config_builder()
end

config.hide_tab_bar_if_only_one_tab = true

config.font = wezterm.font("Iosevka Term")
config.font_size = 16
config.window_padding = {
  left = "2px",
  right = "2px",
  top = 0,
  bottom = "1px",
}

return config
