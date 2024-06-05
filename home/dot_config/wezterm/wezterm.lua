-- Pull in the wezterm API

local wezterm = require "wezterm"

local config = {}

if wezterm.config_builder then
  config = wezterm.config_builder()
end

config.hide_tab_bar_if_only_one_tab = true
config.enable_scroll_bar = true

config.font = wezterm.font("Iosevka")
config.font_size = 14

config.window_padding = {
  left = "0.5cell",
  right = "1cell",
  top = "0.2cell",
  bottom = ".75cell",
}

return config
