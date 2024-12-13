-- {{{ Adjust Hammerspoon defaults

hs.hotkey.alertDuration = 0 -- Don't show titles for hotkeys
hs.window.animationDuration = 0 -- Don't animate window movements/resizes
hs.screen.strictScreenInDirection = false -- Don't care about axes
-- }}}

hyper = { "cmd", "shift", "ctrl", "alt" }
reload_keys = { hyper, "H" }
hs.hotkey.bind(reload_keys[1], reload_keys[2], "Reload Configuration", function()
  hs.reload()
end)

function resize_win(direction)
  local win = hs.window.focusedWindow()
  if win then
    local f = win:frame()
    local screen = win:screen()
    local localf = screen:absoluteToLocal(f)
    direction(localf, screen)
    local absolutef = screen:localToAbsolute(localf)
    win:setFrame(absolutef)
  else
    hs.alert.show("No focused window!")
  end
end

function hd1080(window, screen)
  local max = screen:fullFrame()
  window.w = 1920
  window.h = 1080
  window.x = (max.w - window.w) / 2
  window.y = (max.h - window.h) / 2
end

function hd720(window, screen)
  local max = screen:fullFrame()
  window.w = 1280
  window.h = 720
  window.x = (max.w - window.w) / 2
  window.y = (max.h - window.h) / 2
end

-- }}}

resizeextra_720hd_keys = { hyper, "7" }
hs.hotkey.bind(resizeextra_720hd_keys[1], resizeextra_720hd_keys[2], nil, function()
  resize_win(hd720)
end)

resizeextra_1080_keys = { hyper, "9" }
hs.hotkey.bind(resizeextra_1080_keys[1], resizeextra_1080_keys[2], nil, function()
  resize_win(hd1080)
end)

-- Load Hammerspoon bits from https://github.com/jasonrudolph/ControlEscape.spoon
hs.loadSpoon("ControlEscape"):start()
AutoClick = hs.loadSpoon("AutoClick")
AutoClick:init()
AutoClick.clicksPerSecond = 25
AutoClick:bindHotkeys({triggerAutoClick = {hyper, "0"}})

