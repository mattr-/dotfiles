-- {{{ Adjust Hammerspoon defaults

hs.hotkey.alertDuration = 0 -- Don't show titles for hotkeys
hs.window.animationDuration = 0 -- Don't animate window movements/resizes
-- }}}

hyper = {"cmd", "shift", "ctrl", "alt"}
reload_keys = {hyper, "R"}
hs.hotkey.bind(reload_keys[1],
               reload_keys[2],
               "Reload Configuration",
               function () hs.reload() end)

time_keys = {hyper, "T"}
hs.hotkey.bind(time_keys[1],
               time_keys[2],
               "Show a clock",
               function() show_time() end)

darkblue = {red=24/255,blue=195/255,green=145/255,alpha=1}

-- {{{ Show the time
function close_time_display()
   time_draw:delete()
   time_draw = nil
end

function show_time()
   if time_draw == nil then
      local mainScreen = hs.screen.mainScreen()
      local mainRes = mainScreen:fullFrame()
      local localMainRes = mainScreen:absoluteToLocal(mainRes)
      local time_str = hs.styledtext.new(os.date("%H:%M"),
                                         {font={name="Impact",size=120},
                                          color=darkblue,
                                          paragraphStyle={alignment="center"}})
      local timeframe = hs.geometry.rect(mainScreen:localToAbsolute(
                                            (localMainRes.w - 300) / 2,
                                            (localMainRes.h - 200) / 2,
                                            300, 200))
      time_draw = hs.drawing.text(timeframe,time_str)
      time_draw:setLevel(hs.drawing.windowLevels.overlay)
      time_draw:show()
      if ttimer == nil then
         ttimer = hs.timer.doAfter(1.5, close_time_display)
      else
         ttimer:start()
      end
  else
      ttimer:stop()
      close_time_display()
  end
end -- }}}

-- {{{ Resize Windows
function resize_win(direction)
    local win = hs.window.focusedWindow()
    if win then
        local f = win:frame()
        local screen = win:screen()
        local localf = screen:absoluteToLocal(f)
        local max = screen:fullFrame()
        local stepw = max.w/30
        local steph = max.h/30
        if direction == "right" then
            localf.w = localf.w+stepw
        end
        if direction == "left" then
            localf.w = localf.w-stepw
        end
        if direction == "up" then
            localf.h = localf.h-steph
        end
        if direction == "down" then
            localf.h = localf.h+steph
        end
        if direction == "halfright" then
            localf.x = max.w/2 localf.y = 0 localf.w = max.w/2 localf.h = max.h
        end
        if direction == "halfleft" then
            localf.x = 0 localf.y = 0 localf.w = max.w/2 localf.h = max.h
        end
        if direction == "halfup" then
            localf.x = 0 localf.y = 0 localf.w = max.w localf.h = max.h/2
        end
        if direction == "halfdown" then
            localf.x = 0 localf.y = max.h/2 localf.w = max.w localf.h = max.h/2
        end
        if direction == "cornerNE" then
            localf.x = max.w/2 localf.y = 0 localf.w = max.w/2 localf.h = max.h/2
        end
        if direction == "cornerSE" then
            localf.x = max.w/2 localf.y = max.h/2 localf.w = max.w/2 localf.h = max.h/2
        end
        if direction == "cornerNW" then
            localf.x = 0 localf.y = 0 localf.w = max.w/2 localf.h = max.h/2
        end
        if direction == "cornerSW" then
            localf.x = 0 localf.y = max.h/2 localf.w = max.w/2 localf.h = max.h/2
        end
        if direction == "center" then
            localf.x = (max.w-localf.w)/2 localf.y = (max.h-localf.h)/2
        end
        if direction == "fcenter" then
            localf.x = stepw*5 localf.y = steph*5 localf.w = stepw*20 localf.h = steph*20
        end
        if direction == "fullscreen" then
            localf.x = 0 localf.y = 0 localf.w = max.w localf.h = max.h
        end
        if direction == "shrink" then
            localf.x = localf.x+stepw localf.y = localf.y+steph localf.w = localf.w-(stepw*2) localf.h = localf.h-(steph*2)
        end
        if direction == "expand" then
            localf.x = localf.x-stepw localf.y = localf.y-steph localf.w = localf.w+(stepw*2) localf.h = localf.h+(steph*2)
        end
        if direction == "mright" then
            localf.x = localf.x+stepw
        end
        if direction == "mleft" then
            localf.x = localf.x-stepw
        end
        if direction == "mup" then
            localf.y = localf.y-steph
        end
        if direction == "mdown" then
            localf.y = localf.y+steph
        end
        if direction == "720hd" then
           localf.x = 0
           localf.y = 180
           localf.w = 1280
           localf.h = 720
        end
        if direction == "1080hd" then
            localf.x = 0
            localf.y = 0
            localf.w = 1920
            localf.h = 1080
        end
        local absolutef = screen:localToAbsolute(localf)
        win:setFrame(absolutef)
    else
        hs.alert.show("No focused window!")
    end
end -- }}}

resizeextra_lefthalf_keys = {hyper, "left"}
hs.hotkey.bind(resizeextra_lefthalf_keys[1], resizeextra_lefthalf_keys[2], nil, function() resize_win('halfleft') end)

resizeextra_righthalf_keys = {hyper, "right"}
hs.hotkey.bind(resizeextra_righthalf_keys[1], resizeextra_righthalf_keys[2], nil, function() resize_win('halfright') end)

resizeextra_fullscreen_keys = {hyper, "up"}
hs.hotkey.bind(resizeextra_fullscreen_keys[1], resizeextra_fullscreen_keys[2], nil, function() resize_win('fullscreen') end)

resizeextra_fcenter_keys = {hyper, "down"}
hs.hotkey.bind(resizeextra_fcenter_keys[1], resizeextra_fcenter_keys[2], nil, function() resize_win('fcenter') end)

resizeextra_center_keys = {hyper, "return"}
hs.hotkey.bind(resizeextra_center_keys[1], resizeextra_center_keys[2], nil, function() resize_win('center') end)

resizeextra_720hd_keys = {hyper, "7"}
hs.hotkey.bind(resizeextra_720hd_keys[1], resizeextra_720hd_keys[2], nil, function() resize_win('720hd') end)

resizeextra_1080_keys = {hyper, "9"}
hs.hotkey.bind(resizeextra_1080_keys[1], resizeextra_1080_keys[2], nil, function() resize_win('1080hd') end)


-- {{{ Modal window management configuration
modal_toggle_key = "W"
window_manager = hs.hotkey.modal.new(hyper, modal_toggle_key)
window_manager:bind('', 'escape', function() window_manager:exit() end)
window_manager:bind('', "L", function() resize_win("halfleft") end)
window_manager:bind('', "R", function() resize_win("halfright") end)
window_manager:bind('', "T", function() resize_win("halfup") end)
window_manager:bind('', "B", function() resize_win("halfdown") end)
-- }}}

-- Load Hammerspoon bits from https://github.com/jasonrudolph/ControlEscape.spoon
hs.loadSpoon('ControlEscape'):start()
