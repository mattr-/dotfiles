-- {{{ Adjust Hammerspoon defaults

hs.hotkey.alertDuration = 0 -- Don't show titles for hotkeys
hs.window.animationDuration = 0 -- Don't animate window movements/resizes
hs.screen.strictScreenInDirection = false -- Don't care about axes
-- }}}

hyper = {"cmd", "shift", "ctrl", "alt"}
reload_keys = {hyper, "R"}
hs.hotkey.bind(reload_keys[1],
               reload_keys[2],
               "Reload Configuration",
               function () hs.reload() end)


-- {{{ Resize Windows
-- TODO decide whether or not to reimplement these operations
-- if direction == "right" then
--     localf.w = localf.w+stepw
-- end
-- if direction == "left" then
--     localf.w = localf.w-stepw
-- end
-- if direction == "up" then
--     localf.h = localf.h-steph
-- end
-- if direction == "down" then
--     localf.h = localf.h+steph
-- end
-- if direction == "halfleft" then
--     localf.x = 0 localf.y = 0 localf.w = max.w/2 localf.h = max.h
-- end
-- if direction == "halfup" then
--     localf.x = 0 localf.y = 0 localf.w = max.w localf.h = max.h/2
-- end
-- if direction == "halfdown" then
--     localf.x = 0 localf.y = max.h/2 localf.w = max.w localf.h = max.h/2
-- end
-- if direction == "cornerNE" then
--     localf.x = max.w/2 localf.y = 0 localf.w = max.w/2 localf.h = max.h/2
-- end
-- if direction == "cornerSE" then
--     localf.x = max.w/2 localf.y = max.h/2 localf.w = max.w/2 localf.h = max.h/2
-- end
-- if direction == "cornerNW" then
--     localf.x = 0 localf.y = 0 localf.w = max.w/2 localf.h = max.h/2
-- end
-- if direction == "cornerSW" then
--     localf.x = 0 localf.y = max.h/2 localf.w = max.w/2 localf.h = max.h/2
-- end
-- if direction == "shrink" then
--     localf.x = localf.x+stepw localf.y = localf.y+steph localf.w = localf.w-(stepw*2) localf.h = localf.h-(steph*2)
-- end
-- if direction == "expand" then
--     localf.x = localf.x-stepw localf.y = localf.y-steph localf.w = localf.w+(stepw*2) localf.h = localf.h+(steph*2)
-- end
-- if direction == "mright" then
--     localf.x = localf.x+stepw
-- end
-- if direction == "mleft" then
--     localf.x = localf.x-stepw
-- end
-- if direction == "mup" then
--     localf.y = localf.y-steph
-- end
-- if direction == "mdown" then
--     localf.y = localf.y+steph
-- end
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
    window.x = (max.w - window.w)/2
    window.y = (max.h - window.h)/2
end

function hd720(window, screen)
    local max = screen:fullFrame()
    window.w = 1280
    window.h = 720
    window.x = (max.w - window.w)/2
    window.y = (max.h - window.h)/2
end

function halfright(window, screen)
    local max = screen:fullFrame()
    window.x = max.w/2
    window.y = 0
    window.w = max.w/2
    window.h = max.h
end

function halfleft(window, screen)
    local max = screen:fullFrame()
    window.x = 0 window.y = 0 window.w = max.w/2 window.h = max.h
end

function center(window, screen)
    local max = screen:fullFrame()
    window.x = (max.w-window.w)/2 window.y = (max.h-window.h)/2
end

function fcenter(window, screen)
    local max = screen:fullFrame()
    local stepw = max.w/30
    local steph = max.h/30
    window.x = stepw*5 window.y = steph*5 window.w = stepw*20 window.h = steph*20
end

function fullscreen(window, screen)
    local max = screen:fullFrame()
    window.x = 0 window.y = 0 window.w = max.w window.h = max.h
end

function halfmiddlecenter(window, screen)
    local max = screen:fullFrame()
    window.w = max.w / 2
    window.h = max.h
    window.x = window.w / 2
    window.y = 0
end
 -- }}}

resizeextra_lefthalf_keys = {hyper, "left"}
hs.hotkey.bind(resizeextra_lefthalf_keys[1], resizeextra_lefthalf_keys[2], nil, function() resize_win(halfleft) end)

resizeextra_righthalf_keys = {hyper, "right"}
hs.hotkey.bind(resizeextra_righthalf_keys[1], resizeextra_righthalf_keys[2], nil, function() resize_win(halfright) end)

resizeextra_fullscreen_keys = {hyper, "up"}
hs.hotkey.bind(resizeextra_fullscreen_keys[1], resizeextra_fullscreen_keys[2], nil, function() resize_win(fullscreen) end)

resizeextra_fcenter_keys = {hyper, "down"}
hs.hotkey.bind(resizeextra_fcenter_keys[1], resizeextra_fcenter_keys[2], nil, function() resize_win(fcenter) end)

resizeextra_center_keys = {hyper, "return"}
hs.hotkey.bind(resizeextra_center_keys[1], resizeextra_center_keys[2], nil, function() resize_win(center) end)

resizeextra_720hd_keys = {hyper, "7"}
hs.hotkey.bind(resizeextra_720hd_keys[1], resizeextra_720hd_keys[2], nil, function() resize_win(hd720) end)

resizeextra_1080_keys = {hyper, "9"}
hs.hotkey.bind(resizeextra_1080_keys[1], resizeextra_1080_keys[2], nil, function() resize_win(hd1080) end)

resizeextra_halfmiddlecenter_keys = {hyper, "M"}
hs.hotkey.bind(resizeextra_halfmiddlecenter_keys[1], resizeextra_halfmiddlecenter_keys[2], nil, function() resize_win(halfmiddlecenter) end)

-- Load Hammerspoon bits from https://github.com/jasonrudolph/ControlEscape.spoon
hs.loadSpoon('ControlEscape'):start()
