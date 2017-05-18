leftHandSideWindow = function()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen()
    local max = screen:frame()

    f.x = max.x
    f.y = max.y
    f.w = max.w / 2
    f.h = max.h
    win:setFrame(f, 0)
end

rightHandSideWindow = function()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen()
    local max = screen:frame()

    f.x = max.x + (max.size.w / 2)
    f.y = max.y
    f.w = max.w / 2
    f.h = max.h

    win:setFrame(f, 0)
end

topHalfWindow = function()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen()
    local max = screen:frame()

    f.x = max.x
    f.y = max.y
    f.w = max.w
    f.h = max.h / 2

    win:setFrame(f, 0)
end

bottomHalfWindow = function()
    local win = hs.window.focusedWindow()
    local f = win:frame()
    local screen = win:screen()
    local max = screen:frame()

    f.x = max.x
    f.y = max.y + (max.h / 2)
    f.w = max.w
    f.h = max.h / 2

    win:setFrame(f, 0)
end

fullScreenWindow = function()
    local win = hs.window.focusedWindow()
    local screen = win:screen()

    win:setFrame(screen:frame(),0)
end

hs.hotkey.bind({"cmd", "alt", "shift", "ctrl"}, "Left", leftHandSideWindow)
hs.hotkey.bind({"cmd", "alt", "shift", "ctrl"}, "Right", rightHandSideWindow)
hs.hotkey.bind({"cmd", "alt", "shift", "ctrl"}, "Up", topHalfWindow)
hs.hotkey.bind({"cmd", "alt", "shift", "ctrl"}, "Down", bottomHalfWindow)
hs.hotkey.bind({"cmd", "alt", "shift", "ctrl"}, "f", fullScreenWindow)

function reloadConfig(files)
    doReload = false
    for _,file in pairs(files) do
        if file:sub(-4) == ".lua" then
            doReload = true
        end
    end
    if doReload then
        hs.reload()
    end
end
local myWatcher = hs.pathwatcher.new(os.getenv("HOME") .. "/.hammerspoon/", reloadConfig):start()

hs.alert.show("Hammerspoon ready")
