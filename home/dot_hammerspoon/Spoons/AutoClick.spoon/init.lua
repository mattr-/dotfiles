--- === AutoClick ===
---
--- Autoclicker tool, configurable with clicks per seconds
---
--- Download: [https://github.com/Carleslc/Spoons/raw/master/Spoons/AutoClick.spoon.zip](https://github.com/Carleslc/Spoons/raw/master/Spoons/AutoClick.spoon.zip)

local obj = {}
obj.__index = obj

-- Metadata

obj.name = "AutoClick"
obj.version = "1.0"
obj.author = "Carlos Lázaro Costa <lazaro.costa.carles@gmail.com>"
obj.homepage = "https://github.com/Carleslc/"
obj.license = "MIT - https://opensource.org/licenses/MIT"

-- Utils

function notify(text, seconds, title)
  title = title or "Hammerspoon"
  print(title .. ": " .. text)
  hs.notify.new({title = title, informativeText = text, withdrawAfter = seconds or 5})
end

function obj:notify(text)
    notify(text, 2, obj.name)
end

-- Configuration

--- AutoClick:bindHotkeys(mapping)
--- Method
--- Binds hotkeys for AutoClick
---
--- Parameters:
---  * mapping - A table containing hotkey modifier/key details for the following items:
---   * triggerAutoClick - Start/Stop AutoClick
function obj:bindHotkeys(mapping)
   local def = { triggerAutoClick = hs.fnutils.partial(self.trigger, self) }
   hs.spoons.bindHotkeysToSpec(def, mapping)
end

--- AutoClick.clicksPerSecond
--- Variable
--- Clicks per second. May not work properly if set too high (above ~50). Defaults to 10.
obj.clicksPerSecond = 10

--- AutoClick:init()
--- Method
--- Initializes AutoClick
---
--- Parameters:
---  * None
function obj:init()
    self.running = false
end

--- AutoClick:trigger()
--- Method
--- Start/Stop AutoClick
---
--- Parameters:
---  * None
function obj:trigger()
    self.running = not self:isRunning()
    self:notify(self:isRunning() and "Started" or "Stopped")
    local delayMillis = 1000/self.clicksPerSecond
    local click = function() hs.eventtap.leftClick(hs.mouse.getAbsolutePosition(), delayMillis*1000/2 --[[microseconds]]) end
    hs.timer.doWhile(self.isRunning, click, delayMillis/1000/2 --[[seconds]])
end

-- Methods

--- AutoClick:isRunning()
--- Method
--- Checks if this spoon is enabled and running
---
--- Parameters:
---  * None
---
--- Returns:
---  * A boolean, true if this spoon is running, otherwise false
function obj:isRunning()
    return obj.running
end

-- Return spoon object

return obj
