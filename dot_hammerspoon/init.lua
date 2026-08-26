-- 3-finger horizontal swipe → winner-undo / winner-redo, Emacs only.
-- The NS port never emits swipe events (etc/TODO: only `pinch` landed), so the
-- gesture is read here and replayed as the keys Emacs already binds.

local EMACS   = "org.gnu.Emacs"
local FINGERS = 3
local TRAVEL  = 0.09 -- fraction of trackpad width before it counts
local IDLE    = 0.25 -- seconds of no touch data that ends a gesture
local DEBUG   = false -- set true to log swipes to LOG
local LOG     = "/tmp/hammerspoon-gesture.log" -- outside configdir, see pathwatcher

local swipe = nil

local function note(msg)
  local f = io.open(LOG, "a")
  if f then
    f:write(os.date("%H:%M:%S ") .. msg .. "\n")
    f:close()
  end
end

-- posted globally, not to the app: Emacs ignores app-targeted events
local function chord(key)
  hs.eventtap.keyStroke({ "ctrl" }, "c", 20000)
  hs.timer.doAfter(0.05, function()
    hs.eventtap.keyStroke({}, key, 20000)
  end)
end

gestureTap = hs.eventtap.new({ hs.eventtap.event.types.gesture }, function(e)
  local app = hs.application.frontmostApplication()
  if not app or app:bundleID() ~= EMACS then
    swipe = nil
    return false
  end

  local down = {}
  for _, t in ipairs(e:getTouches() or {}) do
    if t.phase ~= "ended" and t.phase ~= "cancelled" then
      down[#down + 1] = t
    end
  end

  -- zero-touch events are interleaved with real ones mid-swipe, so they must
  -- not reset anything; a gesture ends after IDLE seconds of no touch data
  if #down == 0 then return false end

  local now = hs.timer.secondsSinceEpoch()
  if swipe and (now - swipe.last) > IDLE then
    if DEBUG and not swipe.fired then
      note(("miss peak=%d maxdx=%.3f maxdy=%.3f"):format(
        swipe.peak, swipe.maxdx or 0, swipe.maxdy or 0))
    end
    swipe = nil
  end
  if not swipe then
    swipe = { start = {}, peak = 0, fired = false }
  end
  swipe.last = now
  if #down > swipe.peak then swipe.peak = #down end

  -- per-finger origin, keyed by identity: robust to fingers coming and going
  for _, t in ipairs(down) do
    if not swipe.start[t.identity] then
      swipe.start[t.identity] = { x = t.normalizedPosition.x, y = t.normalizedPosition.y }
    end
  end

  -- exactly 3: four-finger horizontal is macOS space switching
  if swipe.fired then return false end

  local sdx, sdy, n = 0, 0, 0
  for _, t in ipairs(down) do
    local s = swipe.start[t.identity]
    if s then
      sdx = sdx + (t.normalizedPosition.x - s.x)
      sdy = sdy + (t.normalizedPosition.y - s.y)
      n = n + 1
    end
  end
  if n == 0 then return false end

  local dx, dy = sdx / n, sdy / n
  if math.abs(dx) > math.abs(swipe.maxdx or 0) then swipe.maxdx = dx end
  if math.abs(dy) > math.abs(swipe.maxdy or 0) then swipe.maxdy = dy end

  -- exactly 3: four-finger horizontal is macOS space switching
  if swipe.peak ~= FINGERS then return false end
  if math.abs(dx) > TRAVEL and math.abs(dx) > math.abs(dy) * 2 then
    swipe.fired = true
    -- right = back, matching Safari's swipe-to-go-back
    local key = dx > 0 and "left" or "right"
    if DEBUG then note(("FIRE dx=%.3f dy=%.3f fingers=%d → C-c <%s>"):format(dx, dy, swipe.peak, key)) end
    chord(key)
  end
  return false
end)

gestureTap:start()

if DEBUG then
  note("loaded; accessibility=" .. tostring(hs.accessibilityState()) ..
       " tap=" .. tostring(gestureTap:isEnabled()))
end

if not hs.accessibilityState() then
  hs.alert.show("Hammerspoon needs Accessibility permission")
  hs.accessibilityState(true)
end

-- Reload on save, .lua only
hs.pathwatcher.new(hs.configdir, function(files)
  for _, f in ipairs(files) do
    if f:sub(-4) == ".lua" then
      hs.reload()
      return
    end
  end
end):start()
