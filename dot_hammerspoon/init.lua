-- Trackpad gestures, read here and replayed as keys.
-- 3-finger horizontal swipe → winner-undo / winner-redo, Emacs only: the NS
-- port never emits swipe events (etc/TODO: only `pinch` landed).
-- 4-finger tap → ^⌥⇧⌘T, in any app.

local EMACS         = "org.gnu.Emacs"
local SWIPE_FINGERS = 3
local TRAVEL        = 0.09 -- fraction of trackpad width before a swipe counts
local IDLE          = 0.25 -- seconds of no touch data that ends a gesture
local TAP_FINGERS   = 4
local TAP_TRAVEL    = 0.035 -- no finger may drift further than this
local TAP_HOLD      = 0.35 -- and all must lift within this
local TAP_SETTLE    = 0.12 -- silence after which the fingers count as lifted
local DEBUG         = false -- set true to log gestures to LOG
local LOG           = "/tmp/hammerspoon-gesture.log" -- outside configdir, see pathwatcher

local seq, tapTimer = nil, nil

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

local function hotkey()
  hs.eventtap.keyStroke({ "ctrl", "alt", "shift", "cmd" }, "t", 20000)
end

gestureTap = hs.eventtap.new({ hs.eventtap.event.types.gesture }, function(e)
  local down = {}
  for _, t in ipairs(e:getTouches() or {}) do
    if t.phase ~= "ended" and t.phase ~= "cancelled" then
      down[#down + 1] = t
    end
  end

  -- zero-touch events are interleaved with real ones mid-gesture, so they must
  -- not reset anything; a gesture ends after IDLE seconds of no touch data
  if #down == 0 then return false end

  local now = hs.timer.secondsSinceEpoch()
  if seq and (now - seq.last) > IDLE then
    if DEBUG and not seq.fired then
      note(("miss peak=%d maxdx=%.3f dist=%.3f"):format(
        seq.peak, seq.maxdx or 0, seq.dist))
    end
    seq = nil
  end
  if not seq then
    seq = { start = {}, peak = 0, began = now, dist = 0, fired = false }
  end
  seq.last = now
  if #down > seq.peak then seq.peak = #down end

  -- per-finger origin, keyed by identity: robust to fingers coming and going
  for _, t in ipairs(down) do
    if not seq.start[t.identity] then
      seq.start[t.identity] = { x = t.normalizedPosition.x, y = t.normalizedPosition.y }
    end
  end

  local sdx, sdy, n = 0, 0, 0
  for _, t in ipairs(down) do
    local s = seq.start[t.identity]
    if s then
      local fx = t.normalizedPosition.x - s.x
      local fy = t.normalizedPosition.y - s.y
      sdx, sdy, n = sdx + fx, sdy + fy, n + 1
      -- furthest any single finger drifted: a swipe moves them together, so
      -- the mean would also catch a pinch, which must not read as a tap
      local d = math.sqrt(fx * fx + fy * fy)
      if d > seq.dist then seq.dist = d end
    end
  end
  if n == 0 then return false end

  local dx, dy = sdx / n, sdy / n
  if math.abs(dx) > math.abs(seq.maxdx or 0) then seq.maxdx = dx end

  -- a tap is only recognisable once the fingers are off, so decide on a timer
  if tapTimer then tapTimer:stop() end
  local this = seq
  tapTimer = hs.timer.doAfter(TAP_SETTLE, function()
    if this.fired or this.peak ~= TAP_FINGERS then return end
    if this.dist > TAP_TRAVEL or (this.last - this.began) > TAP_HOLD then return end
    this.fired = true
    if DEBUG then
      note(("TAP fingers=%d dist=%.3f hold=%.2f"):format(
        this.peak, this.dist, this.last - this.began))
    end
    hotkey()
  end)

  if seq.fired then return false end

  -- swipe: Emacs only, and exactly 3 — four-finger horizontal switches spaces
  local app = hs.application.frontmostApplication()
  if not app or app:bundleID() ~= EMACS then return false end
  if seq.peak ~= SWIPE_FINGERS then return false end
  if math.abs(dx) > TRAVEL and math.abs(dx) > math.abs(dy) * 2 then
    seq.fired = true
    -- right = back, matching Safari's swipe-to-go-back
    local key = dx > 0 and "left" or "right"
    if DEBUG then note(("FIRE dx=%.3f dy=%.3f fingers=%d → C-c <%s>"):format(dx, dy, seq.peak, key)) end
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
