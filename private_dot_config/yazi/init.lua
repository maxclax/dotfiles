-- yazi init.lua — plugin setup (plugins installed via `ya pkg install`)
require("full-border"):setup()
require("git"):setup()
require("smart-enter"):setup({ open_multi = true })
require("sops"):setup()

-- Ranger-style metadata bars. Only cheap, already-cached data goes here:
-- the status line is redrawn synchronously, so no I/O (df, du) belongs in it.
-- Directory totals live on <A-s> (what-size) and free space on <A-d> instead.

-- user@host in the header, left of the cwd
Header:children_add(function()
	if ya.target_family() ~= "unix" then
		return ""
	end
	return ui.Span(ya.user_name() .. "@" .. ya.host_name() .. ":"):fg("blue")
end, 500, Header.LEFT)

-- owner:group of the hovered file
Status:children_add(function()
	local h = cx.active.current.hovered
	if not h or ya.target_family() ~= "unix" then
		return ""
	end
	return ui.Line {
		ui.Span(ya.user_name(h.cha.uid) or tostring(h.cha.uid)):fg("magenta"),
		":",
		ui.Span(ya.group_name(h.cha.gid) or tostring(h.cha.gid)):fg("magenta"),
		" ",
	}
end, 500, Status.RIGHT)

-- mtime of the hovered file
Status:children_add(function()
	local h = cx.active.current.hovered
	if not h or not h.cha.mtime then
		return ""
	end
	return ui.Line {
		ui.Span(os.date("%Y-%m-%d %H:%M", math.floor(h.cha.mtime))):fg("cyan"),
		" ",
	}
end, 510, Status.RIGHT)
