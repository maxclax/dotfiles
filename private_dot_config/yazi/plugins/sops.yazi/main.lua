--- @since 26.5.6
--- sops.yazi — mark encrypted files (sops, age, gpg) in the file list.
---
--- Structure mirrors git.yazi: an async `fetch` sniffs file contents and stores
--- the result in the sync state, a `Linemode` child renders the cached mark.
--- Nothing here runs in the render path, so listing a directory never blocks.

--- Bytes read from the head of each candidate. sops writes `ENC[...]` values
--- inline, so the first chunk is enough; its `sops:` metadata block (appended
--- at the end for YAML) is not needed to identify the file.
local HEAD = 16384

--- Files larger than this are never read. Encrypted config files are small;
--- this only keeps stray large files out of the read path.
local MAX_LEN = 4 * 1024 * 1024

--- Size limit for extension-less files. `.env` and `kubeconfig` have no
--- extension as far as `Url.ext` is concerned (a leading dot is a stem, not an
--- extension), and sops dotenv files are exactly the case worth catching — so
--- they are sniffed too, but only while they stay small.
local MAX_LEN_NO_EXT = 64 * 1024

--- sops encrypts in place and keeps the original name — `kubeconfig.yaml` may
--- well be encrypted — so the extension can't decide the answer, only whether
--- the file is worth opening at all.
local EXTS = {
	yaml = true,
	yml = true,
	json = true,
	env = true,
	tfvars = true,
	properties = true,
	ini = true,
	toml = true,
	age = true,
	asc = true,
	gpg = true,
	sops = true,
}

local MARKERS = {
	"ENC%[AES256_GCM", -- sops-encrypted value (yaml, json, env, ini)
	"sops_mac=", -- sops dotenv metadata
	"BEGIN AGE ENCRYPTED FILE", -- age, armored (chezmoi's encrypted_* files)
	"age%-encryption%.org", -- age, binary header
	"BEGIN PGP MESSAGE", -- gpg, armored
}

---@param path string
---@return boolean
local function sniff(path)
	local f = io.open(path, "rb")
	if not f then
		return false
	end

	local head = f:read(HEAD) or ""
	f:close()

	for _, marker in ipairs(MARKERS) do
		if head:find(marker) then
			return true
		end
	end
	return false
end

---@param marks table<string, boolean>
local add = ya.sync(function(st, marks)
	st.marks = st.marks or {}
	for path, encrypted in pairs(marks) do
		st.marks[path] = encrypted or nil
	end
	ui.render()
end)

local function setup(st, opts)
	st.marks = st.marks or {}

	opts = opts or {}
	opts.order = opts.order or 1400

	local t = th.sops or {}
	local sign = t.sign or "" -- nf-fa-lock
	local style = t.style or ui.Style():fg("yellow")

	Linemode:children_add(function(self)
		if not self._file.in_current then
			return ""
		elseif not st.marks[tostring(self._file.url)] then
			return ""
		elseif self._file.is_hovered then
			return ui.Line { " ", sign }
		else
			return ui.Line { " ", ui.Span(sign):style(style) }
		end
	end, opts.order)
end

---@type UnstableFetcher
local function fetch(_, job)
	local marks = {}
	for _, file in ipairs(job.files) do
		local cha, ext = file.cha, file.url.ext
		local worth
		if cha.is_dir then
			worth = false
		elseif ext then
			worth = EXTS[ext:lower()] and cha.len <= MAX_LEN
		else
			worth = cha.len <= MAX_LEN_NO_EXT
		end

		marks[tostring(file.url)] = worth and sniff(tostring(file.url)) or false
	end

	add(marks)

	-- Returning false so the mark is refreshed when the directory is: a file
	-- flips between encrypted and plaintext on every `sops -e` / `sops -d`.
	return false
end

return { setup = setup, fetch = fetch }
