local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not (vim.uv or vim.loop).fs_stat(lazypath) then
	local lazyrepo = "https://github.com/folke/lazy.nvim.git"
	local out = vim.fn.system({ "git", "clone", "--filter=blob:none", "--branch=stable", lazyrepo, lazypath })
	if vim.v.shell_error ~= 0 then
		vim.api.nvim_echo({
			{ "Failed to clone lazy.nvim:\n", "ErrorMsg" },
			{ out, "WarningMsg" },
			{ "\nPress any key to exit..." },
		}, true, {})
		vim.fn.getchar()
		os.exit(1)
	end
end
vim.opt.rtp:prepend(lazypath)

require("lazy").setup({

	-- load and confgure plugins in this order (plugins, languages, core, project-specific overrides):
	spec = {
		-- add LazyVim and import its plugins
		-- { "LazyVim/LazyVim", import = "lazyvim.plugins" },
		-- import all plugins and their configs
		{ import = "plugins" },
		-- import language configs
		{ import = "plugins.lang" },
		-- import core configs
		{ import = "plugins.core" },
	},

	-- import per-project config
	-- NOTE: this is built into lazy.nvim; place a .lazy.lua file in the project's
	-- root directory, containing a lazy spec and it will be merged in at the end of the above spec.
	local_spec = true,

	checker = { enabled = false }, -- automatically check for plugin updates
	performance = {
		rtp = {
			-- disable some rtp plugins
			disabled_plugins = {
				"gzip",
				-- "matchit",
				-- "matchparen",
				-- "netrwPlugin",
				"tarPlugin",
				"tohtml",
				"tutor",
				"zipPlugin",
			},
		},
	},
})
