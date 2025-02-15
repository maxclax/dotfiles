local function set_dark()
	vim.o.background = "light" -- NOTE: tokyonight-moon uses light background
	vim.cmd.colorscheme("tokyonight-moon")
end

local function set_light()
	vim.o.background = "light"
	vim.cmd.colorscheme("dayfox")
end

return {
	{
		"f-person/auto-dark-mode.nvim",
		lazy = false,
		enabled = true,
		priority = 1000,
		dependencies = {},
		init = function()
			set_dark() -- avoid flickering when starting nvim, default to dark mode
		end,
		opts = {
			update_interval = 3000, -- milliseconds
			set_dark_mode = function()
				set_dark()
			end,
			set_light_mode = function()
				set_light()
			end,
		},
	},
	{
		"uga-rosa/ccc.nvim",
		enabled = false, -- NOTE: enable when needed
		opts = {
			highlighter = {
				auto_enable = true,
				lsp = true,
			},
		},
		config = function(_, opts)
			local ccc = require("ccc")
			ccc.setup(opts)
		end,
	},
	{
		"folke/tokyonight.nvim",
		enabled = true,
		lazy = true,
		---@class tokyonight.Config
		opts = {
			transparent = false, -- Enable transparency
			styles = {
				-- Background styles. Can be "dark", "transparent" or "normal"
				sidebars = "dark",
				floats = "dark",
			},
			dim_inactive = false, -- dims inactive windows
		},
	},
	{
		"catppuccin/nvim",
		enabled = true,
		lazy = true,
		name = "catppuccin", -- or Lazy will show the plugin as "nvim"
		opts = {
			-- transparent_background = true,
		},
	},
	{
		"EdenEast/nightfox.nvim",
		enabled = true,
		lazy = true,
	},
}
