return {
	{
		"yetone/avante.nvim",
		lazy = true,
		build = "make",
		opts = {
			claude = {
				api_key_name = "cmd:op read op://Personal/Anthropic/tokens/neovim --no-newline",
			},
			openai = {
				api_key_name = "cmd:op read op://Personal/OpenAI/tokens/neovim --no-newline",
			},
			copilot = {
				-- Copilot doesn't need an API key configuration here since it uses
				-- GitHub authentication handled by copilot.vim plugin
				enabled = true,
			},
		},
		dependencies = {
			"nvim-treesitter/nvim-treesitter",
			"stevearc/dressing.nvim",
			"nvim-lua/plenary.nvim",
			"MunifTanjim/nui.nvim",
			"github/copilot.vim", -- Add copilot dependency
			{
				"MeanderingProgrammer/render-markdown.nvim",
				opts = {
					file_types = { "markdown", "Avante" },
				},
				ft = { "markdown", "Avante" },
			},
		},
		cmd = {
			"AvanteAsk",
			"AvanteChat",
			"AvanteEdit",
			"AvanteToggle",
			"AvanteClear",
			"AvanteFocus",
			"AvanteRefresh",
			"AvanteSwitchProvider",
		},
		keys = require("config.keymaps").setup_avante_keymaps(),
	},
}
