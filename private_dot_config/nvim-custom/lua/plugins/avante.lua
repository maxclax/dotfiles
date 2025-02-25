return {
	{
		"yetone/avante.nvim",
		lazy = true,
		build = "make",
		opts = {
			provider = "claude",
			claude = {
				api_key_name = "cmd:op read op://Personal/Anthropic/tokens/neovim --no-newline",
				model = "claude-3-7-sonnet-20250219",
			},
			openai = {
				api_key_name = "cmd:op read op://Personal/OpenAI/tokens/neovim --no-newline",
				model = "o3-mini",
			},
			copilot = {
				-- Copilot doesn't need an API key configuration here since it uses
				-- GitHub authentication handled by copilot.vim plugin
				enabled = true,
				model = "claude-3.7-sonnet",
			},
		},
		dependencies = {
			"nvim-treesitter/nvim-treesitter",
			"stevearc/dressing.nvim",
			"nvim-lua/plenary.nvim",
			"MunifTanjim/nui.nvim",
			"zbirenbaum/copilot.lua", -- for providers='copilot'
			{
				-- support for image pasting
				"HakonHarnes/img-clip.nvim",
				event = "VeryLazy",
				opts = {
					-- recommended settings
					default = {
						embed_image_as_base64 = false,
						prompt_for_file_name = false,
						drag_and_drop = {
							insert_mode = true,
						},
						-- required for Windows users
						use_absolute_path = true,
					},
				},
			},
			{
				"MeanderingProgrammer/render-markdown.nvim",
				opts = {
					file_types = { "markdown", "Avante" },
				},
				ft = { "markdown", "Avante" },
			},
		},
		mappings = {
			--- @class AvanteConflictMappings
			diff = {
				ours = "co",
				theirs = "ct",
				all_theirs = "ca",
				both = "cb",
				cursor = "cc",
				next = "]x",
				prev = "[x",
			},
			suggestion = {
				accept = "<Tab>",
				next = "<C-n>",
				prev = "<C-p>",
				dismiss = "<C-e>",
			},
			jump = {
				next = "]]",
				prev = "[[",
			},
			submit = {
				normal = "<CR>",
				insert = "<C-s>",
			},
			sidebar = {
				apply_all = "A",
				apply_cursor = "a",
				switch_windows = "<Tab>",
				reverse_switch_windows = "<S-Tab>",
			},
		},
		hints = { enabled = true },
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
