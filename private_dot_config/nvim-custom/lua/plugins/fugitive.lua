return {
	{
		"tpope/vim-fugitive",
		cmd = {
			"G",
			"Git",
			"Gdiffsplit",
			"Gread",
			"Gwrite",
			"Ggrep",
			"GMove",
			"GDelete",
			"GBrowse",
			"GRemove",
			"GRename",
			"Glgrep",
			"Gedit",
		},
		ft = { "fugitive" },
		keys = {
			{ "<leader>gfg", "<cmd>Git<cr>", desc = "Git status" },
			{ "<leader>gfb", "<cmd>Git blame<cr>", desc = "Git blame" },
			{ "<leader>gfd", "<cmd>Gdiffsplit<cr>", desc = "Git diff" },
			{ "<leader>gfl", "<cmd>Git log<cr>", desc = "Git log" },
			{ "<leader>gfp", "<cmd>Git push<cr>", desc = "Git push" },
			{ "<leader>gfP", "<cmd>Git pull<cr>", desc = "Git pull" },
		},
	},
}
