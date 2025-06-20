return {
	{
		"nvim-treesitter/nvim-treesitter",
		opts = { ensure_installed = { "vue", "css" } },
	},

	{
		"virtual-lsp-config",
		dependencies = {
			{
				"mason-org/mason-lspconfig.nvim",
				dependencies = {
					{
						"mason-org/mason.nvim",
					},
				},
			},
		},
		opts = {},
	},
}
