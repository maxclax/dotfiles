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
				opts = function(_, opts)
					opts.ensure_installed = opts.ensure_installed or {}
					vim.list_extend(opts.ensure_installed, { "vue_ls" })
				end,
			},
		},
	},
}
