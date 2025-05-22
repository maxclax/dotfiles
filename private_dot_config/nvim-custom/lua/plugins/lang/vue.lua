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
		opts = {
			servers = {
				volar = {
					-- Add these lines to fix the error
					cmd = { "vue-language-server", "--stdio" },
					filetypes = { "typescript", "javascript", "javascriptreact", "typescriptreact", "vue", "json" },
					root_markers = { "package.json", "vite.config.ts", ".git" },
					init_options = {
						vue = {
							hybridMode = false,
						},
						settings = {
							typescript = {
								inlayHints = {
									enumMemberValues = {
										enabled = true,
									},
									functionLikeReturnTypes = {
										enabled = true,
									},
									propertyDeclarationTypes = {
										enabled = true,
									},
									parameterTypes = {
										enabled = true,
										suppressWhenArgumentMatchesName = true,
									},
									variableTypes = {
										enabled = true,
									},
								},
							},
						},
						typescript = {
							tsdk = vim.fn.expand(
								"$HOME/.local/share/nvim-custom/mason/packages/typescript-language-server/node_modules/typescript/lib"
							),
						},
					},
				},
				vtsls = {},
			},
		},
	},
}
