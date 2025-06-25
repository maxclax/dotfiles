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
		opts = {
			servers = {
				---@type vim.lsp.Config
				vue_ls = {
					cmd = { "vue-language-server", "--stdio" },
					filetypes = { "vue" },
					root_markers = { "package.json", "vue.config.js", "vite.config.ts", "vite.config.js", ".git" },
					init_options = {
						typescript = {
							tsdk = vim.fn.getcwd() .. "/node_modules/typescript/lib",
						},
					},
					settings = {
						vue = {
							completeFunctionCalls = true,
							updateImportsOnFileMove = { enabled = "always" },
							inlayHints = {
								missingProps = true,
								optionsWrapper = true,
							},
						},
					},
				},
			},
		},
	},
}
