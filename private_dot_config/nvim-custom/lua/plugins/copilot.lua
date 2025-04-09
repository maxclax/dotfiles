return {
	{
		"zbirenbaum/copilot.lua",
		lazy = true,
		commit = "5a8fdd34bb67eadc3f69e46870db0bed0cc9841c",
		event = "InsertEnter",
		enabled = true,
		dependencies = {
			{
				"nvim-lualine/lualine.nvim",
				event = "VeryLazy",
				opts = function(_, opts)
					local function codepilot()
						local icon = require("utils.icons").icons.kinds.Copilot
						return icon
					end

					local colors = {
						-- statuses NOT supported by copilot.lua (see `require("copilot").api.status`)
						["Offline"] = require("utils.colors").fgcolor("Comment"),
						-- statuses supported by copilot.lua
						[""] = require("utils.colors").fgcolor("Special"),
						["InProgress"] = require("utils.colors").fgcolor("DiagnosticWarning"),
						["Normal"] = require("utils.colors").fgcolor("DiagnosticOk"),
						["Warning"] = require("utils.colors").fgcolor("DiagnosticError"),
					}

					opts.copilot = {
						lualine_component = {
							codepilot,
							color = function()
								if not package.loaded["copilot"] or vim.g.custom_copilot_status == "disabled" then
									-- offline
									return colors["Offline"]
								else
									-- online
									local status = require("copilot.api").status
									if status.data.message ~= "" then
										-- NOTE: could potentially do something based on status.data.message too.
										vim.notify("Copilot message: " .. vim.inspect(status.data.message))
									end
									return colors[status.data.status]
								end
							end,
						},
					}
				end,
			},
		},
		cmd = "Copilot",
		build = ":Copilot auth",
		opts = {
			panel = {
				enabled = true,
				auto_refresh = true,
			},
			suggestion = {
				enabled = true,
				auto_trigger = true,
				accept = false, -- disable built-in keymapping
				keymap = {
					accept = "<Tab>",
					reject = "<C-e>",
					next = "<C-n>",
					prev = "<C-p>",
				},
			},
			filetypes = {
				sh = function()
					if string.match(vim.fs.basename(vim.api.nvim_buf_get_name(0)), "^%.env.*") then
						-- disable for .env files
						return false
					end
					return true
				end,
			},
		},
		config = function(_, opts)
			require("copilot").setup(opts)
			-- Make sure not to enable copilot in private projects
			require("utils.private").toggle_copilot()
		end,
		keys = require("config.keymaps").setup_copilot_keymaps(),
	},
}
