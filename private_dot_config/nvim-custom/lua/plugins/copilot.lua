return {
	{
		"zbirenbaum/copilot.lua",
		lazy = true,
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

					-- local function fgcolor(name)
					--   local hl = vim.api.nvim_get_hl and vim.api.nvim_get_hl(0, { name = name, link = false })
					--   local fg = hl and (hl.fg or hl.foreground)
					--   return fg and { fg = string.format("#%06x", fg) } or nil
					-- end

					local colors = {
						["Normal"] = require("utils.colors").fgcolor("Special"),
						["Warning"] = require("utils.colors").fgcolor("DiagnosticError"),
						["InProgress"] = require("utils.colors").fgcolor("DiagnosticWarn"),
						["Offline"] = require("utils.colors").fgcolor("Comment"),
						["Error"] = require("utils.colors").fgcolor("Error"),
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
									if status.data.status ~= "" or status.data.message ~= "" then
										return colors[status.data.status] or colors["Error"]
									else
										return colors["InProgress"]
									end
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
				-- use the built-in keymapping for "accept" (<M-l>)
				auto_trigger = true,
				accept = false, -- disable built-in keymapping
				keymap = {
					accept = "<M-right>", -- right arrow to accept
					accept_word = "<M-C-right>", -- ctrl + right arrow to accept word
					accept_line = "<M-C-l>", -- ctrl + l to accept line
					next = "<M-]>", -- Alt + ] for next suggestion
					prev = "<M-[>", -- Alt + [ for previous suggestion
					dismiss = "<C-]>", -- Ctrl + ] to dismiss
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
