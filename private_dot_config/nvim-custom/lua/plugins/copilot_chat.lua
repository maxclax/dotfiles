return {

	{
		"CopilotC-Nvim/CopilotChat.nvim",
		lazy = true,
		event = "VeryLazy",
		version = "*",
		dependencies = {
			{ "zbirenbaum/copilot.lua" },
			{ "nvim-lua/plenary.nvim" },
		},
		---@type CopilotChat.config
		opts = {
			debug = false, -- Enable debugging
			model = "claude-3.7-sonnet",
			prompts = require("utils.llm_prompts").to_copilot(),
		},
		keys = function()
			local chat = require("CopilotChat")
			return require("config.keymaps").setup_copilot_chat_keymaps(chat)
		end,
	},
}
