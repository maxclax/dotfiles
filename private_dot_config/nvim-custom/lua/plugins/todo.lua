return {
  {
    "folke/todo-comments.nvim",
    lazy = true,
    event = "VeryLazy",
    dependencies = { "nvim-lua/plenary.nvim" },
    opts = {},
    -- NOTE: keymaps through Snacks.picker
    -- keys = require("config.keymaps").setup_todo_keymaps(),
  },
}
