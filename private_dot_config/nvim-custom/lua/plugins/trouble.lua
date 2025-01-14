return {
  {
    "folke/trouble.nvim",
    lazy = true,
    dependencies = {
      -- icons supported via mini-icons.lua

      {
        "nvim-lualine/lualine.nvim",
        opts = {
          extensions = { "trouble" },
        },
      },
    },
    opts = {},
    keys = require("config.keymaps").setup_trouble_keymaps(),
  },
}
