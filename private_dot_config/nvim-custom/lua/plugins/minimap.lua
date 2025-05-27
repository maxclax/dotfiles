return {
  ---@module "neominimap.config.meta"
  {
    "Isrothy/neominimap.nvim",
    version = "*",
    lazy = false,
    init = function()
      ---@type Neominimap.UserConfig
      vim.g.neominimap = {
        auto_enable = false,
      }
    end,
    cmd = { "Neominimap" },
    keys = require("config.keymaps").setup_minimap_keymaps(),
  },
}
