return {
  "nvim-neo-tree/neo-tree.nvim",
  lazy = true,
  dependencies = {
    "nvim-lua/plenary.nvim",
    -- icons supported via mini-icons.lua
    { "echasnovski/mini.icons", version = false },
    "MunifTanjim/nui.nvim",
    {
      "nvim-lualine/lualine.nvim",
      opts = {
        extensions = { "neo-tree" },
      },
    },
  },
  opts = {
    sources = { "filesystem", "buffers", "git_status", "document_symbols" },
    open_files_do_not_replace_types = { "terminal", "Trouble", "trouble", "qf", "Outline" },
    filesystem = {
      bind_to_cwd = true,
      follow_current_file = { enabled = true },

      -- This will use the OS level file watchers to detect changes
      -- instead of relying on nvim autocmd events.
      use_libuv_file_watcher = true,

      filtered_items = {
        visible = true, -- when true, they will just be displayed differently than normal items
        hide_dotfiles = false,
        hide_gitignored = true,
        hide_by_name = { ".git" },
        never_show = { -- remains hidden even if visible is toggled to true, this overrides always_show
          ".DS_Store",
          "thumbs.db",
        },
      },
    },
    window = {
      mappings = {
        ["l"] = "open",
        ["h"] = "close_node",
        ["<space>"] = "none",
        ["Y"] = {
          function(state)
            local node = state.tree:get_node()
            local path = node:get_id()
            vim.fn.setreg("+", path, "c")
          end,
          desc = "Copy Path to Clipboard",
        },
        ["O"] = {
          function(state)
            require("lazy.util").open(state.tree:get_node().path, { system = true })
          end,
          desc = "Open with System Application",
        },
        ["P"] = { "toggle_preview", config = { use_float = false } },
      },
    },
  },
  keys = require("config.keymaps").setup_neotree_keymaps(),
  cmd = { "Neotree" },
}
