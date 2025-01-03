return {
  "https://codeberg.org/esensar/nvim-dev-container",
  dependencies = "nvim-treesitter/nvim-treesitter",
  keys = {
    {
      "<leader>cds",
      ":DevcontainerStart<cr>",
      desc = "Start the DevContainer",
    },
    {
      "<leader>cda",
      ":DevcontainerAttach<cr>",
      desc = "Attach DevContainer",
    },
    {
      "<leader>cdl",
      ":DevcontainerLogs<cr>",
      desc = "Logs of DevContainer",
    },
  },
}
