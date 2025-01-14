--- Anthropic config for CodeCompanion.
local anthropic_fn = function()
  local anthropic_config = {
    env = { api_key = "cmd:op read op://Personal/Anthropic/tokens/neovim --no-newline" },
  }
  return require("codecompanion.adapters").extend("anthropic", anthropic_config)
end

--- OpenAI config for CodeCompanion.
local openai_fn = function()
  local openai_config = {
    env = { api_key = "cmd:op read op://Personal/OpenAI/tokens/neovim --no-newline" },
  }
  return require("codecompanion.adapters").extend("openai", openai_config)
end

--- Gemini config for CodeCompanion.
local gemini_fn = function()
  local gemini_config = {
    env = { api_key = "cmd:op read op://Personal/Google/tokens/gemini --no-newline" },
    schema = {
      model = {
        default = "gemini-2.0-flash-exp",
      },
    },
  }
  return require("codecompanion.adapters").extend("gemini", gemini_config)
end

--- Ollama config for CodeCompanion.
local ollama_fn = function()
  return require("codecompanion.adapters").extend("ollama", {
    schema = {
      model = {
        default = "llama3.1:8b",
        -- default = "codellama:7b",
      },
      num_ctx = {
        default = 16384,
      },
      num_predict = {
        default = -1,
      },
    },
  })
end

local supported_adapters = {
  anthropic = anthropic_fn,
  openai = openai_fn,
  gemini = gemini_fn,
  ollama = ollama_fn,
}

local function save_path()
  local Path = require("plenary.path")
  local p = Path:new(vim.fn.stdpath("data") .. "/codecompanion_chats")
  p:mkdir({ parents = true })
  return p
end

--- Load a saved codecompanion.nvim chat file into a new CodeCompanion chat buffer.
--- Usage: CodeCompanionLoad
vim.api.nvim_create_user_command("CodeCompanionLoad", function()
  local fzf = require("fzf-lua")

  local function select_adapter(filepath)
    local adapters = vim.tbl_keys(supported_adapters)

    fzf.fzf_exec(adapters, {
      prompt = "Select CodeCompanion Adapter> ",
      actions = {
        ["default"] = function(selected)
          local adapter = selected[1]
          -- Open new CodeCompanion chat with selected adapter
          vim.cmd("CodeCompanionChat " .. adapter)

          -- Read contents of saved chat file
          local lines = vim.fn.readfile(filepath)

          -- Get the current buffer (which should be the new CodeCompanion chat)
          local current_buf = vim.api.nvim_get_current_buf()

          -- Paste contents into the new chat buffer
          vim.api.nvim_buf_set_lines(current_buf, 0, -1, false, lines)
        end,
      },
    })
  end

  local function start_picker()
    local files = vim.fn.glob(save_path() .. "/*", false, true)

    fzf.fzf_exec(files, {
      prompt = "Saved CodeCompanion Chats | <c-r>: remove >",
      previewer = "builtin",
      actions = {
        ["default"] = function(selected)
          if #selected > 0 then
            local filepath = selected[1]
            select_adapter(filepath)
          end
        end,
        ["ctrl-r"] = function(selected)
          if #selected > 0 then
            local filepath = selected[1]
            os.remove(filepath)
            -- Refresh the picker
            start_picker()
          end
        end,
      },
    })
  end

  start_picker()
end, {})

--- Save the current codecompanion.nvim chat buffer to a file in the save_folder.
--- Usage: CodeCompanionSave <filename>.md
---@param opts table
vim.api.nvim_create_user_command("CodeCompanionSave", function(opts)
  local codecompanion = require("codecompanion")
  local success, chat = pcall(function()
    return codecompanion.buf_get_chat(0)
  end)
  if not success or chat == nil then
    vim.notify("CodeCompanionSave should only be called from CodeCompanion chat buffers", vim.log.levels.ERROR)
    return
  end
  if #opts.fargs == 0 then
    vim.notify("CodeCompanionSave requires at least 1 arg to make a file name", vim.log.levels.ERROR)
  end
  local save_name = table.concat(opts.fargs, "-") .. ".md"
  local save_file = save_path():joinpath(save_name)
  local lines = vim.api.nvim_buf_get_lines(0, 0, -1, false)
  save_file:write(table.concat(lines, "\n"), "w")
end, { nargs = "*" })

return {
  {
    "olimorris/codecompanion.nvim",
    lazy = true,
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-treesitter/nvim-treesitter",
      "nvim-telescope/telescope.nvim",
      {
        "stevearc/dressing.nvim",
        opts = {},
      },
      {
        "saghen/blink.cmp",
        ---@module 'blink.cmp'
        ---@type blink.cmp.Config
        opts = {
          sources = {
            default = { "codecompanion" },
            providers = {
              codecompanion = {
                name = "CodeCompanion",
                module = "codecompanion.providers.completion.blink",
                enabled = true,
              },
            },
          },
        },
        opts_extend = {
          "sources.default",
        },
      },
    },
    opts = {

      opts = {
        send_code = function()
          return require("utils.private").is_ai_enabled()
        end,
      },

      adapters = supported_adapters,

      strategies = {
        chat = {
          adapter = "anthropic",
          slash_commands = {
            ["buffer"] = {
              opts = {
                provider = "fzf_lua", -- default|telescope|mini_pick|fzf_lua
              },
            },

            ["file"] = {
              opts = {
                provider = "fzf_lua", -- default|telescope|mini_pick|fzf_lua
              },
            },

            ["help"] = {
              opts = {
                provider = "fzf_lua", -- telescope|mini_pick|fzf_lua
              },
            },

            ["symbols"] = {
              opts = {
                provider = "fzf_lua", -- default|telescope|mini_pick|fzf_lua
              },
            },
          },
        },
        inline = {
          adapter = "copilot",
        },
        cmd = {
          adapter = "copilot",
        },
      },
      display = {
        chat = {
          show_settings = true,
        },
        action_palette = {
          provider = "default", -- default|telescope|mini_pick
        },
        diff = {
          provider = "default", -- default|mini_diff
        },
      },
      prompt_library = require("utils.llm_prompts").to_codecompanion(),
    },
    config = function(_, opts)
      require("codecompanion").setup(opts)
    end,
    keys = require("config.keymaps").setup_codecompanion_keymaps(),
  },
}