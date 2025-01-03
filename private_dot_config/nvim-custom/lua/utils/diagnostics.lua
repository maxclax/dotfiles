M = {}

---@param diagnostic table
local function prefix(diagnostic)
  local icons = require("utils.icons").icons.diagnostics
  for d, icon in pairs(icons) do
    if diagnostic.severity == vim.diagnostic.severity[d:upper()] then
      return icon
    end
  end
end

function M.setup_diagnostics()
  local diagnostics = {
    underline = true,
    update_in_insert = false,
    virtual_text = {
      spacing = 4,
      source = "if_many",
      prefix = prefix,
    },
    severity_sort = true,
    signs = {
      text = {
        [vim.diagnostic.severity.ERROR] = require("utils.icons").icons.diagnostics.Error,
        [vim.diagnostic.severity.WARN] = require("utils.icons").icons.diagnostics.Warn,
        [vim.diagnostic.severity.HINT] = require("utils.icons").icons.diagnostics.Hint,
        [vim.diagnostic.severity.INFO] = require("utils.icons").icons.diagnostics.Info,
      },
    },
  }

  -- set diagnostic icons
  for name, icon in pairs(require("utils.icons").icons.diagnostics) do
    name = "DiagnosticSign" .. name
    vim.fn.sign_define(name, { text = icon, texthl = name, numhl = "" })
  end

  vim.diagnostic.config(vim.deepcopy(diagnostics))

  require("config.keymaps").setup_diagnostics_keymaps()
end

return M
