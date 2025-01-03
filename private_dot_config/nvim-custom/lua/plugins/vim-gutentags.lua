return {
	"ludovicchabant/vim-gutentags",
	event = { "BufReadPost", "BufNewFile" },
	config = function()
		-- Define where to store tag files
		vim.g.gutentags_cache_dir = vim.fn.expand("~/.cache/nvim/ctags/")

		-- Use universal-ctags if available
		vim.g.gutentags_ctags_executable = "ctags"

		-- Generate dataTags files
		vim.g.gutentags_generate_on_write = 1
		vim.g.gutentags_generate_on_new = 1
		vim.g.gutentags_generate_on_missing = 1

		-- Define which files to ignore
		vim.g.gutentags_ctags_exclude = {
			"*.git",
			"*.svg",
			"*.hg",
			"*/tests/*",
			"build",
			"dist",
			"*sites/*/files/*",
			"bin",
			"node_modules",
			"bower_components",
			"cache",
			"compiled",
			"docs",
			"example",
			"bundle",
			"vendor",
			"*.md",
			"*-lock.json",
			"*.lock",
			"*bundle*.js",
			"*build*.js",
			".*rc*",
			"*.json",
			"*.min.*",
			"*.map",
			"*.bak",
			"*.zip",
			"*.pyc",
			"*.class",
			"*.sln",
			"*.Master",
			"*.csproj",
			"*.tmp",
			"*.csproj.user",
			"*.cache",
			"*.pdb",
			"tags*",
			"cscope.*",
			"__pycache__",
		}

		-- File types to include
		vim.g.gutentags_ctags_extra_args = {
			"--tag-relative=yes",
			"--fields=+ailmnS",
		}

		-- Create cache directory if it doesn't exist
		local cache_dir = vim.fn.expand("~/.cache/nvim/ctags")
		if vim.fn.isdirectory(cache_dir) == 0 then
			vim.fn.mkdir(cache_dir, "p")
		end
	end,
}
