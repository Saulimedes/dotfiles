local configs = require("nvim-treesitter.configs")
configs.setup {
  ensure_installed = {"c", "python", "perl", "lua", "css", "html", "rust", "yaml", "markdown", "toml", "bash", "fish", "jq" },
  sync_install = true,
  ignore_install = { "" }, -- List of parsers to ignore installing
  highlight = {
    enable = true, -- false will disable the whole extension
    disable = { "markdown" }, -- list of language that will be disabled
  },
  indent = { enable = true },
  rainbow = {
    enable = true,
    extended_mode = true, -- Also highlight non-bracket delimiters like html tags, boolean or table: lang -> boolean
    max_file_lines = 5000, -- Do not enable for files with more than n lines, int
  }
}

