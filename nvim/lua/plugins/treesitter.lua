local configs = require("nvim-treesitter.configs")
configs.setup {
  -- Add a language of your choice
  ensure_installed = {"c", "python", "perl", "lua", "rust", "yaml", "markdown", "toml", "bash", "fish", "jq" },
  sync_install = false,
  ignore_install = { "" }, -- List of parsers to ignore installing
  highlight = {
    enable = true, -- false will disable the whole extension
    disable = { "markdown" }, -- list of language that will be disabled
    additional_vim_regex_highlighting = false,
  },
  indent = { enable = true, disable = { "yaml", "markdown" } },
  rainbow = {
    enable = true,
    extended_mode = true, -- Also highlight non-bracket delimiters like html tags, boolean or table: lang -> boolean
    max_file_lines = 1500, -- Do not enable for files with more than n lines, int
  }
}
