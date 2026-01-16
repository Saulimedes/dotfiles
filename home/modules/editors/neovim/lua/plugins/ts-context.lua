-- ts_context_commentstring setup
require('ts_context_commentstring').setup({
  enable_autocmd = false,
  languages = {
    typescript = '// %s',
    css = '/* %s */',
    scss = '/* %s */',
    html = '<!-- %s -->',
    svelte = '<!-- %s -->',
    vue = '<!-- %s -->',
    json = '',
    lua = '-- %s',
    python = '# %s',
    rust = '// %s',
    javascript = {
      __default = '// %s',
      jsx_element = '{/* %s */}',
      jsx_fragment = '{/* %s */}',
      jsx_attribute = '/* %s */',
      comment = '// %s'
    },
  },
})

-- Setup Comment.nvim to use it
require('Comment').setup({
  pre_hook = require('ts_context_commentstring.integrations.comment_nvim').create_pre_hook(),
})