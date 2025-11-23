local windline = require('windline')
local helper = require('windline.helpers')
local b_components = require('windline.components.basic')
local state = _G.WindLine.state
local lsp_comps = require('windline.components.lsp')
local git_comps = require('windline.components.git')

-- Git Blame setup with your specific configuration
local git_blame = require('gitblame')
vim.g.gitblame_display_virtual_text = 0
vim.g.gitblame_message_when_not_committed = ''
vim.g.gitblame_date_format = '%m/%d'
vim.g.gitblame_message_template = '  <date> <author> ∙ <summary> '

-- Define the Git Blame component with white text
local git_blame_component = {
    text = function()
        if git_blame.is_blame_text_available() then
            return git_blame.get_current_blame_text()
        end
        return ''
    end,
    hl_colors = { 'white', 'black' },  -- Git blame text color set to white
}


local hl_list = {
    Black = { 'white', 'black' },
    White = { 'black', 'white' },
    Inactive = { 'InactiveFg', 'InactiveBg' },
    Active = { 'ActiveFg', 'ActiveBg' },
}

local basic = {}
local breakpoint_width = 90
basic.divider = { b_components.divider, '' }
basic.bg = { ' ', 'StatusLine' }

local colors_mode = {
    Normal = { 'blue', 'black' },
    Insert = { 'green', 'black' },
    Visual = { 'yellow', 'black' },
    Replace = { 'blue_light', 'black' },
    Command = { 'red', 'black' },
}

-- Check if file is modified to change the color of the second square_mode
local function get_square_mode_color()
    if vim.bo.modified then
        return {'magenta', 'black'}
    end
    return state.mode[2]
end

basic.vi_mode = {
    name = 'vi_mode',
    hl_colors = colors_mode,
    text = function()
        return { { '', state.mode[2] } }
    end,
}

basic.square_mode = {
    hl_colors = colors_mode,
    text = function()
        return { { '▎', state.mode[2] } }
    end,
}

-- Modified square mode for right side
basic.modified_square_mode = {
    hl_colors = colors_mode,
    text = function()
        return { { '▕', get_square_mode_color() } }
    end,
}

basic.lsp_diagnos = {
    name = 'diagnostic',
    hl_colors = {
        red = { 'red', 'black' },
        yellow = { 'yellow', 'black' },
        blue = { 'blue', 'black' },
    },
    width = breakpoint_width,
    text = function(bufnr)
        if lsp_comps.check_lsp(bufnr) then
            local error = lsp_comps.lsp_error({ format = '  %s', show_zero = false })
            local warning = lsp_comps.lsp_warning({ format = '  %s', show_zero = false })
            local hint = lsp_comps.lsp_hint({ format = '  %s', show_zero = false })
            return {
                { error ~= '' and error or '', 'red' },
                { warning ~= '' and warning or '', 'yellow' },
                { hint ~= '' and hint or '', 'blue' },
            }
        end
        return ''
    end,
}

basic.file = {
    name = 'file',
    hl_colors = {
        white = { 'white', 'black' },
        blue = { 'blue', 'black' },
    },
    text = function(_, _, width)
        if width > breakpoint_width then
            return {
                { b_components.cache_file_name('[No Name]', 'unique'), 'blue' },
                { b_components.line_col_lua, 'white' },
                { b_components.progress, '' }, -- Classical Neovim progress with reduced spacing
            }
        else
            return {
                { b_components.cache_file_name('[No Name]', 'unique'), 'blue' },
                { ' ', '' },
            }
        end
    end,
}

basic.file_right = {
    hl_colors = {
        white = { 'white', 'black' },
        magenta = { 'magenta', 'black' },
    },
    text = function(_, _, width)
        if width < breakpoint_width then
            return {
                { b_components.line_col_lua, 'white' },
                { b_components.progress, '' },  -- Classical Neovim progress with reduced spacing
            }
        end
    end,
}

basic.git = {
    name = 'git',
    hl_colors = {
        green = { 'green', 'black' },
        red = { 'red', 'black' },
        blue = { 'blue', 'black' },
    },
    width = breakpoint_width,
    text = function(bufnr)
        if git_comps.is_git(bufnr) then
            local added = git_comps.diff_added({ format = '  %s', show_zero = false })
            local removed = git_comps.diff_removed({ format = '  %s', show_zero = false })
            local changed = git_comps.diff_changed({ format = '  %s', show_zero = false })
            return {
                { added ~= '' and added or '', 'green' },
                { removed ~= '' and removed or '', 'red' },
                { changed ~= '' and changed or '', 'blue' },
            }
        end
        return ''
    end,
}

basic.lsp_name = {
    width = breakpoint_width,
    name = 'lsp_name',
    hl_colors = {
        magenta = { 'magenta', 'black' },
    },
    text = function(bufnr)
        if lsp_comps.check_lsp(bufnr) then
            return {
                { lsp_comps.lsp_name(), 'magenta' },
            }
        end
        return {
            { b_components.cache_file_type({icon = true}), 'magenta' },
        }
    end,
}

-- Statusline configuration
local default = {
    filetypes = { 'default' },
    active = {
        basic.square_mode,
        basic.vi_mode,
        basic.file,
        git_blame_component,  -- Git blame added here with white color
        basic.divider,
        basic.lsp_name,
        basic.git,
        basic.lsp_diagnos,
        { git_comps.git_branch(), { 'white', 'black' }, breakpoint_width },
        { ' ', hl_list.Black },
        basic.modified_square_mode, -- Use modified square mode on the right
    },
    inactive = {
        { b_components.full_file_name, hl_list.Inactive },
        basic.divider,
        { b_components.line_col, hl_list.Inactive },
        { b_components.progress, hl_list.Inactive },
    },
    always_active = false,
    show_last_status = false,
}

-- Quickfix and Explorer configurations as before
local quickfix = {
    filetypes = { 'qf', 'Trouble' },
    active = {
        { '🚦 Quickfix ', { 'white', 'black' } },
        { helper.separators.slant_right, { 'black', 'black_light' } },
        {
            function()
                return vim.fn.getqflist({ title = 0 }).title
            end,
            { 'cyan', 'black_light' },
        },
        { ' Total : %L ', { 'cyan', 'black_light' } },
        { helper.separators.slant_right, { 'black_light', 'InactiveBg' } },
        { ' ', { 'InactiveFg', 'InactiveBg' } },
        basic.divider,
        { helper.separators.slant_right, { 'InactiveBg', 'black' } },
        { '🧛 ', { 'white', 'black' } },
    },
    always_active = true,
    show_last_status = true,
}

local explorer = {
    filetypes = { 'fern', 'NvimTree', 'lir' },
    active = {
        { '  ', { 'black', 'red' } },
        { helper.separators.slant_right, { 'red', 'NormalBg' } },
        { b_components.divider, '' },
        { b_components.file_name(''), { 'white', 'NormalBg' } },
    },
    always_active = true,
    show_last_status = true,
}

windline.setup({
    colors_name = function(colors)
        -- Use Nord colorscheme colors automatically
        return colors
    end,
    statuslines = {
        default,
        quickfix,
        explorer,
    },
})

