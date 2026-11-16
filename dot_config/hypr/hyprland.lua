local mainMod  = "SUPER"
local terminal = "kitty"
local browser  = "brave-origin"
local popup    = "emacs-popup"

hl.monitor({
    output   = "",
    mode     = "preferred",
    position = "auto",
    scale    = "auto",
})

hl.on("hyprland.start", function()
    hl.exec_cmd("hypr-theme apply-current")
    hl.exec_cmd("waybar")
    hl.exec_cmd("mako")
    hl.exec_cmd("hypridle")
    hl.exec_cmd("wl-clip-persist --clipboard regular")
    hl.exec_cmd("pa-applet")
end)

hl.env("XCURSOR_THEME", "Bibata-Modern-Classic")
hl.env("XCURSOR_SIZE", "24")
hl.env("HYPRCURSOR_SIZE", "24")

hl.config({
    general = {
        gaps_in     = 3,
        gaps_out    = 6,
        border_size = 2,
        col = {
            active_border   = "rgba(7aa2f7ff)",
            inactive_border = "rgba(3b4261aa)",
        },
        resize_on_border = true,
        allow_tearing    = false,
        layout           = "master",
    },

    decoration = {
        rounding = 0,
        shadow   = { enabled = false },
        blur     = { enabled = false },
    },

    animations = {
        enabled = false,
    },

    master = {
        new_status      = "slave",
        mfact            = 0.55,
        orientation      = "left",
    },

    misc = {
        disable_hyprland_logo    = true,
        disable_splash_rendering = true,
        background_color         = 0xff000000,
        focus_on_activate        = true,
    },
})

-- ~/.config/xkb/symbols: us-semi (";" is a dead diaeresis key), us-de (direct ä ö ü ß); Alt+Shift toggles
hl.config({
    input = {
        kb_layout  = "us-semi,us-de",
        kb_variant = "",
        kb_model   = "",
        kb_options = "ctrl:nocaps,grp:alt_shift_toggle",
        kb_rules   = "",

        repeat_rate  = 40,
        repeat_delay = 250,

        follow_mouse = 1,
        sensitivity  = 0,

        touchpad = {
            natural_scroll       = false,
            tap_to_click         = true,
            disable_while_typing = true,
        },
    },
})

hl.gesture({
    fingers   = 3,
    direction = "horizontal",
    action    = "workspace",
})

local function emacs(kind)
    return hl.dsp.exec_cmd(popup .. " " .. kind)
end

hl.bind(mainMod .. " + Space",          emacs("launcher"))
hl.bind(mainMod .. " + Tab",            emacs("windows"))
hl.bind(mainMod .. " + C",              emacs("capture"))
hl.bind(mainMod .. " + SHIFT + Escape", emacs("power"))
hl.bind(mainMod .. " + E",              emacs("frame"))
hl.bind(mainMod .. " + SHIFT + E",      emacs("dired"))
hl.bind(mainMod .. " + SHIFT + Return", emacs("vterm"))

hl.bind(mainMod .. " + Return", hl.dsp.exec_cmd(terminal))
hl.bind(mainMod .. " + B",      hl.dsp.exec_cmd(browser))
hl.bind(mainMod .. " + Escape", hl.dsp.exec_cmd("loginctl lock-session"))

hl.bind(mainMod .. " + Q",         hl.dsp.window.close())
hl.bind(mainMod .. " + F",         hl.dsp.window.fullscreen({ mode = "fullscreen" }))
hl.bind(mainMod .. " + SHIFT + F", hl.dsp.window.float({ action = "toggle" }))
hl.bind(mainMod .. " + P",         hl.dsp.window.pseudo())
hl.bind(mainMod .. " + T",         hl.dsp.layout("togglesplit"))

-- master layout (dwm-style master/stack)
hl.bind(mainMod .. " + M",            hl.dsp.layout("cyclenext"))
hl.bind(mainMod .. " + SHIFT + M",    hl.dsp.layout("swapwithmaster"))
hl.bind(mainMod .. " + bracketleft",  hl.dsp.layout("mfact -0.05"))
hl.bind(mainMod .. " + bracketright", hl.dsp.layout("mfact +0.05"))

-- theming: cycle border/waybar/mako palette
hl.bind(mainMod .. " + SHIFT + T", hl.dsp.exec_cmd("hypr-theme next"))

local dirs = {
    { key = "left",  vim = "H", dir = "left"  },
    { key = "down",  vim = "J", dir = "down"  },
    { key = "up",    vim = "K", dir = "up"    },
    { key = "right", vim = "L", dir = "right" },
}
for _, d in ipairs(dirs) do
    hl.bind(mainMod .. " + " .. d.key,         hl.dsp.focus({ direction = d.dir }))
    hl.bind(mainMod .. " + " .. d.vim,         hl.dsp.focus({ direction = d.dir }))
    hl.bind(mainMod .. " + SHIFT + " .. d.key, hl.dsp.window.move({ direction = d.dir }))
    hl.bind(mainMod .. " + SHIFT + " .. d.vim, hl.dsp.window.move({ direction = d.dir }))
end

for i = 1, 10 do
    local key = i % 10
    hl.bind(mainMod .. " + " .. key,         hl.dsp.focus({ workspace = i }))
    hl.bind(mainMod .. " + SHIFT + " .. key, hl.dsp.window.move({ workspace = i }))
end

hl.bind(mainMod .. " + S",         hl.dsp.workspace.toggle_special("scratch"))
hl.bind(mainMod .. " + SHIFT + S", hl.dsp.window.move({ workspace = "special:scratch" }))

hl.bind(mainMod .. " + mouse_down", hl.dsp.focus({ workspace = "e+1" }))
hl.bind(mainMod .. " + mouse_up",   hl.dsp.focus({ workspace = "e-1" }))
hl.bind(mainMod .. " + mouse:272",  hl.dsp.window.drag(),   { mouse = true })
hl.bind(mainMod .. " + mouse:273",  hl.dsp.window.resize(), { mouse = true })

hl.bind("Print",         hl.dsp.exec_cmd([[grim -g "$(slurp)" - | wl-copy]]))
hl.bind("SHIFT + Print", hl.dsp.exec_cmd([[mkdir -p ~/Pictures && grim ~/Pictures/screenshot-$(date +%F_%H-%M-%S).png]]))

hl.bind("XF86AudioRaiseVolume",  hl.dsp.exec_cmd("wpctl set-volume -l 1 @DEFAULT_AUDIO_SINK@ 5%+"), { locked = true, repeating = true })
hl.bind("XF86AudioLowerVolume",  hl.dsp.exec_cmd("wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-"),      { locked = true, repeating = true })
hl.bind("XF86AudioMute",         hl.dsp.exec_cmd("wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"),     { locked = true, repeating = true })
hl.bind("XF86AudioMicMute",      hl.dsp.exec_cmd("wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle"),   { locked = true, repeating = true })
hl.bind("XF86MonBrightnessUp",   hl.dsp.exec_cmd("brightnessctl -e4 -n2 set 5%+"),                  { locked = true, repeating = true })
hl.bind("XF86MonBrightnessDown", hl.dsp.exec_cmd("brightnessctl -e4 -n2 set 5%-"),                  { locked = true, repeating = true })
hl.bind("XF86AudioNext",         hl.dsp.exec_cmd("playerctl next"),       { locked = true })
hl.bind("XF86AudioPause",        hl.dsp.exec_cmd("playerctl play-pause"), { locked = true })
hl.bind("XF86AudioPlay",         hl.dsp.exec_cmd("playerctl play-pause"), { locked = true })
hl.bind("XF86AudioPrev",         hl.dsp.exec_cmd("playerctl previous"),   { locked = true })

hl.window_rule({
    name  = "emacs-popups",
    match = { initial_title = "^emacs-(launcher|windows|capture|power)$" },
    float  = true,
    center = true,
})

hl.window_rule({
    name  = "float-utilities",
    match = { class = "^(nm-connection-editor|\\.blueman-manager-wrapped|org\\.pulseaudio\\.pavucontrol)$" },
    float  = true,
    center = true,
})

hl.window_rule({
    name  = "float-dialogs",
    match = { initial_title = "^(Open File|Save File|Save As|Open Folder|File Operation Progress)$" },
    float  = true,
    center = true,
})

hl.window_rule({
    name           = "suppress-maximize-events",
    match          = { class = ".*" },
    suppress_event = "maximize",
})
