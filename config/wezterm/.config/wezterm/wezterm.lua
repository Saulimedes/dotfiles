local wezterm = require 'wezterm';
local act = wezterm.action

return {
  font = wezterm.font("IosevkaTerm Nerd Font", {weight="Regular"}),
  font_size = 12.0,
  hide_tab_bar_if_only_one_tab = true,
  hide_mouse_cursor_when_typing = true,
  scrollback_lines = 9999999,
  initial_cols = 140,
  initial_rows = 65,
  adjust_window_size_when_changing_font_size = false,
  cursor_blink_ease_in = "EaseIn",
  cursor_blink_ease_out = "EaseOut",
  force_reverse_video_cursor = true,
  color_scheme = 'Nord (Gogh)',
  enable_tab_bar = true,
  enable_wayland = true,
  pane_focus_follows_mouse = true,
  default_cursor_style = 'BlinkingBlock',
  automatically_reload_config = false,
  default_prog = { 'fish', '-l' },
  cursor_blink_rate = 700,
  text_blink_rate = 600,

  window_padding = {
    left = 10,
    right = 10,
    top = 10,
    bottom = 10,
  },
  harfbuzz_features = {"kern", "liga"},
  freetype_load_target = "HorizontalLcd",
  check_for_updates = false,
  enable_scroll_bar = false,
  exit_behavior = "Close",
  enable_csi_u_key_encoding = true,

  mouse_bindings = {
    {
      event={Up={streak=1, button="Middle"}},
      action=act.PasteFrom("PrimarySelection")
    },
    {
      event = { Down = { streak = 1, button = { WheelUp = 1 } } },
      mods = 'CTRL',
      action = act.IncreaseFontSize,
    },

    {
      event = { Down = { streak = 1, button = { WheelDown = 1 } } },
      mods = 'CTRL',
      action = act.DecreaseFontSize, },
  },

  quick_select_patterns = {
    '[0-9a-f]{7,40}',
    'https?://\\S+',
    'ftp://\\S+',
    'file://\\S+',
    'mailto:\\S+',
    'data:[^,;]+,[^,;]+',
    'www\\.\\S+',
  },

  keys = {
    {key="+", mods="CTRL", action="IncreaseFontSize"},
    {key="-", mods="CTRL", action="DecreaseFontSize"},
    {key="0", mods="CTRL", action="ResetFontSize"},
    {key="Backspace", mods="CTRL", action=wezterm.action{SendString="\x17"} },
	  {key = "V", mods = "CTRL|SHIFT", action = act.PasteFrom("Clipboard") },
	  {key = "C", mods = "CTRL|SHIFT", action = act.CopyTo("ClipboardAndPrimarySelection") },
    {key = "f", mods = "CMD", action = act.Search("CurrentSelectionOrEmptyString") },
	  {key = "PageDown", mods = "", action = act.ScrollByPage(0.8) },
	  {key = "PageUp", mods = "", action = act.ScrollByPage(-0.8) },
    {key="L", mods="CTRL|SHIFT", action="DisableDefaultAssignment" },
  }
}
