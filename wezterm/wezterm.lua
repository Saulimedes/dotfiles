local wezterm = require 'wezterm';

return {
  font = wezterm.font_with_fallback({"PragmataPro", "Regular"}),
  font_size = 12.0,
  hide_tab_bar_if_only_one_tab = true,
  scrollback_lines = "unlimited",
  adjust_window_size_when_changing_font_size = false,
  scrollbar = "never",
  default_prog = {"fish"},
  enable_tab_bar = true,
  enable_wayland = true,
  cursor_shape = "Beam",
  cursor_blink = "Always",
  window_padding = {
    left = 10,
    right = 10,
    top = 10,
    bottom = 10,
  },
  harfbuzz_features = {"kern", "liga"},
  check_for_updates = false,
  enable_scroll_bar = false,
  exit_behavior = "Close",
  enable_csi_u_key_encoding = true,

  mouse_bindings = {
    {
      event={Up={streak=1, button="Left", modifiers="CTRL"}},
      action="OpenLinkAtMouseCursor",
    },
  },
  keys = {
    {key="+", mods="CTRL", action="IncreaseFontSize"},
    {key="-", mods="CTRL", action="DecreaseFontSize"},
    {key="0", mods="CTRL", action="ResetFontSize"},
  },
  color_scheme = "Dracula",
  color_schemes = {
    ["Dracula"] = {
      foreground = "#F8F8F2",
      background = "#282A36",
      cursor_bg = "#F8F8F2",
      cursor_fg = "#F8F8F2",
      cursor_border = "#F8F8F2",
      ansi = {"#282A36", "#FF5555", "#50FA7B", "#F1FA8C", "#BD93F9", "#FF79C6", "#8BE9FD", "#F8F8F2"},
      brights = {"#4D4D4D", "#FF6E67", "#5AF78E", "#F4F99D", "#CAA9FA", "#FF92D0", "#9AEDFE", "#E6E6E6"},
    }
  },
}
