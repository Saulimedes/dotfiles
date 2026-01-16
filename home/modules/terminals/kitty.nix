# Kitty terminal configuration
{ config, pkgs, lib, ... }:

{
  programs.kitty = {
    enable = true;

    font = {
      name = "MonoLisaVariable Nerd Font";
      size = 10;
    };

    settings = {
      # Performance
      input_delay = 0;
      sync_to_monitor = "yes";
      repeat_rate = 25;

      # Ligatures
      disable_ligatures = "never";
      force_ltr = "yes";

      # Window
      remember_window_size = "yes";
      initial_window_width = 800;
      initial_window_height = 650;

      # Tab bar
      tab_bar_edge = "top";
      tab_bar_style = "separator";
      tab_separator = " |";
      tab_title_template = "{index}: {title}";
      active_tab_font_style = "bold";
      inactive_tab_font_style = "normal";

      # Misc
      enable_audio_bell = "no";
      wheel_scroll_min_lines = 1;
      open_url_with = "default";
      detect_urls = "yes";
      mouse_hide_wait = 5;
      cursor_trail = 1;
      cursor_trail_thickness = "0.33";

      # Wayland/X11
      linux_display_server = "auto";
      wayland_titlebar_color = "system";
      wayland_enable_ime = "yes";

      # Colors (Nord theme base)
      background = "#000000";
      foreground = "#D8DEE9";
      cursor = "#D8DEE9";
      selection_background = "#434C5E";
      selection_foreground = "#D8DEE9";

      # Normal colors
      color0 = "#3B4252";
      color1 = "#BF616A";
      color2 = "#A3BE8C";
      color3 = "#EBCB8B";
      color4 = "#81A1C1";
      color5 = "#B48EAD";
      color6 = "#88C0D0";
      color7 = "#E5E9F0";

      # Bright colors
      color8 = "#4C566A";
      color9 = "#BF616A";
      color10 = "#A3BE8C";
      color11 = "#EBCB8B";
      color12 = "#81A1C1";
      color13 = "#B48EAD";
      color14 = "#8FBCBB";
      color15 = "#ECEFF4";
    };

    keybindings = {
      "ctrl+shift+c" = "copy_to_clipboard";
      "ctrl+shift+v" = "paste_from_clipboard";
      "ctrl+shift+up" = "scroll_line_up";
      "ctrl+shift+down" = "scroll_line_down";
      "ctrl+shift+home" = "scroll_home";
      "ctrl+shift+end" = "scroll_end";
    };
  };
}
