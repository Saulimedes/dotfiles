# Ghostty terminal configuration
# No programs.* module available yet, using home.file
{ config, pkgs, lib, ... }:

{
  # Install ghostty (if available in nixpkgs, otherwise use overlay or flake input)
  # home.packages = [ pkgs.ghostty ];

  # Ghostty configuration
  home.file.".config/ghostty/config".text = ''
    theme = dark:nordfox,light:nord-light
    background = #000000
    cursor-color = #eeeeee
    selection-background = 222222
    term = xterm-256color

    window-width = 120
    window-height = 45
    window-padding-x = 3
    shell-integration = fish
    shell-integration-features = no-cursor
    cursor-style-blink = true
    cursor-style = block
    adjust-underline-position = 40%
    adjust-underline-thickness = -60%
    font-family = MonoLisaVariable Nerd Font
    font-size = 11
    custom-shader = ~/.config/ghostty/shaders/cursor_tail.glsl

    window-step-resize = true
    clipboard-read = allow
    clipboard-write = allow
    auto-update = download
    copy-on-select = false
    clipboard-paste-protection = false
    mouse-hide-while-typing = true
    mouse-shift-capture = true
    mouse-scroll-multiplier = 1
    gtk-titlebar-hide-when-maximized = true

    keybind = performable:ctrl+shift+c=copy_to_clipboard
    keybind = performable:ctrl+shift+v=paste_from_clipboard
    keybind = performable:f11=toggle_fullscreen
  '';

  # Ghostty shaders (if you have custom shaders)
  # home.file.".config/ghostty/shaders" = {
  #   source = ../../../files/ghostty/shaders;
  #   recursive = true;
  # };
}
