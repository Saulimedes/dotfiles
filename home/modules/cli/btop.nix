# btop - system monitor
# Colors are automatically managed by Stylix
{ config, pkgs, lib, ... }:

{
  programs.btop = {
    enable = true;

    settings = {
      # color_theme is set automatically by Stylix

      # UI settings
      theme_background = true;
      truecolor = true;
      force_tty = false;
      vim_keys = true;

      # Display
      rounded_corners = true;
      graph_symbol = "braille";  # braille, block, tty
      shown_boxes = "cpu mem net proc";
      update_ms = 1000;

      # Process settings
      proc_sorting = "cpu lazy";
      proc_reversed = false;
      proc_tree = false;
      proc_colors = true;
      proc_gradient = true;
      proc_per_core = true;

      # CPU settings
      cpu_graph_upper = "total";
      cpu_graph_lower = "total";
      cpu_invert_lower = true;
      cpu_single_graph = false;

      # Memory settings
      mem_graphs = true;
      mem_below_net = false;
      swap_disk = true;
      show_disks = true;

      # Network settings
      net_download = 100;
      net_upload = 100;
      net_auto = true;
      net_sync = false;
    };
  };
}
