function fish_user_key_bindings
    # Ctrl+O: AI command suggestion/correction (opencode)
    bind \co _ai_toggle_widget

    # Ctrl+X Ctrl+E: edit command line in $EDITOR
    bind \cx\ce edit_command_buffer
end
