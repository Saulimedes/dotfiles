-- sbs-toggle.lua
-- Ctrl+e: save & clear vf filters (crop), enable mpv360
-- Ctrl+e again: disable mpv360, restore vf filters (crop)

local saved_vf = nil
local vr_active = false

mp.register_event("file-loaded", function()
    vr_active = false
    saved_vf = nil
end)

local function toggle()
    if vr_active then
        mp.command("script-binding mpv360/toggle")
        if saved_vf then
            mp.set_property_native("vf", saved_vf)
        end
        vr_active = false
    else
        saved_vf = mp.get_property_native("vf")
        mp.set_property_native("vf", {})
        mp.command("script-binding mpv360/toggle")
        vr_active = true
    end
end

mp.add_forced_key_binding("Ctrl+e", "toggle-vr", toggle)
