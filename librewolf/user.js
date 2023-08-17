user_pref("toolkit.legacyUserProfileCustomizations.stylesheets", true);
user_pref("browser.download.useDownloadDir", false);
user_pref("browser.download.alwaysOpenPanel", false);
user_pref("browser.download.manager.addToRecentDocs", false);
user_pref("browser.download.always_ask_before_handling_new_types", true);
user_pref("browser.tabs.loadBookmarksInTabs", true);
user_pref("browser.bookmarks.openInTabClosesMenu", false);
user_pref("browser.toolbars.bookmarks.visibility", "never");
user_pref("layout.css.has-selector.enabled", true);
user_pref("geo.provider.network.url", "https://location.services.mozilla.com/v1/geolocate?key=%MOZILLA_API_KEY%");
user_pref("permissions.default.geo", 2);
user_pref("nglayout.initialpaint.delay", 0); // default=5; used to be 250
user_pref("nglayout.initialpaint.delay_in_oopif", 0); // default=5
user_pref("content.notify.interval", 100000); // (.10s); alt=500000 (.50s)
user_pref("layout.css.animation-composition.enabled", true);
user_pref("gfx.webrender.all", true); // enables WR (GPU) + additional features
user_pref("gfx.webrender.precache-shaders", true);
user_pref("gfx.webrender.compositor", true);
user_pref("layers.gpu-process.enabled", true);
user_pref("media.hardware-video-decoding.enabled", true);
user_pref("browser.cache.memory.max_entry_size", 153600); 
user_pref("network.buffer.cache.size", 262144); // preferred=327680; default=32768
user_pref("network.buffer.cache.count", 128); // preferred=240; default=24
user_pref("network.http.max-connections", 1800); // default=900
user_pref("network.http.max-persistent-connections-per-server", 10); // default=6; download connections; anything above 10 is excessive

user_pref("general.smoothScroll",                                       true); // DEFAULT
user_pref("general.smoothScroll.msdPhysics.continuousMotionMaxDeltaMS", 12);
user_pref("general.smoothScroll.msdPhysics.enabled",                    true);
user_pref("general.smoothScroll.msdPhysics.motionBeginSpringConstant",  600);
user_pref("general.smoothScroll.msdPhysics.regularSpringConstant",      650);
user_pref("general.smoothScroll.msdPhysics.slowdownMinDeltaMS",         25);
user_pref("general.smoothScroll.msdPhysics.slowdownMinDeltaRatio",      2.0);
user_pref("general.smoothScroll.msdPhysics.slowdownSpringConstant",     250);
user_pref("general.smoothScroll.currentVelocityWeighting",              1.0);
user_pref("general.smoothScroll.stopDecelerationWeighting",             1.0);
user_pref("mousewheel.default.delta_multiplier_y",                      300); // 250-400
