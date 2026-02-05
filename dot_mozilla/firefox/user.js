// =============================================================================
// DISABLE AI FEATURES
// =============================================================================
user_pref("browser.ml.enable", false);
user_pref("browser.ai.control.default", "blocked");
user_pref("browser.ai.control.sidebarChatbot", "blocked");
user_pref("browser.ai.control.linkPreviewKeyPoints", "blocked");
user_pref("browser.ai.control.smartTabGroups", "blocked");
user_pref("browser.ai.control.translations", "blocked");
user_pref("browser.ai.control.pdfjsAltText", "blocked");
user_pref("browser.ml.chat.enabled", false);
user_pref("browser.ml.chat.sidebar", false);
user_pref("browser.ml.chat.shortcuts", false);
user_pref("browser.ml.chat.menu", false);
user_pref("browser.ml.chat.page", false);
user_pref("browser.ml.chat.page.footerBadge", false);
user_pref("browser.ml.chat.page.menuBadge", false);
user_pref("browser.ml.linkPreview.enabled", false);
user_pref("browser.ml.pageAssist.enabled", false);
user_pref("browser.ml.smartAssist.enabled", false);
user_pref("browser.tabs.groups.smart.enabled", false);
user_pref("browser.tabs.groups.smart.userEnabled", false);
user_pref("extensions.ml.enabled", false);
user_pref("pdfjs.enableAltTextModelDownload", false);
user_pref("pdfjs.enableGuessAltText", false);

// Disable translations
user_pref("browser.translations.enable", false);

// Disable screenshots
user_pref("extensions.screenshots.disabled", true);
user_pref("extensions.screenshots.upload-disabled", true);
user_pref("screenshots.browser.component.enabled", false);

// Disable PiP entirely
user_pref("media.videocontrols.picture-in-picture.enabled", false);

// Disable web compat reporter
user_pref("extensions.webcompat-reporter.enabled", false);

// =============================================================================
// PERFORMANCE & NETWORK
// =============================================================================
user_pref("browser.sessionstore.restore_pinned_tabs_on_demand", true);
user_pref("network.http.speculative-parallel-limit", 0);
user_pref("browser.urlbar.speculativeConnect.enabled", true);
user_pref("browser.places.speculativeConnect.enabled", true);
user_pref("network.prefetch-next", true);
user_pref("network.http.max-persistent-connections-per-server", 20); // increase download connections
user_pref("network.http.max-connections", 1800);
user_pref("network.http.max-urgent-start-excessive-connections-per-host", 5);
user_pref("network.http.pacing.requests.enabled", false);
user_pref("network.ssl_tokens_cache_capacity", 10240); // SSL session cache
user_pref("privacy.trackingprotection.allow_list.convenience.enabled", false); // disable Strict allowlist of convenience features
user_pref("signon.rememberSignons", false); // disable password manager
user_pref("extensions.formautofill.addresses.enabled", false); // disable address manager
user_pref("extensions.formautofill.creditCards.enabled", false); // disable credit card manager
user_pref("browser.urlbar.suggest.recentsearches", false);  // unselect "Show recent searches" for clean UI
user_pref("browser.urlbar.showSearchSuggestionsFirst", false); // unselect "Show search suggestions ahead of browsing history in address bar results" for clean UI
user_pref("signon.management.page.breach-alerts.enabled", false); // extra hardening
user_pref("signon.generation.enabled", false); // unselect "Suggest and generate strong passwords" for clean UI
user_pref("signon.firefoxRelay.feature", ""); // unselect suggestions from Firefox Relay for clean UI
user_pref("browser.safebrowsing.downloads.enabled", false); // deny SB to scan downloads to identify suspicious files; local checks only
user_pref("browser.safebrowsing.downloads.remote.url", ""); // enforce no remote checks for downloads by SB
user_pref("browser.safebrowsing.downloads.remote.block_potentially_unwanted", false); // clean up UI; not needed in user.js if remote downloads are disabled
user_pref("browser.safebrowsing.downloads.remote.block_uncommon", false); // clean up UI; not needed in user.js if remote downloads are disabled
user_pref("browser.safebrowsing.allowOverride", false); // do not allow user to override SB
user_pref("browser.search.update", false); // do not update opensearch engines
user_pref("network.trr.confirmationNS", "skip"); // skip TRR confirmation request
user_pref("extensions.webextensions.restrictedDomains", ""); // remove Mozilla domains so adblocker works on pages
user_pref("browser.firefox-view.feature-tour", "{\"screen\":\"\",\"complete\":true}"); // disable the Firefox View tour from popping up for new profiles
user_pref("accessibility.force_disabled", 1); // disable Accessibility features
user_pref("security.cert_pinning.enforcement_level", 2); // strict public key pinning
user_pref("captivedetect.canonicalURL", ""); // disable captive portal detection
user_pref("network.captive-portal-service.enabled", false); // disable captive portal detection
user_pref("network.connectivity-service.enabled", false); // disable captive portal detection
user_pref("browser.download.enableDeletePrivate", true); // Delete files downloaded in private browsing when all private windows are closed
user_pref("browser.download.deletePrivateChosen", true); // Delete files downloaded in private browsing when all private windows are closed
user_pref("browser.download.deletePrivate", true); // Delete files downloaded in private browsing when all private windows are closed
user_pref("devtools.accessibility.enabled", false); // removes un-needed "Inspect Accessibility Properties" on right-click
user_pref("browser.newtabpage.activity-stream.showSponsoredTopSites", false); // Settings>Home>Firefox Home Content>Recent Activity>Shortcuts>Sponsored shortcuts
user_pref("browser.newtabpage.activity-stream.showSponsored", false); // Settings>Home>Firefox Home Content>Recent Activity>Recommended by Pocket>Sponsored Stories
user_pref("browser.newtabpage.activity-stream.section.highlights.includeBookmarks", false); // Settings>Home>Firefox Home Content>Recent Activity>Bookmarks
user_pref("browser.newtabpage.activity-stream.section.highlights.includeDownloads", false); // Settings>Home>Firefox Home Content>Recent Activity>Most Recent Download
user_pref("browser.newtabpage.activity-stream.section.highlights.includeVisited", false); // Settings>Home>Firefox Home Content>Recent Activity>Visited Pages
user_pref("browser.newtabpage.activity-stream.section.highlights.includePocket", false); // Settings>Home>Firefox Home Content>Recent Activity>Pages Saved to Pocket
user_pref("browser.toolbars.bookmarks.visibility", "never"); // always hide bookmark bar
user_pref("browser.startup.homepage_override.mstone", "ignore"); // What's New page after updates; master switch
user_pref("browser.urlbar.suggest.history", false); // Browsing history; hide URL bar dropdown suggestions
user_pref("browser.urlbar.suggest.bookmark", false); // Bookmarks; hide URL bar dropdown suggestions
user_pref("browser.urlbar.suggest.openpage", false); // Open tabs; hide URL bar dropdown suggestions
user_pref("browser.urlbar.suggest.topsites", false); // Shortcuts; disable dropdown suggestions with empty query
user_pref("browser.urlbar.suggest.engines", false); // Search engines; tab-to-search
user_pref("browser.urlbar.quicksuggest.enabled", false); // hide Firefox Suggest UI in the settings
user_pref("browser.bookmarks.max_backups", 0); // minimize disk use; manually back-up
user_pref("view_source.wrap_long_lines", true);  // wrap source lines
user_pref("devtools.debugger.ui.editor-wrapping", true);  // wrap lines in devtools
user_pref("browser.zoom.full", false); // text-only zoom, not all elements on page
user_pref("layout.word_select.eat_space_to_next_word", false); // do not select the space next to a word when selecting a word
user_pref("browser.tabs.loadBookmarksInTabs", true); // force bookmarks to open in a new tab, not the current tab
user_pref("ui.key.menuAccessKey", 0); // remove underlined characters from various settings
user_pref("general.autoScroll", false); // disable unintentional behavior for middle click
user_pref("ui.SpellCheckerUnderlineStyle", 1); // [HIDDEN] dots for spell check errors
user_pref("reader.parse-on-load.enabled", false); // disable reader mode
user_pref("network.trr.mode", 2); // enable TRR (with System fallback)
user_pref("network.trr.max-fails", 5); // lower max attempts to use DoH
user_pref("geo.provider.use_geoclue", false); // [LINUX]
user_pref("pdfjs.defaultZoomValue", "page-width"); // PDF zoom level
// =============================================================================
// SIDEBAR & VERTICAL TABS
// =============================================================================
user_pref("sidebar.revamp", true);
user_pref("sidebar.visibility", "hide-sidebar");
user_pref("sidebar.main.tools", "history");
user_pref("sidebar.verticalTabs", true);

// =============================================================================
// PRIVACY EXTRAS
// =============================================================================
// Disable Pocket
user_pref("extensions.pocket.enabled", false);

// Disable telemetry
user_pref("toolkit.telemetry.enabled", false);
user_pref("toolkit.telemetry.unified", false);
user_pref("toolkit.telemetry.archive.enabled", false);
user_pref("datareporting.healthreport.uploadEnabled", false);
user_pref("datareporting.policy.dataSubmissionEnabled", false);
user_pref("browser.ping-centre.telemetry", false);
user_pref("browser.newtabpage.activity-stream.feeds.telemetry", false);
user_pref("browser.newtabpage.activity-stream.telemetry", false);

// Disable studies and experiments
user_pref("app.shield.optoutstudies.enabled", false);
user_pref("app.normandy.enabled", false);
user_pref("app.normandy.api_url", "");

// Disable crash reporter sending
user_pref("breakpad.reportURL", "");
user_pref("browser.tabs.crashReporting.sendReport", false);
user_pref("browser.crashReports.unsubmittedCheck.autoSubmit2", false);

// Disable extension recommendations
user_pref("browser.messaging-system.whatsNewPanel.enabled", false);
user_pref("extensions.htmlaboutaddons.recommendations.enabled", false);
user_pref("browser.urlbar.suggest.quicksuggest.sponsored", false);

// Enable userChrome.css
user_pref("toolkit.legacyUserProfileCustomizations.stylesheets", true);
user_pref("svg.context-properties.content.enabled", true);
user_pref("layout.css.backdrop-filter.enabled", true);

// GPU acceleration
user_pref("layers.acceleration.force-enabled", true);
user_pref("gfx.webrender.all", true);
user_pref("gfx.webrender.enabled", true);
user_pref("gfx.canvas.accelerated", true);
user_pref("gfx.canvas.accelerated.cache-items", 4096);
user_pref("gfx.canvas.accelerated.cache-size", 512);
user_pref("gfx.content.skia-font-cache-size", 20);
user_pref("media.ffmpeg.vaapi.enabled", true); // hardware video decode on Linux
user_pref("media.hardware-video-decoding.force-enabled", true);

// Compact mode option
user_pref("browser.compactmode.show", true);

// Resist fingerprinting lite (breaks fewer sites than full RFP)
user_pref("privacy.resistFingerprinting.letterboxing", false);
user_pref("webgl.disabled", false); // keep webgl for compatibility

// HTTPS-only mode
user_pref("dom.security.https_only_mode", true);
user_pref("dom.security.https_only_mode_send_http_background_request", false);

// Disable prefetching (saves bandwidth, slight privacy gain)
user_pref("network.dns.disablePrefetch", true);
user_pref("network.dns.disablePrefetchFromHTTPS", true);

// =============================================================================
// PERFORMANCE TWEAKS
// =============================================================================
// Faster session restore
user_pref("browser.sessionstore.interval", 30000); // save session every 30s instead of 15s
user_pref("browser.sessionstore.max_tabs_undo", 10);
user_pref("browser.sessionhistory.max_entries", 25); // fewer back/forward entries

// Memory
user_pref("browser.cache.disk.enable", true);
user_pref("browser.cache.memory.capacity", 524288); // 512MB memory cache
user_pref("browser.cache.disk.smart_size.enabled", false);
user_pref("browser.cache.disk.capacity", 1048576); // 1GB disk cache

// Image/media
user_pref("image.mem.decode_bytes_at_a_time", 131072); // faster image decode
user_pref("media.memory_cache_max_size", 131072);

// Rendering
user_pref("nglayout.initialpaint.delay", 0);
user_pref("nglayout.initialpaint.delay_in_oopif", 0);
user_pref("content.notify.interval", 100000);

// Tab unloading (memory saver for many tabs)
user_pref("browser.tabs.unloadOnLowMemory", true);
