# Browser configuration
{ config, pkgs, lib, inputs, ... }:

{
  # Helium - Primary browser (Chromium-based, MV2 support)
  # Extensions need to be installed manually in Helium
  home.packages = [
    pkgs.ungoogled-chromium

    # Zen Browser (Firefox-based, privacy focused)
    inputs.zen-browser.packages.${pkgs.stdenv.hostPlatform.system}.twilight

    # Helium browser - uncomment when package is fixed
    # pkgs.custom.helium
  ];

  # Brave - Secondary browser (MV2 support, with managed extensions)
  programs.chromium = {
    enable = true;
    package = pkgs.brave;
    extensions = [
      # uBlock Origin
      { id = "cjpalhdlnbpafiamejdnhcphjbkeiagm"; }
      # Bitwarden
      { id = "nngceckbapebfimnlniiiahkandclblb"; }
      # Floccus (bookmark sync)
      { id = "fnaicdffflnofjppbagibeoednpnhbae"; }
      # Get cookies.txt
      { id = "bgaddhkoddajcdgocldbbfleckgcbcid"; }
      # Tampermonkey
      { id = "dhdgffkkebhmkfjojejmpbldmpobfkfo"; }
      # Return YouTube Dislikes
      { id = "gebbhagfogifgggkldgodflihgfeippi"; }
      # SponsorBlock
      { id = "mnjggcdmjocbbbhaepdhchncahnbgone"; }
      # Enhancer for YouTube
      { id = "ponfpcnoihfmfllpaingbgckeeldkhle"; }
    ];
    commandLineArgs = [
      "--enable-features=VaapiVideoDecodeLinuxGL"
      "--ignore-gpu-blocklist"
      "--enable-zero-copy"
    ];
  };

  # Firefox - Tertiary browser
  programs.firefox = {
    enable = true;

    profiles.default = {
      id = 0;
      name = "default";
      isDefault = true;

      # Settings written to user.js (from your existing profile)
      settings = {
        # === UI & Startup ===
        "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
        "svg.context-properties.content.enabled" = true;
        "browser.compactmode.show" = true;
        "browser.startup.homepage" = "about:blank";
        "browser.newtabpage.enabled" = false;
        "browser.newtab.url" = "about:blank";
        "browser.tabs.tabClipWidth" = 83;
        "browser.tabs.loadBookmarksInTabs" = true;
        "browser.tabs.firefox-view" = false;
        "browser.tabs.tabmanager.enabled" = false;
        "browser.bookmarks.openInTabClosesMenu" = false;
        "browser.toolbars.bookmarks.visibility" = "never";
        "layout.css.has-selector.enabled" = true;
        "layout.css.animation-composition.enabled" = true;
        "browser.uidensity" = 1;

        # === Downloads ===
        "browser.download.useDownloadDir" = false;
        "browser.download.alwaysOpenPanel" = false;
        "browser.download.manager.addToRecentDocs" = false;
        "browser.download.always_ask_before_handling_new_types" = true;

        # === Performance & Rendering ===
        "nglayout.initialpaint.delay" = 0;
        "nglayout.initialpaint.delay_in_oopif" = 0;
        "content.notify.interval" = 100000;
        "gfx.webrender.all" = true;
        "gfx.webrender.precache-shaders" = true;
        "gfx.webrender.compositor" = true;
        "layers.gpu-process.enabled" = true;
        "webgl.disabled" = false;
        "media.hardware-video-decoding.enabled" = true;
        "media.ffmpeg.vaapi.enabled" = true;
        "media.eme.enabled" = true;
        "media.gmp-widevinecdm.enabled" = true;

        # === Network & Cache ===
        "browser.cache.memory.max_entry_size" = 153600;
        "browser.cache.memory.capacity" = 512000;
        "network.buffer.cache.size" = 262144;
        "network.buffer.cache.count" = 128;
        "network.http.max-connections" = 1800;
        "network.http.max-persistent-connections-per-server" = 10;
        "network.trr.mode" = 5;

        # === Smooth Scrolling ===
        "general.smoothScroll" = true;
        "general.smoothScroll.msdPhysics.continuousMotionMaxDeltaMS" = 12;
        "general.smoothScroll.msdPhysics.enabled" = true;
        "general.smoothScroll.msdPhysics.motionBeginSpringConstant" = 600;
        "general.smoothScroll.msdPhysics.regularSpringConstant" = 650;
        "general.smoothScroll.msdPhysics.slowdownMinDeltaMS" = 25;
        "general.smoothScroll.msdPhysics.slowdownMinDeltaRatio" = "2.0";
        "general.smoothScroll.msdPhysics.slowdownSpringConstant" = 250;
        "general.smoothScroll.currentVelocityWeighting" = "1.0";
        "general.smoothScroll.stopDecelerationWeighting" = "1.0";
        "mousewheel.default.delta_multiplier_y" = 300;

        # === Privacy & Tracking ===
        "privacy.donottrackheader.enabled" = true;
        "privacy.trackingprotection.enabled" = true;
        "privacy.trackingprotection.fingerprinting.enabled" = true;
        "privacy.trackingprotection.cryptomining.enabled" = true;
        "privacy.trackingprotection.socialtracking.enabled" = true;
        "dom.private-attribution.submission.enabled" = false;
        "geo.enabled" = false;
        "geo.provider.network.url" = "https://location.services.mozilla.com/v1/geolocate?key=%MOZILLA_API_KEY%";
        "permissions.default.geo" = 2;

        # === Telemetry (all disabled) ===
        "toolkit.telemetry.unified" = false;
        "toolkit.telemetry.enabled" = false;
        "toolkit.telemetry.server" = "data:,";
        "toolkit.telemetry.archive.enabled" = false;
        "toolkit.telemetry.newProfilePing.enabled" = false;
        "toolkit.telemetry.shutdownPingSender.enabled" = false;
        "toolkit.telemetry.updatePing.enabled" = false;
        "toolkit.telemetry.bhrPing.enabled" = false;
        "toolkit.telemetry.firstShutdownPing.enabled" = false;
        "toolkit.telemetry.coverage.opt-out" = true;
        "toolkit.coverage.opt-out" = true;
        "toolkit.coverage.endpoint.base" = "";
        "toolkit.aboutProcesses.showProfilerIcons" = false;
        "datareporting.policy.dataSubmissionEnabled" = false;
        "datareporting.healthreport.uploadEnabled" = false;
        "browser.newtabpage.activity-stream.feeds.telemetry" = false;
        "browser.newtabpage.activity-stream.telemetry" = false;
        "browser.send_pings" = false;
        "browser.send_pings.require_same_host" = true;

        # === Crash Reports & Normandy ===
        "app.normandy.enabled" = false;
        "app.normandy.api_url" = "";
        "breakpad.reportURL" = "";
        "browser.tabs.crashReporting.sendReport" = false;
        "browser.crashReports.unsubmittedCheck.autoSubmit2" = false;

        # === New Tab & Activity Stream ===
        "browser.newtabpage.activity-stream.default.sites" = "";
        "browser.newtabpage.activity-stream.improvesearch.topSiteSearchShortcuts.searchEngines" = "";
        "browser.newtabpage.activity-stream.showSponsored" = false;
        "browser.newtabpage.activity-stream.showSponsoredTopSites" = false;

        # === Misc ===
        "identity.fxaccounts.enabled" = true;
        "browser.display.use_document_fonts" = 1;
        "browser.translations.neverTranslateLanguages" = "de, en";
        "browser.translations.panelShown" = false;
        "browser.vpn_promo.enabled" = false;
        "browser.ml.chat.sidebar" = false;
        "sidebar.main.tools" = "syncedtabs,history";

        # === Enable right-click ===
        "dom.event.contextmenu.enabled" = true;
      };

      # Extensions from NUR
      extensions.packages = with pkgs.nur.repos.rycee.firefox-addons; [
        # Core
        ublock-origin
        bitwarden
        floccus

        # YouTube
        return-youtube-dislikes
        sponsorblock
        enhancer-for-youtube

        # Privacy & Utility
        tampermonkey
        clearurls
        absolute-enable-right-click

        # Cookies
        cookies-txt

        # Theming
        # Note: Search NUR for ambient theme or install manually
      ];
    };
  };

  # Set Helium as default browser
  xdg.mimeApps = {
    enable = true;
    defaultApplications = {
      "text/html" = "helium.desktop";
      "x-scheme-handler/http" = "helium.desktop";
      "x-scheme-handler/https" = "helium.desktop";
      "x-scheme-handler/about" = "helium.desktop";
      "x-scheme-handler/unknown" = "helium.desktop";
    };
  };

  # Ungoogled Chromium extensions (manual install):
  # Install "ArkenfoxUC" or "chromium-web-store" helper first, then:
  # - uBlock Origin: cjpalhdlnbpafiamejdnhcphjbkeiagm
  # - Bitwarden: nngceckbapebfimnlniiiahkandclblb
  # - FoxyProxy: gcknhkkoolaabfmlnjonogaaifnjlfnp
  # - Floccus: fnaicdffflnofjppbagibeoednpnhbae
}
