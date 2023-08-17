# Paul Becker <p@becker.kiwi>
# Based on dotbot-librewolf from Kurt McKee <contactme@kurtmckee.org>
# SPDX-License-Identifier: MIT

from __future__ import annotations

import logging
import os
import pathlib
import sys
import typing

import dotbot.plugin
import dotbot.plugins.link

VALID_DIRECTIVES: set[str] = {"librewolf"}

log = logging.getLogger(__name__)


def _get_librewolf_directories() -> typing.Iterable[pathlib.Path]:
    """Yield Librewolf directories that appear to be valid."""

    paths = []

    if sys.platform.startswith("win32"):  # Windows
        paths.append(os.path.expandvars("%LOCALAPPDATA%/Librewolf"))
    elif sys.platform.startswith("darwin"):  # MacOS
        paths.append("~/Library/Application Support/Librewolf")
    else:  # Linux and other Unix-like systems
        paths.append("~/.librewolf")
        paths.append("~/.var/app/io.gitlab.librewolf-community/.librewolf")

    for path in paths:
        expanded_path = pathlib.Path(os.path.expanduser(path))
        if expanded_path.is_dir():
            yield expanded_path


def _get_profile_directories() -> typing.Iterable[pathlib.Path]:
    """Yield Librewolf profile directories that appear to be valid."""

    for librewolf_directory in _get_librewolf_directories():
        for profile in librewolf_directory.glob("*"):
            if profile.is_dir() and (profile / "prefs.js").is_file():
                yield profile


class Librewolf(dotbot.plugin.Plugin):
    def can_handle(self, directive: str) -> bool:
        """
        Flag whether this plugin supports the given *directive*.
        """

        return directive in VALID_DIRECTIVES

    def handle(self, directive: str, data: dict[str, typing.Any]) -> bool:
        """
        Handle Librewolf configuration directives.

        :raises ValueError:
            ValueError is raised if `handle()` is called with an unsupported directive.
        """

        if not self.can_handle(directive):
            message = f"The Librewolf plugin does not handle the '{directive}' directive."
            raise ValueError(message)

        success: bool = True

        if "librewolf.overrides.cfg" in data:
            success &= self._handle_librewolf_overrides_cfg(data["librewolf.overrides.cfg"])

        if "user.js" in data:
            success &= self._handle_user_js(data["user.js"])

        if "userChrome.css" in data or "chrome" in data:
            user_chrome_value = data.get("userChrome.css", data.get("chrome"))
            success &= self._handle_user_chrome(user_chrome_value)

        return success

    def _handle_user_js(self, value: typing.Any) -> bool:
        """Create links to a specified ``user.js`` in each Librewolf profile directory."""

        link_plugin = dotbot.plugins.link.Link(self._context)
        links: dict[str, typing.Any] = {
            str(profile / "user.js"): value for profile in _get_profile_directories()
        }

        if not links:
            log.warning("No Librewolf profiles found")
            return True

        return link_plugin.handle("link", links)


    def _handle_librewolf_overrides_cfg(self, value: typing.Any) -> bool:
        """Create a link to the specified ``librewolf.overrides.cfg`` in the Librewolf directory."""

        link_plugin = dotbot.plugins.link.Link(self._context)
        success = True
        for librewolf_directory in _get_librewolf_directories():
            links: dict[str, typing.Any] = {str(librewolf_directory / "librewolf.overrides.cfg"): value}
            success &= link_plugin.handle("link", links)

        if not success:
            log.warning("No valid Librewolf directories found")

        return success

    def _handle_user_chrome(self, value: typing.Any) -> bool:
        """Link userChrome.css or chrome directory in each Librewolf profile directory."""
        success = True

        if isinstance(value, str):
            src = pathlib.Path(os.path.expandvars(os.path.expanduser(value)))

            if src.is_file() and src.name == "userChrome.css":
                link_plugin = dotbot.plugins.link.Link(self._context)
                links = {str(profile / "chrome" / "userChrome.css"): value for profile in _get_profile_directories()}
                success &= link_plugin.handle("link", links)
            elif src.is_dir() and src.name == "chrome":
                link_plugin = dotbot.plugins.link.Link(self._context)
                for profile in _get_profile_directories():
                    chrome_dst = profile / "chrome"
                    links = {str(chrome_dst / f.relative_to(src)): str(f) for f in src.glob("**/*") if f.is_file()}
                    success &= link_plugin.handle("link", links)
            else:
                log.error(f"Invalid path for userChrome.css or chrome directory: {value}")
                success = False

        return success
