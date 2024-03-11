import subprocess
from dotbot import Plugin

class Zypper(Plugin):
    _directive = 'zypper'

    def can_handle(self, directive):
        return directive == self._directive

    def handle(self, directive, data):
        self._log.debug(f"Handling directive: {directive}")
        if directive != self._directive:
            raise ValueError(f"Unsupported directive: {directive}")
        if not isinstance(data, dict):
            self._log.error("Data must be a dictionary")
            return False

        success = True

        for key_url in data.get('public_keys', []):
            if not self._import_public_key(key_url):
                self._log.error(f"Failed to import public key: {key_url}")
                success = False

        for repo_url in data.get('repositories', []):
            repo_alias = self._derive_alias_from_url(repo_url)
            if not repo_alias or not self._add_repository(repo_url, repo_alias):
                success = False

        if not self._update_system() or not self._install_packages(data.get('packages', [])):
            success = False

        return success

    def _import_public_key(self, key_url):
        return self._execute(['sudo', 'rpm', '--import', key_url]) == 0

    def _add_repository(self, repo_url, repo_alias):
        # Use the alias exactly as derived, without appending '.repo'
        return self._execute(['sudo', 'zypper', '--non-interactive', 'ar', repo_url, repo_alias]) == 0

    def _update_system(self):
        return self._execute(['sudo', 'zypper', '--non-interactive', 'up']) == 0

    def _install_packages(self, packages):
        if packages:
            return self._execute(['sudo', 'zypper', '--non-interactive', 'in'] + packages) == 0
        return True

    def _execute(self, command):
        self._log.debug(f"Executing: {' '.join(command)}")
        result = subprocess.run(command, check=False)
        return result.returncode

    def _derive_alias_from_url(self, repo_url):
        # Extract the last segment of the URL; remove '.repo' if present
        alias = repo_url.rstrip('/').split('/')[-1]
        if alias.endswith('.repo'):
            # Remove the trailing '.repo' to avoid duplication
            alias = alias[:-5]
        return alias

def setup(bot):
    bot.register_plugin(Zypper)

