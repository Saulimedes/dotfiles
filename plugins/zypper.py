import subprocess
import time
from dotbot import Plugin


class ZypperPlugin(Plugin):
    _directive = 'zypper'

    def can_handle(self, directive):
        return directive == self._directive

    def handle(self, directive, data):
        if directive != self._directive:
            self._log.error("Unsupported directive: {}".format(directive))
            return False
        if not isinstance(data, dict):
            self._log.error("Data must be a dictionary")
            return False

        success = True

        for key_url in data.get('public_keys', []):
            success &= self._import_public_key(key_url)

        for repo_entry in data.get('repositories', []):
            if not self._repo_exists(repo_entry):
                success &= self._add_repository(repo_entry)

        success &= self._update_system()
        success &= self._install_packages(data.get('packages', []))

        return success

    def _import_public_key(self, key_url):
        return self._execute(['sudo', 'rpm', '--import', key_url])

    def _add_repository(self, repo_entry):
        if isinstance(repo_entry, str):
            repo_url = repo_entry
            command = ['sudo', 'zypper', 'addrepo', repo_url]
        elif isinstance(repo_entry, dict):
            repo_url = repo_entry.get('url')
            repo_alias = repo_entry.get('alias')
            if repo_alias:
                command = ['sudo', 'zypper', 'ar', repo_url, repo_alias]
            else:
                command = ['sudo', 'zypper', 'addrepo', repo_url]
        else:
            self._log.error("Invalid repository entry format")
            return False
        
        return self._execute(command)

    def _update_system(self):
        return self._execute(['sudo', 'zypper', '--non-interactive', 'ref'])

    def _install_packages(self, packages):
        if packages:
            return self._execute(['sudo', 'zypper', '--non-interactive', 'in'] + packages)
        return True

    def _execute(self, command):
        self._log.debug(f"Executing: {' '.join(command)}")
        for attempt in range(3):
            result = subprocess.run(command, check=False, capture_output=True, text=True)
            if result.returncode == 0:
                return True
            elif "System management is locked" in result.stderr:
                self._log.warning("Zypper is locked, waiting...")
                time.sleep(5)  # Wait for 5 seconds before retrying
            else:
                self._log.error(f"Command failed: {' '.join(command)}\n{result.stderr}")
                return False
        return False

    def _repo_exists(self, repo_entry):
        if isinstance(repo_entry, str):
            repo_alias = repo_entry.split('/')[-1].split('.')[0]
        elif isinstance(repo_entry, dict):
            repo_alias = repo_entry.get('alias')
        else:
            return False
        result = subprocess.run(['sudo', 'zypper', 'lr', '--no-refresh'], capture_output=True, text=True)
        return repo_alias in result.stdout

def create_plugin():
    return ZypperPlugin()
