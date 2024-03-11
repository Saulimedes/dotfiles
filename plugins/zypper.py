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
            raise ValueError("The data for 'zypper' directive must be a dictionary")

        packages = data.get('packages', [])
        repositories = data.get('repositories', [])
        public_keys = data.get('public_keys', [])

        success = True

        for key_url in public_keys:
            success &= self._import_public_key(key_url).returncode == 0

        existing_repos = self._get_existing_repositories()

        for repo in repositories:
            repo_alias, _ = self._parse_repo_input(repo)
            if repo_alias not in existing_repos:
                success &= self._add_repository(repo).returncode in (0, 4)
            else:
                self._log.debug(f"Repository '{repo_alias}' already exists, skipping addition.")

        command = ['sudo', 'zypper', '--non-interactive', '--gpg-auto-import-keys', 'update']
        success &= self._execute(command).returncode == 0

        command = ['sudo', 'zypper', '--non-interactive', '--gpg-auto-import-keys', 'install'] + packages
        success &= self._execute(command).returncode == 0

        return success

    def _import_public_key(self, key_url):
        command = ['sudo', 'rpm', '--import', key_url]
        return self._execute(command)

    def _add_repository(self, repo):
        command = ['sudo', 'zypper', '--non-interactive', '--gpg-auto-import-keys', 'ar', repo]
        return self._execute(command)

    def _execute(self, command):
        self._log.debug(f"Executing: {' '.join(command)}")
        return subprocess.run(command, check=False)

    def _get_existing_repositories(self):
        command = ['zypper', 'lr', '--no-refresh']
        result = subprocess.run(command, capture_output=True, check=True, text=True)
        repos = {}
        for line in result.stdout.splitlines():
            parts = line.split('|')
            if len(parts) > 2:  # Valid lines containing repo info
                alias = parts[1].strip()
                repos[alias.lower()] = True  # Use lower case for case-insensitive comparison
        return repos

    def _parse_repo_input(self, repo_input):
        if ' ' in repo_input:
            repo_alias, repo_url = repo_input.split(' ', 1)
        else:
            repo_url = repo_input
            repo_alias = repo_url.rstrip('/').split('/')[-1]
        return repo_alias.lower(), repo_url  # Lower case to match the case-insensitive check

def setup(bot):
    bot.register_plugin(Zypper)
