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
            repo_alias = self._find_existing_repository(repo, existing_repos)
            if repo_alias:
                success &= self._refresh_repository(repo_alias).returncode == 0
            else:
                add_repo_result = self._add_repository(repo)
                success &= add_repo_result.returncode in (0, 4)

        # Execute zypper update
        command = ['sudo', 'zypper', '--non-interactive', 'update']
        success &= self._execute(command).returncode == 0

        # Install packages
        command = ['sudo', 'zypper', '--non-interactive', 'install'] + packages
        success &= self._execute(command).returncode == 0

        return success

    def _import_public_key(self, key_url):
        command = ['sudo', 'rpm', '--import', key_url]
        return self._execute(command)

    def _add_repository(self, repo):
        command = ['sudo', 'zypper', '--non-interactive', 'ar', repo]
        result = self._execute(command)
        if result.returncode == 0:
            repo_alias = self._find_existing_repository(repo, self._get_existing_repositories())
            if repo_alias:
                self._refresh_repository(repo_alias)
        return result

    def _refresh_repository(self, repo_alias):
        command = ['sudo', 'zypper', '--non-interactive', 'refresh', repo_alias]
        return self._execute(command)

    def _execute(self, command):
        self._log.debug(f"Executing: {' '.join(command)}")
        return subprocess.run(command, check=False)

    def _get_existing_repositories(self):
        command = ['zypper', 'lr', '-u']
        result = subprocess.run(command, capture_output=True, check=True, text=True)
        return result.stdout.splitlines()

    def _find_existing_repository(self, repo_input, existing_repos):
        existing_repo_data = []
        for line in existing_repos:
            line_parts = [part.strip() for part in line.split('|')]
            if len(line_parts) >= 4:
                existing_repo_data.append({'alias': line_parts[2], 'url': line_parts[4]})

        repo_alias, repo_url = self._parse_repo_input(repo_input)

        for repo_data in existing_repo_data:
            if repo_data['url'] == repo_url or repo_data['alias'] == repo_alias:
                return repo_data['alias']

        return None

    def _parse_repo_input(self, repo_input):
        repo_parts = repo_input.split()
        if len(repo_parts) == 1:
            repo_url = repo_parts[0]
            repo_alias = repo_url.rstrip('/').split('/')[-1]
        elif len(repo_parts) == 2:
            repo_alias, repo_url = repo_parts
        else:
            raise ValueError(f"Invalid repository input: {repo_input}")

        return repo_alias, repo_url

def setup(bot):
    bot.register_plugin(Zypper)

