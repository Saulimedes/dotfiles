import requests
import os
from tempfile import mkdtemp
import shutil
from ansible.module_utils.basic import AnsibleModule
import json

class FirefoxExtension:
    def __init__(self, slug, profile_path):
        self.slug = slug
        self.profile_path = profile_path
        self.info = self._get_info()
        self.download_path = os.path.join(mkdtemp(), self.filename)
        self.destination = os.path.join(profile_path, 'extensions', f'{self.guid}.xpi')

    def _get_info(self):
        url = f'https://addons.mozilla.org/api/v5/addons/addon/{self.slug}/'
        try:
            r = requests.get(url)
            r.raise_for_status()
            info = r.json()
            self.id = info['id']
            self.guid = info['guid']
            self.filename = f'{info["slug"]}-{info["current_version"]["version"]}.xpi'
            return info
        except requests.RequestException as e:
            raise Exception(f'Error fetching addon info: {str(e)}')
        except KeyError as e:
            raise Exception(f'Error processing addon info: Missing key {str(e)}. Full response: {json.dumps(info, indent=2)}')

    def url(self):
        try:
            if 'current_version' not in self.info:
                raise KeyError('current_version')
            if 'file' not in self.info['current_version']:
                raise KeyError('file')
            
            file = self.info['current_version']['file']
            if 'url' not in file:
                raise KeyError('url')
            
            return file['url']
        except KeyError as e:
            raise Exception(f'Error finding download URL: Missing key {str(e)}. Full info: {json.dumps(self.info, indent=2)}')

    def _download(self):
        try:
            download_url = self.url()
            r = requests.get(download_url, stream=True)
            r.raise_for_status()
            with open(self.download_path, 'wb') as f:
                for chunk in r.iter_content(chunk_size=8192):
                    f.write(chunk)
        except requests.RequestException as e:
            raise Exception(f'Error downloading extension: {str(e)}')

    def is_installed(self):
        return os.path.isfile(self.destination)

    def install(self):
        path = os.path.dirname(self.destination)
        os.makedirs(path, mode=0o700, exist_ok=True)
        self._download()
        shutil.move(self.download_path, self.destination)

    def uninstall(self):
        if os.path.isfile(self.destination):
            os.remove(self.destination)

def main():
    module = AnsibleModule(
        argument_spec=dict(
            name=dict(required=True, type='str'),
            profile_path=dict(required=True, type='path'),
            state=dict(default='present', choices=['present', 'absent'], type='str'),
        )
    )

    try:
        addon = FirefoxExtension(module.params['name'], module.params['profile_path'])
        changed = False
        result = None

        if module.params['state'] == 'present' and not addon.is_installed():
            addon.install()
            changed = True
            result = {'id': addon.id, 'url': addon.url(), 'name': addon.filename}
        elif module.params['state'] == 'absent' and addon.is_installed():
            addon.uninstall()
            changed = True

        module.exit_json(changed=changed, meta=result)
    except Exception as e:
        module.fail_json(msg=f'Error: {str(e)}')

if __name__ == '__main__':
    main()
