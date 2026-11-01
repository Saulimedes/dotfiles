import re

from yt_dlp.extractor.common import InfoExtractor
from yt_dlp.utils import ExtractorError, int_or_none


class XXXTubeIE(InfoExtractor):
    """KVS based site. The generic extractor picks the <video> ad banner
    (spicy-box.mp4) instead of the real file in flashvars."""

    _VALID_URL = r'https?://(?:www\.)?x-x-x\.tube/videos/(?P<id>\d+)'

    def _real_extract(self, url):
        video_id = self._match_id(url)

        webpage = None
        for attempt in range(4):
            try:
                webpage = self._download_webpage(url, video_id)
                break
            except ExtractorError as e:
                # the site sporadically answers with HTTP 500
                if attempt == 3 or '500' not in str(e):
                    raise
                self.report_warning('HTTP 500, retrying')
        flashvars = self._search_regex(
            r'flashvars\s*=\s*\{(.+?)\}\s*;', webpage, 'flashvars', flags=re.S)

        def fv(key):
            return self._search_regex(
                rf"\b{key}\s*:\s*'([^']*)'", flashvars, key, default=None)

        formats = []
        for key in ('video_url', 'video_alt_url', 'video_alt_url2', 'video_alt_url3'):
            video_url = fv(key)
            if not video_url:
                continue
            if video_url.startswith('function/'):
                raise ExtractorError('Obfuscated video_url is not supported', expected=True)
            text = fv(f'{key}_text') or ('720p' if key == 'video_url' else key)
            formats.append({
                'url': video_url,
                'format_id': key,
                'height': int_or_none(self._search_regex(r'(\d{3,4})p', text, 'height', default=None)),
                'http_headers': {'Referer': url},
            })
        if not formats:
            raise ExtractorError('No video_url found in flashvars', expected=True)

        return {
            'id': video_id,
            'title': fv('video_title') or self._og_search_title(webpage),
            'thumbnail': fv('preview_url'),
            'tags': [t.strip() for t in (fv('video_tags') or '').split(',') if t.strip()],
            'cast': [m.strip() for m in (fv('video_models') or '').split(',') if m.strip()],
            'webpage_url': url,
            'formats': formats,
        }
