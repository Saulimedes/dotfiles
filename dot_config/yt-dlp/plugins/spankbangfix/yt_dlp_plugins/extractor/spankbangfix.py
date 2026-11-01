import re

from yt_dlp.extractor.spankbang import SpankBangIE as _SpankBangIE
from yt_dlp.utils import clean_html


class SpankBangIE(_SpankBangIE):
    """The stock extractor reads the title from <h1 title="...">, which the
    site dropped, so every video ended up titled by its id."""

    _sb_page = None

    def _download_webpage(self, *args, **kwargs):
        page = super()._download_webpage(*args, **kwargs)
        if isinstance(page, str) and 'data-testid=video-title' in page:
            self._sb_page = page
        return page

    def _real_extract(self, url):
        self._sb_page = None
        info = super()._real_extract(url)
        if info.get('title') not in (None, '', info.get('id'), info.get('display_id')):
            return info

        page = self._sb_page or self._download_webpage(url, info['id'], 'Downloading title page')
        h1 = re.search(r'<h1[^>]+data-testid=["\']?video-title["\']?[^>]*>(.*?)</h1>', page, re.S)
        title = clean_html(h1.group(1)) if h1 else None
        if not title:
            og = self._og_search_title(page, default=None)
            title = re.sub(r'\s*-\s*SpankBang$', '', og or '') or None
        if title:
            info['title'] = title
        return info
