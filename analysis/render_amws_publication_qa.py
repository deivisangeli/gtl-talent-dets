"""Render publication pages and numbered contact sheets for visual inspection."""
import argparse
import json
from pathlib import Path
import subprocess
from PIL import Image, ImageDraw
from pypdf import PdfReader

from amws_publication_refresh import REPO, RUN, digest
from compile_amws_publications import DOCUMENTS

POPLER = Path('C:/Users/megaj/.cache/codex-runtimes/codex-primary-runtime/dependencies/native/poppler/Library/bin')


def render(name):
    pdf = REPO / 'project' / (name + '.pdf')
    folder = RUN / 'pdf_qa' / name
    folder.mkdir(parents=True, exist_ok=True)
    subprocess.run([str(POPLER / 'pdftoppm.exe'), '-r', '100', '-png', str(pdf), str(folder / 'page')], check=True)
    images = sorted(folder.glob('page-*.png'))
    reader = PdfReader(pdf)
    assert len(images) == len(reader.pages)
    pages = []
    for i, page in enumerate(reader.pages, 1):
        text = page.extract_text() or ''
        pages.append({'page': i, 'characters': len(text), 'text': text})
    contacts = []
    for start in range(0, len(images), 6):
        canvas = Image.new('RGB', (1500, 1480), '#d6d6d6')
        draw = ImageDraw.Draw(canvas)
        for index, path in enumerate(images[start:start + 6]):
            with Image.open(path) as image:
                image.thumbnail((478, 692))
                x = (index % 3) * 500 + (500 - image.width) // 2
                y = (index // 3) * 740 + 30
                canvas.paste(image, (x, y))
                draw.text((index % 3 * 500 + 12, index // 3 * 740 + 8),
                          f'{name} | page {start + index + 1}', fill='black')
        target = folder / f'contact-{start // 6 + 1:02d}.png'
        canvas.save(target)
        contacts.append(str(target))
    (folder / 'text_pages.json').write_text(json.dumps(pages, indent=2), encoding='utf-8')
    manifest = RUN / 'pdf_render_manifest.json'
    records = json.loads(manifest.read_text()) if manifest.exists() else {}
    records[name] = {'pdf': str(pdf), 'sha256': digest(pdf), 'pages': len(images),
                     'contacts': contacts, 'dpi': 100, 'visual_inspection': 'pending'}
    manifest.write_text(json.dumps(records, indent=2))
    print(name, len(images), 'pages;', len(contacts), 'contact sheets', flush=True)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--documents', nargs='+', choices=DOCUMENTS, default=DOCUMENTS)
    for name in parser.parse_args().documents:
        render(name)
