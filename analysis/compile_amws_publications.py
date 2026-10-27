"""Compile the four refreshed multi-file LaTeX publications with saved logs."""
import argparse
import json
import os
import re
from pathlib import Path
import shutil
import subprocess
import time

from amws_publication_refresh import REPO, RUN, TAG, digest

DOCUMENTS = ('main', 'wwii_gi_bill_amws_reduced_form', 'mandatory_schooling', 'report_companion')


def compile_document(name):
    assert name in DOCUMENTS
    project = REPO / 'project'
    build = project / 'tmp' / TAG / name
    build.mkdir(parents=True, exist_ok=True)
    executable = shutil.which('pdflatex')
    assert executable, 'pdflatex is required for these multi-file documents'
    started = time.time()
    command = [executable, '-interaction=nonstopmode', '-halt-on-error',
               '-file-line-error', '-output-directory=' + str(build), str(project / (name + '.tex'))]
    # Run inside the isolated build directory so stale root-level .aux/.bbl
    # files cannot override this run's generated bibliography and references.
    env = os.environ.copy()
    for variable in ('TEXINPUTS', 'BIBINPUTS'):
        env[variable] = '.;' + project.as_posix() + ';' + env.get(variable, '') + ';'
    commands = [command]
    if '\\bibliography{' in (project / (name + '.tex')).read_text(encoding='utf-8-sig'):
        commands.append([shutil.which('bibtex'), name])
        commands += [command, command]
    else:
        commands.append(command)
    for i, cmd in enumerate(commands):
        log = RUN / 'logs' / f'compile_{name}_{i + 1}.log'
        with log.open('w', encoding='utf-8') as stream:
            result = subprocess.run(cmd, cwd=build, env=env, stdout=stream, stderr=subprocess.STDOUT)
        if result.returncode:
            print(log.read_text(errors='replace')[-6000:], flush=True)
            raise RuntimeError(f'Compilation failed: {name}, pass {i + 1}; {log}')
    final_log = (build / (name + '.log')).read_text(errors='replace')
    unresolved = [line for line in final_log.splitlines()
                  if ('undefined' in line.lower() or 'multiply defined' in line.lower())
                  and ('Warning' in line or 'references' in line)]
    assert not unresolved, (name, unresolved)
    output = build / (name + '.pdf')
    assert output.exists() and output.stat().st_size > 1000
    target = project / output.name
    before = digest(target) if target.exists() else None
    shutil.copy2(output, target)
    record = {'document': name, 'source': str(project / (name + '.tex')),
              'source_sha256': digest(project / (name + '.tex')),
              'output': str(target), 'sha256_before': before,
              'sha256_after': digest(target), 'commands': commands,
              'seconds': time.time() - started, 'finished': time.time(),
              'warnings': [line for line in final_log.splitlines()
                           if 'Overfull' in line or 'Warning' in line]}
    source_text = (project / (name + '.tex')).read_text(encoding='utf-8-sig')
    dependencies = re.findall(r'\\(?:includegraphics(?:\[[^\]]*\])?|input|figurepage)\{([^}]+)\}', source_text)
    dependencies += [entry.strip() + '.bib' for group in re.findall(r'\\bibliography\{([^}]+)\}', source_text)
                     for entry in group.split(',')]
    resolved = []
    for value in dependencies:
        if '#' in value:
            continue
        path = project / value
        if not path.suffix:
            path = path.with_suffix('.tex')
        assert path.exists(), path
        resolved.append(path)
    record['dependencies'] = {str(path): digest(path) for path in resolved}
    record['build_directory'] = str(build)
    record['search_paths'] = {key: env[key] for key in ('TEXINPUTS','BIBINPUTS')}
    manifest = RUN / 'compilation_manifest.json'
    records = json.loads(manifest.read_text()) if manifest.exists() else {}
    records[name] = record
    manifest.write_text(json.dumps(records, indent=2))
    print('Compiled', name, 'in', round(record['seconds']), 'seconds;',
          len(record['warnings']), 'warnings', flush=True)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--documents', nargs='+', choices=DOCUMENTS, default=DOCUMENTS)
    for document in parser.parse_args().documents:
        compile_document(document)
