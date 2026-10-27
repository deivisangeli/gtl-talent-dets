"""Local AMWS working checkpoints with portable ZIP backups and path relocation."""
import argparse
import datetime
import functools
import hashlib
import json
import os
from pathlib import Path
import shutil
import zipfile


def storage_root():
    return Path(os.environ.get('AMWS_CHECKPOINT_ROOT', str(Path.home()/'.codex/amws_checkpoints'))).resolve()


def raw_digest(path):
    value=hashlib.sha256()
    with Path(path).open('rb') as source:
        for block in iter(lambda:source.read(1048576),b''):value.update(block)
    return value.hexdigest()


def write_json(path,value):
    path=Path(path);path.parent.mkdir(parents=True,exist_ok=True)
    temporary=path.with_suffix(path.suffix+'.tmp')
    temporary.write_text(json.dumps(value,ensure_ascii=False,indent=2)+'\n',encoding='utf-8')
    temporary.replace(path)


@functools.lru_cache(maxsize=8)
def registry(root):
    file=Path(root)/'registry.json'
    return json.loads(file.read_text(encoding='utf-8')) if file.exists() else dict(directories=[],historical_code={})


def save_registry(data):
    write_json(storage_root()/'registry.json',data);registry.cache_clear()


def resolve(path):
    path=Path(path).resolve()
    rules=registry(str(storage_root()))['directories']
    for item in sorted(rules,key=lambda r:-len(r['source'])):
        source=Path(item['source'])
        if path.is_relative_to(source):return Path(item['destination'])/path.relative_to(source)
    return path


def register_directory(source,destination):
    source=Path(source).resolve();destination=Path(destination).resolve()
    data=registry(str(storage_root()))
    old=next((r for r in data['directories'] if Path(r['source'])==source),None)
    if old:
        if Path(old['destination'])!=destination:raise ValueError('Conflicting checkpoint relocation')
        return
    data['directories'].append(dict(source=str(source),destination=str(destination)))
    save_registry(data)


def canonical_run(run):
    name=Path(run).name
    return name[1:-8] if name.startswith('.') and name.endswith('.staging') else name


def checkpoint_dir(run,name):
    run=Path(run).resolve(); source=run/name
    relocated=resolve(source)
    if relocated!=source:return relocated
    destination=storage_root()/canonical_run(run)/name
    register_directory(source,destination)
    if run.name.startswith('.') and run.name.endswith('.staging'):
        register_directory(run.parent/canonical_run(run)/name,destination)
    return destination


def archive_code(paths):
    data=registry(str(storage_root()))
    for source in map(Path,paths):
        checksum=raw_digest(source)
        target=storage_root()/'historical_code'/checksum/source.name
        target.parent.mkdir(parents=True,exist_ok=True)
        if not target.exists():shutil.copy2(source,target)
        if raw_digest(target)!=checksum:raise ValueError('Historical runner archive changed')
        data['historical_code'][str(source.resolve()).casefold()+'|'+checksum]=str(target)
    save_registry(data)


def verify_checksum(path,expected):
    historical=registry(str(storage_root()))['historical_code'].get(str(Path(path).resolve()).casefold()+'|'+expected)
    actual=Path(historical) if historical else resolve(path)
    if raw_digest(actual)!=expected:raise ValueError('Protected file changed: '+str(path))


def verify_release(run):
    sidecar=Path(run)/'checkpoint_storage.json'
    if not sidecar.exists():return 0
    metadata=json.loads(sidecar.read_text(encoding='utf-8'))
    count=0
    for relative,checksum in metadata['files'].items():
        verify_checksum(Path(run)/relative,checksum);count+=1
    archive=Path(run)/metadata['archive_file']
    if raw_digest(archive)!=metadata['archive_sha256']:raise ValueError('Checkpoint ZIP backup changed')
    return count


def migrate(run,names):
    """Copy and verify first. Return explicit safe source paths for native removal."""
    run=Path(run).resolve();root=storage_root()
    if root.is_relative_to(run.parent):raise ValueError('Local checkpoint root must be outside Dropbox output')
    metadata_file=run/'checkpoint_storage.json'
    if metadata_file.exists():
        metadata=json.loads(metadata_file.read_text(encoding='utf-8'))
        verify_release(run)
        remaining=[str(run/name) for name in names if (run/name).exists()]
        return dict(release=str(run),files=len(metadata['files']),safe_to_remove=remaining,resumed=True)
    source_files={};destinations={}
    for name in names:
        source=(run/name).resolve()
        if source.parent!=run or not source.is_dir():raise ValueError('Unexpected checkpoint source: '+str(source))
        destination=root/run.name/name
        destinations[name]=str(destination)
        for path in sorted(source.rglob('*')):
            if not path.is_file():continue
            if path.is_symlink():raise ValueError('Checkpoint symlinks are unsupported')
            relative=path.relative_to(run);target=root/run.name/relative;checksum=raw_digest(path)
            target.parent.mkdir(parents=True,exist_ok=True)
            if not target.exists():shutil.copy2(path,target)
            if raw_digest(target)!=checksum:raise ValueError('Local checkpoint copy differs: '+str(relative))
            source_files[str(relative)]=checksum
    archive=run/'checkpoints_backup.zip';temporary=run/'checkpoints_backup.zip.tmp'
    if archive.exists():raise ValueError('Existing backup without relocation manifest')
    with zipfile.ZipFile(temporary,'w',compression=zipfile.ZIP_DEFLATED,compresslevel=1) as zipped:
        for relative in source_files:zipped.write(root/run.name/relative,arcname=Path(relative).as_posix())
    with zipfile.ZipFile(temporary) as zipped:
        if len(zipped.namelist())!=len(source_files):raise ValueError('ZIP checkpoint coverage differs')
        for relative,checksum in source_files.items():
            if hashlib.sha256(zipped.read(Path(relative).as_posix())).hexdigest()!=checksum:
                raise ValueError('ZIP checkpoint checksum differs')
    temporary.replace(archive)
    for name,destination in destinations.items():register_directory(run/name,destination)
    metadata=dict(migrated_at_utc=datetime.datetime.now(datetime.timezone.utc).isoformat(),
                  local_root=str(root/run.name),local_directories=destinations,files=source_files,
                  archive_file=archive.name,archive_sha256=raw_digest(archive),
                  original_manifests_unchanged=True,local_and_archive_checksums_verified=True)
    write_json(metadata_file,metadata)
    verify_release(run)
    return dict(release=str(run),files=len(source_files),safe_to_remove=[str(run/name) for name in names],resumed=False)


def describe_new_checkpoints(run,names):
    """Consolidate new local working files into one portable release backup."""
    run=Path(run);files={};directories={}
    for name in names:
        folder=checkpoint_dir(run,name);directories[name]=str(folder)
        for path in sorted(folder.rglob('*')):
            if path.is_file():files[str(Path(name)/path.relative_to(folder))]=raw_digest(path)
    archive=run/'checkpoints_backup.zip'
    with zipfile.ZipFile(archive,'w',compression=zipfile.ZIP_DEFLATED,compresslevel=1) as zipped:
        for relative in files:
            name=Path(relative).parts[0];zipped.write(Path(directories[name])/Path(relative).relative_to(name),arcname=Path(relative).as_posix())
    write_json(run/'checkpoint_storage.json',dict(local_root=str(storage_root()/canonical_run(run)),
               local_directories=directories,files=files,archive_file=archive.name,archive_sha256=raw_digest(archive)))
    verify_release(run)


def restore(run):
    run=Path(run).resolve()
    metadata=json.loads((run/'checkpoint_storage.json').read_text(encoding='utf-8'))
    archive=run/metadata['archive_file']
    if raw_digest(archive)!=metadata['archive_sha256']:raise ValueError('Checkpoint ZIP backup changed')
    target_root=storage_root()/run.name
    with zipfile.ZipFile(archive) as zipped:
        for relative,checksum in metadata['files'].items():
            part=Path(relative)
            target=(target_root/part).resolve()
            if part.is_absolute() or not target.is_relative_to(target_root.resolve()):
                raise ValueError('Unsafe checkpoint archive path')
            data=zipped.read(part.as_posix())
            if hashlib.sha256(data).hexdigest()!=checksum:raise ValueError('Archived checkpoint changed')
            target.parent.mkdir(parents=True,exist_ok=True)
            if target.exists():
                if raw_digest(target)!=checksum:raise ValueError('Conflicting local checkpoint')
            else:target.write_bytes(data)
    for name in metadata['local_directories']:register_directory(run/name,target_root/name)
    return dict(restored_files=verify_release(run),local_root=str(target_root))


if __name__=='__main__':
    cli=argparse.ArgumentParser(description=__doc__)
    cli.add_argument('command',choices=['migrate','verify','restore'])
    cli.add_argument('--run',type=Path,required=True)
    cli.add_argument('--folders',nargs='*',default=[])
    args=cli.parse_args()
    result=(migrate(args.run,args.folders) if args.command=='migrate' else
            restore(args.run) if args.command=='restore' else dict(checkpoint_files=verify_release(args.run),passed=True))
    print(json.dumps(result,indent=2))
