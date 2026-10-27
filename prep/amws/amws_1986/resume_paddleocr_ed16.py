"""Recover locally; run bounded GPU workers; deliver one verified ZIP on completion."""
from __future__ import annotations

import argparse
from collections import Counter
import hashlib
import json
import logging
import os
from pathlib import Path
import shutil
import subprocess
import sys
import time
import zipfile

import run_paddleocr_ed16 as ocr


def read_json(path):
    return json.loads(path.read_text(encoding="utf-8"))


def sha256(path):
    digest = hashlib.sha256()
    with path.open("rb") as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b""):
            digest.update(chunk)
    return digest.hexdigest()


def recover(source, target):
    """Copy without changing originals; verify every copied byte before reconciliation."""
    source, target = source.resolve(), target.resolve()
    if source == target or source in target.parents or target in source.parents:
        raise ValueError("Recovery source and destination must be separate directories")
    report_path = target / "recovery_copy.json"
    if report_path.exists():
        report = read_json(report_path)
        if report["source"] != str(source) or report["target"] != str(target):
            raise ValueError("Recovery paths changed")
        return
    files = []
    for path in sorted(source.rglob("*")):
        if not path.is_file():
            continue
        relative = path.relative_to(source)
        destination = target / relative
        destination.parent.mkdir(parents=True, exist_ok=True)
        if destination.exists():
            if sha256(path) != sha256(destination):
                raise ValueError(f"Refusing to overwrite different local file: {destination}")
        else:
            shutil.copy2(path, destination)
        digest = sha256(path)
        if digest != sha256(destination):
            raise ValueError(f"Recovery copy verification failed: {relative}")
        files.append(dict(path=relative.as_posix(), size_bytes=path.stat().st_size, sha256=digest))
    ocr.atomic_json(report_path, dict(source=str(source), target=str(target),
                    verified_at=ocr.utc_now(), files=files, file_count=len(files),
                    size_bytes=sum(f["size_bytes"] for f in files)))
    logging.info("Verified recovery copy: %d files, %d bytes", len(files), sum(f["size_bytes"] for f in files))


def inventory(args):
    import pypdfium2 as pdfium
    config = ocr.config_identity(ocr.parse_args([]))
    manifest_path = getattr(args, "selection_manifest", None)
    frozen = read_json(manifest_path) if manifest_path else None
    if frozen:
        frozen_docs = frozen["documents"]
        names = [d["source_file"] for d in frozen_docs]
        if not names or len(names) != len(set(n.casefold() for n in names)):
            raise ValueError("Selection manifest must contain unique documents")
        selected = []
        for item in frozen_docs:
            name = item["source_file"]
            if Path(name).name != name:
                raise ValueError("Manifest PDF must be a filename")
            pdf = ocr.select_pdfs(args.input_dir, name, allow_transcribed=True)[0]
            if ocr.source_identity(pdf) != item["source"] or sha256(pdf) != item["pdf_sha256"]:
                raise ValueError(f"Frozen PDF identity/hash changed: {name}")
            baseline = Path(item["baseline"]["path"])
            if baseline.parent.resolve() != args.input_dir.resolve() or baseline.stem.casefold() != pdf.stem.casefold() or baseline.suffix.casefold() != ".docx":
                raise ValueError(f"Invalid matching Word baseline: {name}")
            if ocr.source_identity(baseline) != item["baseline"] or sha256(baseline) != item["docx_sha256"]:
                raise ValueError(f"Frozen Word baseline identity/hash changed: {name}")
            selected.append(pdf)
    else:
        selected = ocr.select_pdfs(args.input_dir)
    result = []
    for pdf in selected:
        with pdfium.PdfDocument(str(pdf)) as doc:
            item = dict(source=ocr.source_identity(pdf), source_file=pdf.name, page_count=len(doc))
            if frozen:
                expected = next(d for d in frozen_docs if d["source_file"] == pdf.name)
                if len(doc) != expected["page_count"]:
                    raise ValueError(f"Frozen page count changed: {pdf.name}")
                item.update(allow_transcribed=True, baseline=expected["baseline"],
                            pdf_sha256=expected["pdf_sha256"], docx_sha256=expected["docx_sha256"],
                            priority_pages=expected.get("priority_pages", []))
                if any(not isinstance(p, int) or not 1 <= p <= len(doc) for p in item["priority_pages"]):
                    raise ValueError("Priority page outside PDF")
            result.append(item)
    # The recovery plan is for this exact corpus, not a changing DOCX selection.
    if not frozen and (len(result) != 14 or sum(d["page_count"] for d in result) != 1792):
        raise ValueError("Expected the approved 14-PDF, 1792-page corpus")
    if frozen and sum(d["page_count"] for d in result) != frozen["total_pages"]:
        raise ValueError("Frozen manifest total does not match documents")
    prior = args.output_dir / "selection.json"
    if prior.exists() and read_json(prior)["documents"] != result:
        raise ValueError("Selected PDF identities/page counts changed")
    for doc in result:
        manifest = args.output_dir / Path(doc["source_file"]).stem / "manifest.json"
        if manifest.exists():
            data = read_json(manifest)
            if data["source"] != doc["source"] or data["config"] != config:
                raise ValueError(f"Source or OCR settings changed: {doc['source_file']}")
    ocr.atomic_json(prior, dict(selected_at=ocr.utc_now(), documents=result,
                    total_pages=sum(d["page_count"] for d in result),
                    selection_manifest_sha256=sha256(manifest_path) if manifest_path else None))
    return result, config


def valid(root, doc, page, config):
    folder = root / Path(doc["source_file"]).stem
    if not ocr.checkpoint_valid(folder, page, config):
        return False
    record = read_json(folder / f"page_{page:04d}.json")
    return (record["source_path"] == doc["source"]["path"]
            and record["pdf_page_count"] == doc["page_count"])


def reconcile(root, docs, config):
    """Invalidate false successes locally and account for every page before resume."""
    report = dict(checked_at=ocr.utc_now(), counts=Counter(), documents=[])
    for doc in docs:
        folder = root / Path(doc["source_file"]).stem
        folder.mkdir(exist_ok=True)
        path = folder / "manifest.json"
        manifest = read_json(path) if path.exists() else dict(source=doc["source"], config=config,
                    page_count=doc["page_count"], created_at=ocr.utc_now(), pages={})
        counts = Counter()
        for page in range(1, doc["page_count"] + 1):
            key = str(page)
            previous = manifest["pages"].get(key, {})
            if valid(root, doc, page, config):
                counts["retained"] += 1
                record = read_json(folder / f"page_{page:04d}.json")
                manifest["pages"][key] = {k: record[k] for k in
                    ("status", "elapsed_seconds", "gpu_memory_used_mib", "text_characters")}
            elif previous.get("status") == "ok":
                counts["invalid_success"] += 1
                manifest["pages"][key] = dict(status="failed", failed_at=ocr.utc_now(),
                    error="Invalid or empty legacy checkpoint; fresh OCR required", previous=previous)
            elif previous.get("status") == "failed":
                counts["failed"] += 1
            else:
                counts["untouched"] += 1
        manifest["summary"] = ocr.page_accounting(manifest, doc["source_file"], {})
        ocr.atomic_json(path, manifest)
        report["counts"].update(counts)
        report["documents"].append(dict(source_file=doc["source_file"], **counts))
    if not (root / "recovery_audit.json").exists():
        ocr.atomic_json(root / "recovery_audit.json", report)
    logging.info("Local checkpoint audit: %s", dict(report["counts"]))
    return report


def progress(root, docs, config, state, **extra):
    documents = []
    for doc in docs:
        folder = root / Path(doc["source_file"]).stem
        manifest = read_json(folder / "manifest.json")
        summary = ocr.page_accounting(manifest, doc["source_file"], {})
        documents.append(summary)
    record = dict(updated_at=ocr.utc_now(), state=state, total_pages=sum(d["page_count"] for d in docs),
                  completed_pages=sum(d["completed_pages"] for d in documents),
                  failed_pages=sum(len(d["failed_pages"]) for d in documents),
                  pending_pages=sum(d["pending_pages"] for d in documents),
                  documents=documents, config=config, **extra)
    ocr.atomic_json(root / "run_summary.json", record)
    return record


def worker(root, input_dir, doc, first, count):
    command = [sys.executable, "-u", str(Path(ocr.__file__).resolve()),
               "--input-dir", str(input_dir), "--output-dir", str(root),
               "--pdf", doc["source_file"], "--start-page", str(first),
               "--limit-pages", str(count), "--resume", "--worker"]
    if doc.get("allow_transcribed"):
        command.append("--allow-transcribed")
    override_path = root / "page_geometry_overrides.json"
    if override_path.exists():
        override = read_json(override_path).get("pages", {}).get(f"{doc['source_file']}:{first}")
        if override:
            if count != 1 or override["source"] != doc["source"]:
                raise ValueError("Page geometry override requires matching source and a single-page worker")
            if override.get('layout_review_path'):
                command.extend(['--layout-review',override['layout_review_path']])
            else:
                command.extend(["--gutter-points", json.dumps(override["gutter_points_yx"], separators=(",", ":"))])
    logging.info("Fresh worker: %s pages %d-%d", doc["source_file"], first, first + count - 1)
    started = time.monotonic()
    with (root / "workers_stdout.log").open("a", encoding="utf-8") as stdout, \
         (root / "workers_stderr.log").open("a", encoding="utf-8") as stderr:
        process = subprocess.Popen(command, stdout=stdout, stderr=stderr,
                    creationflags=getattr(subprocess, "CREATE_NO_WINDOW", 0))
        ocr.atomic_json(root / "active_worker.json", dict(pid=process.pid, command=command,
                    started_at=ocr.utc_now(), source_file=doc["source_file"], first_page=first, page_count=count))
        try:
            code = process.wait(timeout=1200)
        except subprocess.TimeoutExpired:
            # Windows venv launchers can have a Python child; terminate the entire
            # process tree of this still-live owned worker, never a stale PID.
            subprocess.run(["taskkill", "/PID", str(process.pid), "/T", "/F"],
                           capture_output=True, creationflags=getattr(subprocess, "CREATE_NO_WINDOW", 0))
            process.wait(timeout=30)
            code = -1
    ocr.atomic_json(root / "active_worker.json", dict(finished_at=ocr.utc_now(),
                    exit_code=code, source_file=doc["source_file"], first_page=first, page_count=count,
                    elapsed_seconds=round(time.monotonic() - started, 3)))
    return code


def run_range(root, input_dir, doc, first, count, config, docs):
    pages = range(first, first + count)
    if all(valid(root, doc, p, config) for p in pages):
        return
    overrides_path = root / "page_geometry_overrides.json"
    if count > 1 and overrides_path.exists():
        overrides = read_json(overrides_path).get("pages", {})
        if any(f"{doc['source_file']}:{p}" in overrides for p in pages):
            for page in pages:
                run_range(root, input_dir, doc, page, 1, config, docs)
            return
    progress(root, docs, config, "running", current_file=doc["source_file"], current_page=first)
    worker(root, input_dir, doc, first, count)
    missing = [p for p in pages if not valid(root, doc, p, config)]
    if missing and count > 1:
        smaller = 2 if count > 2 else 1
        for start in range(first, first + count, smaller):
            run_range(root, input_dir, doc, start, min(smaller, first + count - start), config, docs)
    elif missing:
        # A single-page worker gets one additional clean retry; failures remain
        # explicit rather than looping forever or silently accepting empty output.
        worker(root, input_dir, doc, first, 1)
        if not valid(root, doc, first, config):
            folder = root / Path(doc["source_file"]).stem
            path = folder / "manifest.json"
            manifest = read_json(path)
            manifest["pages"][str(first)] = dict(status="failed", failed_at=ocr.utc_now(),
                error="Persistent failure after fresh single-page retries",
                last_attempt=manifest["pages"].get(str(first)))
            ocr.atomic_json(path, manifest)
            logging.error("Persistent failure: %s page %d", doc["source_file"], first)
    progress(root, docs, config, "running")


def verify_complete(root, docs, config):
    failures = []
    blanks = []
    for doc in docs:
        if ocr.source_identity(Path(doc["source"]["path"])) != doc["source"]:
            raise ValueError(f"Source changed while processing: {doc['source_file']}")
        if doc.get("pdf_sha256"):
            if sha256(Path(doc["source"]["path"])) != doc["pdf_sha256"]:
                raise ValueError("Frozen PDF hash changed")
            baseline = Path(doc["baseline"]["path"])
            if ocr.source_identity(baseline) != doc["baseline"] or sha256(baseline) != doc["docx_sha256"]:
                raise ValueError("Frozen Word baseline changed")
        for page in range(1, doc["page_count"] + 1):
            if not valid(root, doc, page, config):
                failures.append(dict(source_file=doc["source_file"], page=page))
            elif read_json(root / Path(doc["source_file"]).stem / f"page_{page:04d}.json")["status"] == "verified_blank":
                blanks.append(dict(source_file=doc["source_file"], page=page))
        ocr.document_exports(root / Path(doc["source_file"]).stem, doc["page_count"], doc["source_file"])
    total = sum(d["page_count"] for d in docs)
    report = dict(verified_at=ocr.utc_now(), total_pages=total, documents=len(docs),
                  valid_pages=total - len(failures), failures=failures, complete=not failures)
    report["verified_blank_pages"] = blanks
    report["nonempty_ocr_pages"] = total - len(failures) - len(blanks)
    ocr.atomic_json(root / "completion_verification.json", report)
    return report


def deliver(root, destination):
    """Create and CRC/hash verify locally, then copy a single archive to Dropbox."""
    destination.mkdir(parents=True, exist_ok=True)
    archive = root.parent / "amws_ed16_complete.zip"
    temporary = archive.with_suffix(".zip.tmp")
    with zipfile.ZipFile(temporary, "w", compression=zipfile.ZIP_DEFLATED, compresslevel=6) as z:
        for path in sorted(root.rglob("*")):
            if path.is_file() and path.name not in (".batch.lock", ".ocr.lock"):
                z.write(path, "amws_ed16/" + path.relative_to(root).as_posix())
    with zipfile.ZipFile(temporary) as z:
        damaged = z.testzip()
        if damaged:
            raise ValueError(f"ZIP integrity failure: {damaged}")
        members = len(z.infolist())
    temporary.replace(archive)
    digest = sha256(archive)
    # Digest-specific name permits safe resume without replacing a different delivery.
    target = destination / f"amws_ed16_complete_{digest[:16]}.zip"
    if target.exists():
        if sha256(target) != digest:
            raise ValueError("Existing Dropbox archive has different content")
    else:
        partial = target.with_suffix(".zip.partial")
        shutil.copy2(archive, partial)
        if sha256(partial) != digest:
            raise ValueError("Dropbox copy hash mismatch")
        partial.replace(target)
    ocr.atomic_json(root / "delivery.json", dict(delivered_at=ocr.utc_now(), local_zip=str(archive),
                    dropbox_zip=str(target), sha256=digest, members=members, size_bytes=archive.stat().st_size))
    logging.info("Verified ZIP delivered: %s", target)


def main():
    defaults = ocr.parse_args([])
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--output-dir", type=Path, default=defaults.output_dir)
    parser.add_argument("--input-dir", type=Path, default=defaults.input_dir)
    parser.add_argument("--recover-from", type=Path)
    parser.add_argument("--selection-manifest", type=Path,
                        help="Frozen PDF/Word pairs for an explicit OCR comparison")
    parser.add_argument("--no-deliver", action="store_true", help="Verify locally; package with a separate comparison audit")
    parser.add_argument("--prepare-only", action="store_true")
    parser.add_argument("--verify-only", action="store_true")
    parser.add_argument("--dropbox-dir", type=Path, default=defaults.input_dir.parents[1] / "output" / "amws")
    args = parser.parse_args()
    if args.selection_manifest and not args.no_deliver:
        parser.error("--selection-manifest requires --no-deliver; package with the comparison audit")
    args.output_dir = args.output_dir.resolve()
    if "dropbox" in str(args.output_dir).casefold():
        raise ValueError("Worker output must be local, outside Dropbox")
    args.output_dir.mkdir(parents=True, exist_ok=True)
    # The controller holds its own lock; each sequential child holds the OCR lock.
    with ocr.output_lock(args.output_dir, ".batch.lock"):
        if args.recover_from:
            recover(args.recover_from, args.output_dir)
        logging.basicConfig(level=logging.INFO, format="%(asctime)s %(levelname)s %(message)s",
            handlers=[logging.FileHandler(args.output_dir / "controller.log", encoding="utf-8"), logging.StreamHandler()], force=True)
        docs, config = inventory(args)
        reconcile(args.output_dir, docs, config)
        progress(args.output_dir, docs, config, "prepared")
        if args.prepare_only:
            return 0
        ocr.atomic_json(args.output_dir / "controller_job.json", dict(pid=os.getpid(), started_at=ocr.utc_now(),
                    command=sys.argv, max_worker_pages=10, fallback_worker_pages=[2, 1]))
        if not args.verify_only:
            for doc in docs:
                for page in doc.get("priority_pages", []):
                    run_range(args.output_dir, args.input_dir, doc, page, 1, config, docs)
            for doc in docs:
                for first in range(1, doc["page_count"] + 1, 10):
                    run_range(args.output_dir, args.input_dir, doc, first,
                              min(10, doc["page_count"] - first + 1), config, docs)
        report = verify_complete(args.output_dir, docs, config)
        if not report["complete"]:
            progress(args.output_dir, docs, config, "failed", unresolved_pages=report["failures"])
            logging.error("Completion withheld: %d unresolved pages; no Dropbox delivery", len(report["failures"]))
            return 1
        progress(args.output_dir, docs, config, "verified", finished_at=ocr.utc_now())
        if args.no_deliver:
            return 0
        if args.selection_manifest:
            raise ValueError("Comparison manifests require --no-deliver; use audited comparison packaging")
        deliver(args.output_dir, args.dropbox_dir)
        progress(args.output_dir, docs, config, "delivered", finished_at=ocr.utc_now(),
                 delivery=read_json(args.output_dir / "delivery.json"))
        return 0


if __name__ == "__main__":
    try:
        sys.exit(main())
    except Exception:
        logging.exception("Local recovery/controller failed; inspect logs before restarting")
        sys.exit(1)
