"""Page-at-a-time raw OCR for Ed16 biography PDFs without matching DOCX files.

Run --list first; use --pdf NAME --limit-pages 5 --save-images for a pilot,
then --resume for the full batch. No biography parsing or source edits.
"""
from __future__ import annotations

import argparse
import hashlib
from collections import Counter
from contextlib import contextmanager
from datetime import datetime, timezone
from importlib.metadata import version
import json
import logging
import os
from pathlib import Path
import subprocess
import sys
import time

SCHEMA_VERSION = 3
MODEL_SETTINGS = dict(
    text_detection_model_name="PP-OCRv5_mobile_det",
    text_recognition_model_name="en_PP-OCRv5_mobile_rec",
    text_det_limit_side_len=3200, text_det_limit_type="max",
    text_det_unclip_ratio=0.7, text_det_box_thresh=0.4,
    use_doc_orientation_classify=False, use_doc_unwarping=True,
    use_textline_orientation=False, use_table_recognition=False,
    use_formula_recognition=False, use_chart_recognition=False,
    use_seal_recognition=False, text_recognition_batch_size=1,
    markdown_ignore_labels=[],
)
ONNX_SETTINGS = {"onnxruntime": dict(providers=["CUDAExecutionProvider"],
    provider_options=[dict(arena_extend_strategy="kSameAsRequested", cudnn_conv_use_max_workspace="0")],
    log_severity_level=3)}


def utc_now():
    return datetime.now(timezone.utc).isoformat()


def atomic_text(path, text):
    path = Path(path)
    tmp = path.with_name(path.name + ".tmp")
    tmp.write_text(text, encoding="utf-8")
    # Dropbox can briefly hold the destination during synchronization on Windows.
    for attempt in range(7):
        try:
            tmp.replace(path)
            return
        except PermissionError:
            if attempt == 6:
                raise
            time.sleep(min(0.1 * 2 ** attempt, 2))


def atomic_json(path, value):
    atomic_text(path, json.dumps(value, ensure_ascii=False, indent=2) + "\n")


def select_pdfs(input_dir, pdf_name=None, allow_transcribed=False):
    if allow_transcribed and not pdf_name:
        raise ValueError("Transcribed OCR requires one explicit PDF filename")
    files = list(Path(input_dir).iterdir())
    word_stems = {p.stem.casefold() for p in files if p.suffix.casefold() == ".docx"}
    selected = sorted((p.resolve() for p in files if p.is_file()
                       and p.suffix.casefold() == ".pdf"
                       and not p.stem.casefold().startswith("16_index_")
                       and (allow_transcribed or p.stem.casefold() not in word_stems)), key=lambda p: p.name)
    if pdf_name:
        selected = [p for p in selected if p.name.casefold() == pdf_name.casefold()]
        if not selected:
            raise ValueError(f"{pdf_name!r} is not an eligible biography PDF for this selection mode")
    return selected


def source_identity(pdf):
    stat = pdf.stat()
    return dict(path=str(pdf.resolve()), size_bytes=stat.st_size, mtime_ns=stat.st_mtime_ns)


def config_identity(args):
    runtime = "onnxruntime-gpu" if args.engine == "onnxruntime" else "paddlepaddle-gpu"
    return dict(schema_version=SCHEMA_VERSION, dpi=args.dpi, device=args.device, engine=args.engine,
                column_mode="single" if args.single_column else "two",
                model_settings=MODEL_SETTINGS,
                engine_settings=ONNX_SETTINGS if args.engine == "onnxruntime" and args.device.startswith("gpu") else {},
                packages={p: version(p) for p in ("paddleocr", "paddlex", runtime, "pypdfium2")})


def checkpoint_valid(folder, page, config):
    stem = f"page_{page:04d}"
    try:
        data = json.loads((folder / f"{stem}.json").read_text(encoding="utf-8"))
        text = (folder / f"{stem}.txt").read_text(encoding="utf-8")
        markdown = (folder / f"{stem}.md").read_text(encoding="utf-8")
        expected_config = config
        if data.get('layout_review'):
            if not layout_review_valid(data['layout_review'],Path(data['source_path']),page):
                return False
            if len(data['prediction'].get('column_predictions',[])) != 1 or data['prediction'].get('gutter_x') is not None:
                return False
            expected_config = dict(config,column_mode='single')
        if data.get("status") == "verified_blank":
            review = data["blank_verification"]
            review_image = folder.parent / review["review_image"]
            marker = f"[VISUALLY VERIFIED BLANK PAGE | {data['source_file']} | PDF page {page}]\n"
            return (data["config"] == config and data["pdf_page_number"] == page
                    and data["source_file"].casefold() == (folder.name + ".pdf").casefold()
                    and (review.get("user_approved") is True or (
                        review.get("review_authorization") == "task_authorized_visual_review"
                        and bool(review.get("reviewer")) and bool(review.get("notes"))))
                    and review["source"] == source_identity(Path(data["source_path"]))
                    and hashlib.sha256(review_image.read_bytes()).hexdigest() == review["review_image_sha256"]
                    and data["text_characters"] == 0
                    and data["prediction"]["parsing_res_list"] == []
                    and text == marker and markdown == marker)
        return (data["status"] == "ok" and data["config"] == expected_config
                and data["pdf_page_number"] == page
                and data["source_file"] == folder.name + ".pdf"
                and text.strip() and markdown.strip()
                and data["text_characters"] == len(text.strip())
                and text == ordered_text(data["prediction"])
                and all(ordered_text(c["prediction"]).strip()
                        for c in data["prediction"].get("column_predictions", [])))
    except (OSError, ValueError, KeyError, TypeError):
        return False


def layout_review_valid(review, pdf, page):
    return (review.get('layout') == 'single_column_nonbiography'
            and review.get('review_authorization') == 'task_authorized_visual_review'
            and bool(review.get('reviewer')) and bool(review.get('notes'))
            and review.get('source') == source_identity(pdf)
            and review.get('pdf_page_number') == page
            and hashlib.sha256(Path(review['review_image']).read_bytes()).hexdigest()
                == review.get('review_image_sha256'))


def prediction_payload(result):
    value = result.json
    if isinstance(value, str):
        value = json.loads(value)
    # JSON round trip isolates Paddle's serializable representation from live arrays.
    value = json.loads(json.dumps(value, ensure_ascii=False))
    return value.get("res", value)


def ordered_text(payload):
    blocks = payload.get("parsing_res_list")
    if blocks is None:
        raise ValueError("PP-StructureV3 result has no ordered parsing_res_list")
    return "\n\n".join(str(b.get("block_content", "")) for b in blocks).rstrip() + "\n"


def column_gutter(image, np):
    """Find the blank gutter between this edition's two biography columns."""
    height, width = image.shape[:2]
    ink = np.min(image[int(height * .1):int(height * .9)], axis=2) < 110
    density = ink.mean(axis=0)
    window = max(5, int(width * .008))
    smooth = np.convolve(density, np.ones(window) / window, mode="same")
    low, high = int(width * .35), int(width * .65)
    best = low + int(np.argmin(smooth[low:high]))
    if smooth[best] > .02:
        raise ValueError("No clear two-column gutter; inspect the page and retry with --single-column if appropriate")
    # Center the blank band rather than cutting at its first quiet pixel.
    left = right = best
    threshold = min(.02, float(smooth[best]) + .005)
    while left > low and smooth[left - 1] <= threshold:
        left -= 1
    while right < high - 1 and smooth[right + 1] <= threshold:
        right += 1
    return (left + right) // 2


class ColumnPipeline:
    """Keep text detection from merging lines across the narrow column gutter."""
    def __init__(self, structure, preprocessor, single_column=False, gutter_points=None):
        self.structure, self.preprocessor = structure, preprocessor
        self.single_column = single_column
        self.gutter_points = gutter_points

    def predict(self, input):
        import copy
        import numpy as np

        if self.preprocessor is not None:
            preprocessing = self.preprocessor.predict(input=input)[0]
            image = preprocessing["output_img"]
            preprocessing_json = prediction_payload(preprocessing)
        else:
            image, preprocessing_json = input, {}
        height, width = image.shape[:2]
        self.last_coordinate_image = image
        trace = None
        if self.gutter_points:
            # Page-specific review points follow curved gutters. Overlapping crops
            # retain full image coordinates; the opposite column is masked white.
            ys, xs = zip(*self.gutter_points)
            if not (ys[0] == 0 and ys[-1] == height and all(0 < x < width for x in xs)
                    and all(a < b for a, b in zip(ys, ys[1:]))):
                raise ValueError("Reviewed gutter points do not match the unwarped image")
            trace = np.rint(np.interp(np.arange(height), ys, xs)).astype(int)
            gutter = int(np.median(trace))
            bounds = [(0, int(trace.max())), (int(trace.min()), width)]
        else:
            gutter = None if self.single_column else column_gutter(image, np)
            bounds = [(0, width)] if gutter is None else [(0, gutter), (gutter, width)]
        columns, blocks, markdown = [], [], []
        for column_no, (x1, x2) in enumerate(bounds, 1):
            crop = image[:, x1:x2].copy()
            if trace is not None:
                columns_x = np.arange(x1, x2)[None, :]
                mask = columns_x >= trace[:, None] if column_no == 1 else columns_x < trace[:, None]
                crop[mask] = 255
            predictions = self.structure.predict(input=crop)
            if len(predictions) != 1:
                raise ValueError("Expected one result per column")
            result = predictions[0]
            raw = prediction_payload(result)
            if not ordered_text(raw).strip():
                raise ValueError(f"Empty OCR in column {column_no}; restart the worker")
            columns.append(dict(column_number=column_no, x_offset=x1,
                                coordinate_space="column_crop", prediction=raw))
            for raw_block in raw["parsing_res_list"]:
                block = copy.deepcopy(raw_block)
                block["block_bbox"][0] += x1
                block["block_bbox"][2] += x1
                block["column_number"] = column_no
                blocks.append(block)
            markdown.append(result.markdown["markdown_texts"])
        # Page headers precede the text even when printed above the right column.
        blocks = ([b for b in blocks if b["block_label"] == "header"]
                  + [b for b in blocks if b["block_label"] not in ("header", "footer")]
                  + [b for b in blocks if b["block_label"] == "footer"])
        for idx, block in enumerate(blocks, 1):
            block["block_order"] = idx
        payload = dict(width=width, height=height, gutter_x=gutter,
                       parsing_res_list=blocks, column_predictions=columns,
                       doc_preprocessor_res=preprocessing_json)
        if self.gutter_points:
            payload["reviewed_gutter_points_yx"] = self.gutter_points
            payload["gutter_method"] = "page_specific_visual_review"
        class PageResult(dict):
            pass
        composite = PageResult(doc_preprocessor_res={"output_img": image})
        composite.json = {"res": payload}
        composite.markdown = {"markdown_texts": "\n\n".join(markdown)}
        return [composite]


def gpu_memory(device):
    if not device.startswith("gpu"):
        return None
    gpu_id = device.split(":")[-1] if ":" in device else "0"
    try:
        value = subprocess.check_output(
            ["nvidia-smi", "-i", gpu_id, "--query-gpu=memory.used",
             "--format=csv,noheader,nounits"], text=True, timeout=10,
            creationflags=getattr(subprocess, "CREATE_NO_WINDOW", 0))
        return int(value.strip())
    except (OSError, ValueError, subprocess.SubprocessError):
        return None


def verify_onnx_sessions(pipeline, gpu_required):
    """Check actual sessions, since ORT can silently fall back to CPU."""
    seen, sessions = set(), []

    def visit(obj):
        if id(obj) in seen:
            return
        seen.add(id(obj))
        module = type(obj).__module__
        if module.startswith("onnxruntime") and hasattr(obj, "get_providers"):
            providers = obj.get_providers()
            if gpu_required and "CUDAExecutionProvider" not in providers:
                raise RuntimeError(f"Model fell back to CPU: {getattr(obj, '_model_path', None)}")
            sessions.append(dict(model_path=str(getattr(obj, "_model_path", "")), providers=providers))
        elif isinstance(obj, dict):
            for child in obj.values():
                visit(child)
        elif isinstance(obj, (list, tuple)):
            for child in obj:
                visit(child)
        elif module.startswith(("paddlex", "paddleocr")) and hasattr(obj, "__dict__"):
            for child in vars(obj).values():
                visit(child)

    visit(pipeline)
    if not sessions:
        raise RuntimeError("Could not verify any ONNX model sessions")
    return sessions


@contextmanager
def output_lock(root, name=".ocr.lock"):
    # OS-managed lock releases even if the OCR process crashes.
    import msvcrt
    with (root / name).open("a+b") as handle:
        if handle.tell() == 0:
            handle.write(b"0")
            handle.flush()
        handle.seek(0)
        try:
            msvcrt.locking(handle.fileno(), msvcrt.LK_NBLCK, 1)
        except OSError as exc:
            raise RuntimeError("Another OCR process is using this output directory") from exc
        try:
            yield
        finally:
            handle.seek(0)
            msvcrt.locking(handle.fileno(), msvcrt.LK_UNLCK, 1)


def document_exports(folder, page_count, source_file):
    # Include explicit gaps; never silently concatenate around missing/failed pages.
    for ext in ("txt", "md"):
        parts = []
        for page in range(1, page_count + 1):
            path = folder / f"page_{page:04d}.{ext}"
            marker = f"[{source_file} | PDF page {page}]"
            text = path.read_text(encoding="utf-8") if path.is_file() else "[PAGE NOT PROCESSED OR FAILED]\n"
            parts.append(marker + "\n" + text)
        atomic_text(folder / f"document.{ext}", "\n".join(parts))


def page_accounting(manifest, source_file, counts):
    return dict(source_file=source_file, page_count=manifest["page_count"],
                completed_pages=sum(p.get("status") in ("ok", "verified_blank") for p in manifest["pages"].values()),
                verified_blank_pages=sorted(int(k) for k, v in manifest["pages"].items() if v["status"] == "verified_blank"),
                failed_pages=sorted(int(k) for k, v in manifest["pages"].items() if v["status"] == "failed"),
                pending_pages=manifest["page_count"] - len(manifest["pages"]), this_run=dict(counts))


def run_pdf(pdf, pipeline, args, config, pdfium, np):
    folder = args.output_dir / pdf.stem
    folder.mkdir(parents=True, exist_ok=True)
    manifest_path = folder / "manifest.json"
    identity = source_identity(pdf)
    if manifest_path.exists():
        manifest = json.loads(manifest_path.read_text(encoding="utf-8"))
        if manifest["source"] != identity or manifest["config"] != config:
            raise ValueError(f"Source/config changed for {pdf.name}; use a new output directory")
        if not args.resume:
            raise ValueError(f"Existing output for {pdf.name}; use --resume")
    else:
        manifest = dict(source=identity, config=config, created_at=utc_now(), pages={})
    doc = pdfium.PdfDocument(str(pdf))
    counts = Counter()
    try:
        page_count = len(doc)
        manifest["page_count"] = page_count
        atomic_json(manifest_path, manifest)
        stop = min(page_count, args.start_page - 1 + args.limit_pages) if args.limit_pages else page_count
        if args.start_page > page_count:
            raise ValueError(f"Start page exceeds {pdf.name}'s {page_count} pages")
        for idx in range(args.start_page - 1, stop):
            page_num = idx + 1
            stem = f"page_{page_num:04d}"
            previous = manifest["pages"].get(str(page_num), {})
            if args.resume and checkpoint_valid(folder, page_num, config):
                if previous.get("status") not in ("ok", "verified_blank"):
                    previous = json.loads((folder / f"{stem}.json").read_text(encoding="utf-8"))
                    manifest["pages"][str(page_num)] = {k: previous[k] for k in
                        ("status", "elapsed_seconds", "gpu_memory_used_mib", "text_characters")}
                    atomic_json(manifest_path, manifest)
                counts["resumed"] += 1
                logging.info("%s page %d/%d: resumed", pdf.name, page_num, page_count)
                continue
            started = time.perf_counter()
            page = bitmap = image = None
            try:
                layout_review = None
                page_config = config
                if getattr(args,'layout_review',None):
                    layout_review = json.loads(args.layout_review.read_text(encoding='utf-8'))
                    if not layout_review_valid(layout_review,pdf,page_num):
                        raise ValueError('Single-column layout review missing, stale or for another page')
                    page_config = dict(config,column_mode='single')
                    pipeline.single_column = True
                    pipeline.gutter_points = None
                # Remove artifacts from an incomplete prior attempt before retrying.
                for ext in ("json", "txt", "md"):
                    (folder / f"{stem}.{ext}").unlink(missing_ok=True)
                page = doc[idx]
                bitmap = page.render(scale=args.dpi / 72)
                image = bitmap.to_pil().convert("RGB")
                if args.save_images:
                    image.save(folder / f"{stem}.png")
                results = list(pipeline.predict(input=np.array(image)[:, :, ::-1].copy()))
                if len(results) != 1:
                    raise ValueError(f"Expected one prediction for one page, got {len(results)}")
                result = results[0]
                payload = prediction_payload(result)
                coordinate_image = (result["doc_preprocessor_res"]["output_img"]
                                    if config.get("model_settings", {}).get("use_doc_unwarping") else None)
                if args.save_images and coordinate_image is not None:
                    from PIL import Image
                    with Image.fromarray(coordinate_image[:, :, ::-1]) as corrected:
                        corrected.save(folder / f"{stem}_unwarped.png")
                text = ordered_text(payload)
                markdown = result.markdown["markdown_texts"]
                if not text.strip() or not markdown.strip():
                    raise ValueError("Empty OCR output; page must be retried in a fresh worker")
                elapsed = round(time.perf_counter() - started, 3)
                stats = dict(status="ok", elapsed_seconds=elapsed,
                             gpu_memory_used_mib=gpu_memory(args.device), text_characters=len(text.strip()))
                record = dict(schema_version=SCHEMA_VERSION, source_file=pdf.name,
                              source_path=str(pdf), pdf_page_number=page_num,
                              pdf_page_count=page_count, rendered_width=image.width,
                              rendered_height=image.height, dpi=args.dpi,
                              box_coordinate_space="unwarped_page" if coordinate_image is not None else "rendered_page",
                              box_coordinate_width=int(coordinate_image.shape[1]) if coordinate_image is not None else image.width,
                              box_coordinate_height=int(coordinate_image.shape[0]) if coordinate_image is not None else image.height,
                              completed_at=utc_now(), config=page_config, **stats,
                              prediction=payload)
                if layout_review:
                    record['layout_review'] = layout_review
                atomic_text(folder / f"{stem}.txt", text)
                atomic_text(folder / f"{stem}.md", markdown + "\n")
                # JSON is the final commit marker after both readable exports exist.
                atomic_json(folder / f"{stem}.json", record)
                manifest["pages"][str(page_num)] = stats
                counts["processed"] += 1
                logging.info("%s page %d/%d: %.1fs, %d chars, GPU %s MiB", pdf.name,
                             page_num, page_count, elapsed, len(text.strip()), stats["gpu_memory_used_mib"])
            except Exception as exc:
                # Preserve the actual straightened coordinate frame on failure
                # so a scan-bound gutter review can be made without guessing.
                if hasattr(pipeline, "last_coordinate_image"):
                    from PIL import Image
                    failure_dir = args.output_dir / "failure_images"
                    failure_dir.mkdir(exist_ok=True)
                    with Image.fromarray(pipeline.last_coordinate_image[:, :, ::-1]) as failed_image:
                        failed_image.save(failure_dir / f"{pdf.stem}_{stem}_unwarped.png")
                for ext in ("json", "txt", "md"):
                    (folder / f"{stem}.{ext}").unlink(missing_ok=True)
                counts["failed"] += 1
                manifest["pages"][str(page_num)] = dict(status="failed", error=f"{type(exc).__name__}: {exc}",
                    failed_at=utc_now(), elapsed_seconds=round(time.perf_counter() - started, 3))
                logging.exception("%s page %d failed", pdf.name, page_num)
            finally:
                if image is not None:
                    image.close()
                if bitmap is not None:
                    bitmap.close()
                if page is not None:
                    page.close()
                manifest["updated_at"] = utc_now()
                manifest["summary"] = page_accounting(manifest, pdf.name, counts)
                atomic_json(manifest_path, manifest)
            if getattr(args, "worker", False) and counts["failed"]:
                # An allocation failure can poison later predictions. Exit immediately;
                # the controller retries unfinished pages in a fresh, smaller worker.
                break
        summary = page_accounting(manifest, pdf.name, counts)
        manifest["summary"] = summary
        atomic_json(manifest_path, manifest)
        return summary
    finally:
        doc.close()
        if "page_count" in manifest:
            document_exports(folder, manifest["page_count"], pdf.name)


def parse_args(argv=None):
    root = Path(os.environ.get("TALENT_DETS_DATA_DIR",
                str(Path.home() / "Globtalent Dropbox" / "gtl_talent_dets")))
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--input-dir", type=Path, default=root / "Data" / "amws ed 16")
    parser.add_argument("--output-dir", type=Path, default=Path.home() / "ocr_runs" / "amws_ed16")
    parser.add_argument("--worker", action="store_true", help=argparse.SUPPRESS)
    parser.add_argument("--gutter-points", help="Reviewed gutter as JSON [[y,x],...], for one page only")
    parser.add_argument('--layout-review',type=Path,
                        help='Source/image-bound visual review authorizing full-page OCR of one non-biography page')
    parser.add_argument("--device", default="gpu:0")
    parser.add_argument("--engine", choices=("paddle", "onnxruntime"), default="onnxruntime",
                        help="PP-StructureV3 inference engine")
    parser.add_argument("--pdf", help="One eligible source filename, including .pdf")
    parser.add_argument("--allow-transcribed", action="store_true",
                        help="Allow a same-stem DOCX for one explicitly named biography PDF")
    parser.add_argument("--limit-pages", type=int, help="Maximum pages per selected PDF")
    parser.add_argument("--start-page", type=int, default=1, help="One-based PDF page to start at")
    parser.add_argument("--dpi", type=int, default=300)
    parser.add_argument("--det-limit-side-len", type=int, default=3200,
                        help="Maximum detection image side; recognition uses full-resolution crops")
    parser.add_argument("--no-unwarp", action="store_true", help="Disable page straightening (enabled by default)")
    parser.add_argument("--single-column", action="store_true", help="Process the full page instead of splitting the two biography columns")
    parser.add_argument("--resume", action="store_true", help="Skip completed pages and retry failures")
    parser.add_argument("--save-images", action="store_true", help="Keep rendered PNGs for visual QA")
    parser.add_argument("--list", action="store_true", help="List selected PDFs without loading models or writing")
    args = parser.parse_args(argv)
    if args.allow_transcribed and not args.pdf:
        parser.error("--allow-transcribed requires --pdf")
    if args.gutter_points:
        args.gutter_points = json.loads(args.gutter_points)
        if not args.pdf or args.limit_pages != 1 or args.single_column:
            parser.error("Reviewed gutter points require one PDF and exactly one page")
    if args.layout_review and (not args.pdf or args.limit_pages != 1 or args.single_column or args.gutter_points):
        parser.error('--layout-review requires one explicit PDF/page and no other geometry override')
    if args.start_page < 1 or args.dpi < 1 or args.det_limit_side_len < 1 or (args.limit_pages is not None and args.limit_pages < 1):
        parser.error("start-page, dpi, det-limit-side-len and limit-pages must be positive")
    return args


def main(argv=None):
    args = parse_args(argv)
    MODEL_SETTINGS["text_det_limit_side_len"] = args.det_limit_side_len
    MODEL_SETTINGS["use_doc_unwarping"] = not args.no_unwarp
    selected = select_pdfs(args.input_dir, args.pdf, args.allow_transcribed)
    if not selected:
        print("No eligible untranscribed biography PDFs found.")
        return 0
    if args.list:
        print("\n".join(p.name for p in selected))
        print(f"Selected {len(selected)} PDFs")
        return 0
    args.output_dir = args.output_dir.resolve()
    args.output_dir.mkdir(parents=True, exist_ok=True)
    with output_lock(args.output_dir):
        logging.basicConfig(level=logging.INFO, format="%(asctime)s %(levelname)s %(message)s",
            handlers=[logging.FileHandler(args.output_dir / "ocr.log", encoding="utf-8"), logging.StreamHandler()])
        import numpy as np
        import pypdfium2 as pdfium
        if args.engine == "paddle":
            import paddle
            if args.device.startswith("gpu") and (not paddle.is_compiled_with_cuda() or paddle.device.cuda.device_count() < 1):
                raise RuntimeError("GPU requested but Paddle cannot use CUDA; run paddle.utils.run_check()")
        else:
            import onnxruntime as ort
            if args.device.startswith("gpu"):
                ort.preload_dlls(directory="")
                if "CUDAExecutionProvider" not in ort.get_available_providers():
                    raise RuntimeError("ONNX Runtime has no CUDA execution provider")
        from paddleocr import PPStructureV3, DocPreprocessor
        config = config_identity(args)
        inventory = []
        # Check all prior manifests before downloading/loading models.
        for pdf in selected:
            prior = args.output_dir / pdf.stem / "manifest.json"
            if prior.exists():
                data = json.loads(prior.read_text(encoding="utf-8"))
                if not args.resume or data["source"] != source_identity(pdf) or data["config"] != config:
                    raise ValueError(f"Existing/source-changed/config-changed output for {pdf.name}; "
                                     "use --resume with unchanged settings or a new output directory")
            doc = pdfium.PdfDocument(str(pdf))
            try:
                inventory.append(dict(source=source_identity(pdf), source_file=pdf.name, page_count=len(doc)))
            finally:
                doc.close()
        if args.worker and (not args.pdf or not args.limit_pages or args.limit_pages > 10):
            raise ValueError("A worker must select one PDF and at most ten pages")
        if not args.worker:
            atomic_json(args.output_dir / "selection.json", dict(selected_at=utc_now(), documents=inventory,
                    total_pages=sum(item["page_count"] for item in inventory)))
        logging.info("Selected %d PDFs; loading PP-StructureV3 on %s", len(selected), args.device)
        started = time.perf_counter()
        engine_options = ONNX_SETTINGS if args.engine == "onnxruntime" and args.device.startswith("gpu") else {}
        preprocessor = (DocPreprocessor(device=args.device, engine=args.engine,
                            engine_config=engine_options or None,
                            use_doc_orientation_classify=False, use_doc_unwarping=True)
                        if MODEL_SETTINGS["use_doc_unwarping"] else None)
        structure_settings = {**MODEL_SETTINGS, "use_doc_unwarping": False}
        structure = PPStructureV3(device=args.device, engine=args.engine,
                                engine_config=engine_options or None, **structure_settings)
        pipeline = ColumnPipeline(structure, preprocessor, args.single_column, args.gutter_points)
        sessions = verify_onnx_sessions([structure, preprocessor], args.device.startswith("gpu")) if args.engine == "onnxruntime" else []
        if sessions:
            atomic_json(args.output_dir / "gpu_verification.json", dict(verified_at=utc_now(), sessions=sessions))
        summary = dict(started_at=utc_now(), input_dir=str(args.input_dir), config=config,
                       model_load_seconds=round(time.perf_counter() - started, 3), sessions=sessions,
                       documents=[dict(source_file=item["source_file"], page_count=item["page_count"],
                                       status="pending") for item in inventory])
        summary_path = args.output_dir / ("worker_summary.json" if args.worker else "run_summary.json")
        atomic_json(summary_path, summary)
        for doc_idx, pdf in enumerate(selected):
            try:
                result = run_pdf(pdf, pipeline, args, config, pdfium, np)
            except Exception as exc:
                logging.exception("Document failed: %s", pdf.name)
                result = dict(source_file=pdf.name, document_error=str(exc))
            summary["documents"][doc_idx] = result
            summary["updated_at"] = utc_now()
            atomic_json(summary_path, summary)
        summary["finished_at"] = utc_now()
        summary["elapsed_seconds"] = round(time.perf_counter() - started, 3)
        summary["failed"] = any(d.get("document_error") or d.get("failed_pages") for d in summary["documents"])
        atomic_json(summary_path, summary)
        logging.info("Finished: %s", json.dumps(summary["documents"]))
        return 1 if summary["failed"] else 0


if __name__ == "__main__":
    try:
        sys.exit(main())
    except KeyboardInterrupt:
        logging.warning("Interrupted; completed pages remain resumable")
        sys.exit(130)
    except Exception:
        logging.exception("OCR startup/run failed")
        sys.exit(1)
