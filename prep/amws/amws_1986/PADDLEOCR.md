# Raw OCR for AMWS edition 16

`run_paddleocr_ed16.py` reads scanned PDFs directly. It selects only PDFs
without a same-stem DOCX (case insensitive) and excludes `16_Index_*`.
It does not segment biographies, correct OCR, parse birthplaces, or modify
source PDFs, existing DOCX files, or downstream AMWS datasets.

For an explicit historical-OCR comparison, `--allow-transcribed --pdf NAME`
permits one same-stem DOCX pair. This opt-in does not change default selection
and never permits index PDFs. See the nine-document comparison workflow below.

## Environment

Use 64-bit Python 3.11 in an isolated environment outside Dropbox. This
machine's environment is `C:\Users\megaj\.venvs\amws-paddleocr`.
PowerShell commands for a fresh installation:

```powershell
py -3.11 -m venv "$env:USERPROFILE\.venvs\amws-paddleocr"
$ocrPython = "$env:USERPROFILE\.venvs\amws-paddleocr\Scripts\python.exe"
& $ocrPython -m pip install --upgrade pip
& $ocrPython -m pip install paddlepaddle-gpu==3.2.2 -i https://www.paddlepaddle.org.cn/packages/stable/cu126/
& $ocrPython -m pip install 'paddleocr[doc-parser]==3.7.0' paddlex==3.7.2 pypdfium2==5.14.0 pandas==2.2.3 shapely==2.0.7 scikit-learn==1.6.1
& $ocrPython -m pip install onnxruntime-gpu==1.23.2
& $ocrPython -m pip check
```

An NVIDIA GPU and compatible CUDA runtime libraries are required for
`--device gpu:0`. The runner verifies the actual ONNX model sessions use
CUDA; GPU detection by `nvidia-smi` alone is insufficient. Models are automatically downloaded at first use;
subsequent runs reuse the local PaddleX model cache.

### Windows Smart App Control compatibility

On this machine, native PaddlePaddle 3.2.2 import is blocked with WinError
4551 for the unsigned `paddle/libs/warprnnt.dll`. This is Smart App Control,
not an antivirus detection; Defender exclusions do not grant SAC approval.
Windows security settings have not been modified by this setup.

PP-StructureV3 also supports an official ONNX Runtime inference engine.
The default engine is ONNX Runtime 1.23.2 (CUDA 12), installed above. To
test native Paddle if its DLLs are accepted by Windows:

```powershell
& $ocrPython -c "import paddle; paddle.utils.run_check(); print('GPU count:', paddle.device.cuda.device_count())"
# Only after that succeeds, choose --engine paddle with a fresh --output-dir.
```

The runner preloads the NVIDIA CUDA/cuDNN runtime packages installed by the
GPU Paddle wheel, and requests official ONNX models for the same pipeline.
Changing engines requires a fresh output directory if page checkpoints
already exist. The older pandas, Shapely and scikit-learn pins above avoid
SAC blocks observed in newer binary wheels on this PC; verify real inference
before assuming the full environment is accepted.

[Microsoft's SAC FAQ](https://support.microsoft.com/en-us/windows/security/threat-malware-protection/smart-app-control-frequently-asked-questions)
states there is no per-app allow exception. A signed/trusted build or a
Microsoft reputation review can resolve individual blocks; the system-wide
Off setting removes SAC enforcement. Recent Windows updates support
re-enabling it, but a previously blocked DLL may still be blocked again.
No automatic security-setting changes are part of this runner.

Official references:
[Windows PaddlePaddle installation](https://www.paddlepaddle.org.cn/documentation/docs/en/install/pip/windows-pip_en.html),
[PaddleOCR installation](https://www.paddleocr.ai/main/en/version3.x/installation.html),
[PP-StructureV3](https://www.paddleocr.ai/main/en/version3.x/pipeline_usage/PP-StructureV3.html).

## Run

Run from the repository root, using the environment's Python explicitly:

```powershell
$ocrPython = "$env:USERPROFILE\.venvs\amws-paddleocr\Scripts\python.exe"
$ocrScript = 'prep/amws/amws_1986/run_paddleocr_ed16.py'
& $ocrPython $ocrScript --list
# Pilot: keep images for comparison against OCR.
& $ocrPython $ocrScript --pdf 16_C_400_600.pdf --limit-pages 5 --save-images --resume
# Full batch: sequential workers of at most ten pages, then verified ZIP delivery.
& $ocrPython prep/amws/amws_1986/resume_paddleocr_ed16.py
```

Defaults use `TALENT_DETS_DATA_DIR` if set; otherwise
`%USERPROFILE%\Globtalent Dropbox\gtl_talent_dets`:

- Input: `Data/amws ed 16`
- Output: `%USERPROFILE%/ocr_runs/amws_ed16` (outside Dropbox)
- Engine: `onnxruntime`; device: `gpu:0`; render resolution: 300 DPI
- Detection: `PP-OCRv5_mobile_det`; English recognition: `en_PP-OCRv5_mobile_rec`
- Table, formula, chart, seal, and orientation classification are disabled.
- UVDoc page straightening is enabled: it recovered merged/garbled lines in
  the pilot, including Hadeen's birth date. `--no-unwarp` disables it.
- After straightening, find the blank gutter and process the two biography
  columns separately with PP-StructureV3. This prevents detection from merging
  lines across columns. Page text follows left column, then right column;
  page headers are moved to the beginning of the plain text export.
- Each column's detection image is capped at 3200 pixels on its longest side;
  recognition uses full-resolution crops. A 0.4 detection box threshold and
  0.7 unclip ratio recover dense lines without capturing neighboring lines.
  The mobile detector reduces memory use compared with the server detector.
  `--det-limit-side-len` overrides the cap.
- Layout and region detection remain enabled within each column.
- All layout labels are retained in raw exports, including page headers.

Use `--input-dir` and `--output-dir` for explicit directories. `--pdf` takes
one eligible filename. `--single-column` handles an exceptional page without
two biography columns; use a separate output directory because this changes
the configuration. An unclear gutter is logged as a failed page for review.
`--start-page` is one based; `--limit-pages` applies
per selected PDF, starting at that page. `--list` is read only and requires
no OCR dependencies. `--save-images` retains PNGs only for pages processed
in that invocation; it does not regenerate images for resumed pages.

## Outputs and recovery

Each source PDF has its own output folder:

- `page_0001.json`: source path/name, one-based PDF page number, DPI, rendered
  size, package/model configuration, timing, sampled GPU memory, and the
  raw results in `prediction.column_predictions` (including OCR text, boxes
  and recognition scores). Each column includes its `x_offset`: add that to
  its raw box/polygon x coordinates to locate it on the straightened page.
  The combined `parsing_res_list.block_bbox` already uses full-page coordinates.
  `box_coordinate_space/width/height` record that page coordinate frame;
  `prediction.gutter_x` records the column split.
- `page_0001.txt` and `.md`: raw text blocks in the pipeline's reading order
  and the Markdown export. These are OCR hypotheses, not verified transcripts.
- `page_0001.png`: optional pilot image.
- `page_0001_unwarped.png`: optional straightened image to inspect OCR boxes.
- `manifest.json`: source identity, settings, page count and page checkpoints.
- `document.txt` and `.md`: concatenated pages with filename/page markers.
  Missing or failed pages have explicit gap markers.

The output root contains `ocr.log` (append only), `run_summary.json` (latest
invocation), `selection.json` (all selected sources and page counts),
`gpu_verification.json` (actual model session providers), and an OS-managed lock to prevent concurrent runs in the same
output directory. Document manifests retain progress across invocations.
Each manifest's `summary` is refreshed after every processed page. The
run summary advances per document and records `finished_at` only when the
invocation finishes. Inspect manifests for progress while a document runs.

`--resume` skips a page only if JSON, TXT and Markdown exist and the JSON
checkpoint has the matching configuration, nonempty columns and Markdown,
and TXT matching the ordered JSON text. Interrupted partial pages and
failed pages are retried. Source identity uses absolute path, byte size and
modification time. Changed source files, packages, models, device, or DPI
require a new output directory. Existing output is refused without `--resume`.

Page errors are logged and processing continues; summary reports include
failed page numbers and pending counts. Exit code 1 indicates page/document
failures; 130 indicates interruption. A page-limited pilot can exit 0 while
pages outside its requested range remain pending. Full completion means zero
failed and zero pending pages in every selected document manifest.

For visual QA, compare retained pilot PNGs to text, checking the transition
from the bottom of the left column to the top of the right column, names,
birthplaces/dates, and partial entries at page boundaries. GPU memory is a
sample of device-wide memory after a page, not peak per-process allocation.

## Checks

```powershell
& $ocrPython prep/amws/amws_1986/support/test_paddleocr_ed16.py
```

Checks cover PDF selection/exclusion, reading-order export, failure logging,
partial-checkpoint recovery, failed-page retries, provenance, confidence
preservation, column coordinate offsets, transient Dropbox locks, and
source-change rejection. Validate real OCR on scans before
using it for biography extraction.

## Verified setup on this PC (2026-10-09)

The final pilot processed PDF pages 1-5 of `16_C_400_600.pdf` without failures;
all five ONNX model sessions had CUDA enabled. It averaged 12.55 seconds per
page including rendering and two saved PNGs, with device-wide memory samples
of 4121-6381 MiB. A second invocation resumed all five pages without processing
them again. Six runner checks and `pip check` passed.

The selected batch contains 14 PDFs and 1792 pages. `pilot_qa.json` records
the sample review. The original long worker stopped with memory allocation
errors; 605 empty pages had incorrectly been marked successful. The local
recovery copied and SHA-256 checked every existing file, preserved the
Dropbox originals, and retained 28 nonempty checkpoints. `recovery_copy.json`
and `recovery_audit.json` record this recovery.

`resume_paddleocr_ed16.py` runs sequential fresh workers handling at most
ten pages. A worker exits at its first error, so a damaged inference session
cannot produce later false successes. Unfinished ranges retry in workers of
two pages, then one page, with one additional clean single-page retry. No
model, render, recognition, or reading-order settings change. Persistent
failures remain explicit and prevent ZIP delivery.

All page files, checkpoints and logs stay under `C:\Users\megaj\ocr_runs\amws_ed16`.
`controller_job.json` identifies the parent process; `active_worker.json`
identifies its current child. `controller.log`, `workers_stdout.log` and
`workers_stderr.log` record execution. Root `run_summary.json` covers all
14 PDFs; `worker_summary.json` covers only the most recent worker. The root
summary updates between workers; manifests update after each page.

Only after all 1792 JSON/TXT/Markdown triplets pass validation and source
identities remain unchanged does the controller create
`C:\Users\megaj\ocr_runs\amws_ed16_complete.zip`. It checks every ZIP member's
CRC and verifies SHA-256 after copying one archive into the Dropbox
`output/amws` folder. `completion_verification.json` and `delivery.json`
record those checks. The local results and original partial Dropbox files
remain available. `delivered` is the final root-summary state.

The completed run on 2026-10-10 accounts for all 1792 pages: 1791 nonempty
OCR pages and one visually verified blank source page (`16_G_800_900.pdf`,
PDF page 106). The user explicitly approved recording the blank page and
finishing. Its JSON status is `verified_blank`, its predictions contain no
invented text, and readable exports contain a labeled blank-page marker.
Resume validates the approval, source identity and hash of the reviewed scan.

Four photographed pages needed individually reviewed curved column
boundaries: `16_C_400_600.pdf` page 81, `16_E_800_900.pdf` pages 79-80,
and `16_G_800_900.pdf` page 99. `page_geometry_overrides.json` records the
review images, their hashes, source identities and gutter points. Overlapping
column crops mask the opposite column white without changing coordinates;
raw scores and boxes remain available. The models and OCR settings are
unchanged. These exceptions passed the final complete-corpus validation;
13 recovery/runner checks passed.

## Manual quality review

### Saved-OCR paragraph repair pilot

The separate `support/repair_ocr_paragraphs.py` utility restores paragraph
regions that PP-Structure discarded inside an enclosing `reference` block.
It uses saved layout boxes and recognized lines; it does not load OCR models
or alter the original corpus, checkpoints, or delivered archive.

```powershell
& $ocrPython prep/amws/amws_1986/support/repair_ocr_paragraphs.py
```

The default pilot covers `16_C_400_600.pdf` PDF pages 1, 67, and 81, plus
`16_G_500_600.pdf` page 1 as an unchanged control. Results and the comparison
viewer are saved under `C:\Users\megaj\ocr_runs\amws_ed16_paragraph_pilot`.
Use `--ocr-dir`, `--output-dir`, and repeated `--page PDF:PAGE` options to
select other inputs explicitly. No corpus-wide repair or Dropbox delivery
is performed.

Each eligible enclosing reference requires at least two vertically separated
paragraph boxes with 90% containment and exactly one owner for every saved
OCR line inside the reference. Missing regions, overlapping regions, uncovered
lines, or nonunique export content leave that block unchanged and produce an
unresolved audit case; the command exits nonzero if any cases remain.

TXT and Markdown replace only the collapsed content. Raw recognized strings
are joined with line breaks, and recovered paragraphs with one empty row.
The JSON files use a distinct `amws_paragraph_repair` artifact format:
`original_record` retains the complete source JSON; `corrected_prediction`
contains corrected page blocks alongside the original column predictions;
`repair_audit` records assignments, source hashes and failures. Recovered
blocks include line indices, scores, full-page boxes and source/page references.
These derived files are not OCR runner checkpoints.

The viewer shows the original scan, both exports, marked paragraph cards and
a coordinate diagram. Its boxes belong to the straightened OCR image and
are deliberately not drawn over the original photographed scan. Disable
"Mark recovered paragraphs" to see the exact corrected TXT/Markdown source.

Run the conservation and rejection checks with:

```powershell
& $ocrPython -m unittest discover -s prep/amws/amws_1986/support -p test_repair_ocr_paragraphs.py -v
```

The three affected columns contain eight, nine, and nine recovered regions,
respectively, retaining all 268 saved OCR lines. The 23 internal paragraph
boundaries match the scans. The first region on page 81 is a continuation,
not a new biography. Recognition errors remain: page 67's final printed
ZIP code `20550`, for example, is already absent from the saved OCR and
cannot be recovered by this paragraph-only correction.

### Full-corpus saved-OCR paragraph repair

Process the complete inventory into a separate local tree, automatically
resuming valid pages if interrupted:

```powershell
& $ocrPython prep/amws/amws_1986/support/repair_ocr_paragraphs.py --all-pages --verify-expected-ed16
```

The full-corpus default output is
`C:\Users\megaj\ocr_runs\amws_ed16_paragraph_repaired`. It produces derived
JSON/TXT/Markdown for all 1792 pages, document-level exports with page markers,
individual scan-versus-export HTML viewers, a searchable `index.html`, and
`review_queue.json` / `review_queue.csv`. The approved blank marker is retained.
`--all-pages` and explicit `--page` selections are mutually exclusive; the
three-page pilot remains the default without either option.

Resume validates the repair/corpus version, input-triplet SHA-256 values, and
hashes of every derived JSON/TXT/Markdown, scan image and page viewer. Invalid
or incomplete pages are rebuilt. Final verification independently recomputes
the repair from saved inputs, verifies line conservation, unchanged geometry
and raw predictions, checks document exports and accounts for every page.
Scans are rendered at 150 DPI using a reused PDF handle; no OCR model is loaded.

Expected outcomes are 1007 fully repaired pages, 78 partially repaired,
54 unresolved only, 652 unchanged, and one verified blank. The 1463 repaired
reference blocks yield 13496 paragraph regions; 138 blocks on 132 pages stay
unchanged and explicitly flagged. Gray diagram regions already existed,
green regions were recovered, and amber blocks require review. Each page's
content loads independently of the index.

The first command ends at `verified_ready_for_review`. Its deterministic
`visual_review_sample.json` selects the pilot/control, two fully repaired
pages per PDF (seed 1986), and an example of each rejection reason. Record
actual visual comparisons in `visual_review.json`, with one row per selected
page containing its source filename, PDF page, derived JSON SHA-256 and
`review_status` (`passed` or `flagged_as_expected`), plus review notes.
This records paragraph review, not certification of character accuracy.

After that review, verify and package locally:

```powershell
& $ocrPython prep/amws/amws_1986/support/repair_ocr_paragraphs.py --all-pages --package-only --verify-expected-ed16
```

Packaging refuses incomplete or stale visual review. It builds
`C:\Users\megaj\ocr_runs\amws_ed16_paragraph_repaired.zip`, checks ZIP member
names and CRCs, verifies each member's SHA-256 against the output manifest,
and records the archive hash in `packaging.json`. `--all-pages --verify-only`
performs structural checks without processing or packaging. No Dropbox copy
is performed, and the original corpus, pilot and previous archive stay intact.

Run both paragraph and corpus regression checks with:

```powershell
& $ocrPython -m unittest discover -s prep/amws/amws_1986/support -p "test_repair_ocr*.py" -v
```

### Reviewing the original corpus

Open `C:\Users\megaj\ocr_runs\amws_ed16_quality_review\index.html` in a browser.
The local reviewer compares original scans at 150 DPI with ordered OCR and
recognized-line confidence. It includes 28 random pages (two per PDF, seed
1986), four column-adjusted pages, six lower-confidence pages, a prior pilot
page and the verified blank page: 40 pages in total.

Follow the full page for column order and omitted lines, then audit five
complete biographies spread across the top, middle and bottom. Count checked
and incorrect names, birth dates and birthplaces; record omissions and source
damage separately. The reviewer reports field-error rates separately for the
random sample and targeted-only pages. Model confidence scores help prioritize
review but cannot detect text that OCR omitted. These sample checks do not
certify the full corpus.

Notes save in browser local storage when available. Export review JSON for a
portable backup; import it to resume. CSV export supports later audit analysis.
The review changes no OCR files or source PDFs. Rebuild the sample with:

```powershell
& $ocrPython prep/amws/amws_1986/support/build_ocr_quality_review.py
```

The checked columns follow the scans. Dense lines formerly omitted in
Haddy's, Hadermann's, and Haeder's entries are now readable. Outputs still
contain punctuation/capitalization substitutions and character errors:
Bruce Alan Hadley's printed date `Aug 1, 50` appears as `Aug I, 5O`, and
the scan itself clips the initial H of Haeberle/Haeberli on printed page 403.
The review is a sample check, not certification of every line or birth date;
keep the raw output for later validation rather than treating it as a clean
parsed scientist dataset.

To check the current background run in PowerShell:

```powershell
$ocrOutput = "$env:USERPROFILE\ocr_runs\amws_ed16"
$ocrJob = Get-Content "$ocrOutput\controller_job.json" -Raw | ConvertFrom-Json
Get-CimInstance Win32_Process -Filter "ProcessId=$($ocrJob.pid)" | Select-Object ProcessId, CommandLine
Get-Content "$ocrOutput\ocr.log" -Tail 10
Get-Content "$ocrOutput\run_summary.json" -Raw
```

To intentionally stop, first verify the recorded PID still runs this
controller, then terminate its process tree (including the active worker).
Restart the controller command above to resume. Do not start a second job
while an existing controller or worker is running.


### Visually reviewed exception repairs

`support/repair_ocr_exceptions.py` leaves the first paragraph algorithm unchanged.
It processes the 138 first-pass exceptions using saved detections and explicit,
scan-bound decisions. It imports no OCR models. Run from the repository root:

```powershell
$ocrPython = 'C:\Users\megaj\.venvs\amws-paddleocr\Scripts\python.exe'
& $ocrPython prep/amws/amws_1986/support/repair_ocr_exceptions.py --phase pilot
& $ocrPython prep/amws/amws_1986/support/repair_ocr_exceptions.py --phase all
& $ocrPython prep/amws/amws_1986/support/repair_ocr_exceptions.py --phase verify
& $ocrPython prep/amws/amws_1986/support/repair_ocr_exceptions.py --phase package
```

Optional `--baseline-dir` and `--output-dir` override the local defaults.
The pilot is saved under `ocr_runs/amws_ed16_exception_pilot`; the full corpus
and its ZIP are `ocr_runs/amws_ed16_paragraph_reviewed` and the corresponding
`.zip`, outside Dropbox. Full processing requires completed pilot verification.

`proposals.json` contains original paragraph candidates and all target raw
records. `decisions.json` records stable block identities, context and scan
hashes, reviewer, timestamp, inspection notes, accepted source-index groups,
region labels and geometry. Automatic proposals are never accepted without a
visual decision. Exact-empty strings go into a separate audit; nonempty
fragments are assigned once or retained explicitly in an unresolved block.
Recognition observations are written separately to
`recognition_review_queue.json`/`.csv`; `review_queue.json`/`.csv` contains
remaining assignment problems. Recognition text is never corrected here.

Each page has its own comparison viewer. Purple dashed boxes show original
candidates, blue shows accepted exception assignments, green shows first-pass
paragraphs and amber shows unresolved references. The index searches PDF/page
and rejection reason, with document and outcome filters. It loads page content
only when a viewer opens. Document exports preserve one-based PDF page markers.

Resume checkpoints bind the repair version, baseline assets, source context,
accepted decision hash and all five completed page outputs. A changed decision
or missing/corrupt output recomputes that page. Unchanged assets use hard links
when available; changed files are written through atomic replacement so earlier
results remain intact. Do not edit linked assets in place.

Verification checks original OCR hashes, target detection conservation,
unaffected exports, prior repaired blocks, gutters and the approved blank page.
Packaging requires complete visual decisions, checks free space with at least
1 GiB headroom, then checks ZIP membership, CRCs and streamed member hashes.
`packaging.json` records the streamed archive SHA-256. The repair utility itself
does not copy files to Dropbox. A separate reduced delivery is described below.

### Reviewed OCR data in Dropbox

The uncompressed sharing copy is in the Dropbox project at:

```text
output/amws/amws_ed16_paragraph_reviewed_data/
```

It preserves the 14 PDF subfolders and contains all 1,792 corrected page JSON
files, 14 combined TXT documents and 14 combined Markdown documents. It also
includes the corpus inventory, decisions, proposals, audits, verification,
visual-inspection records, both review queues and implementation provenance.
The selected source files total about 984 MB. A delivery README, manifest and
report accompany the copy.

Use `corrected_prediction.parsing_res_list` in page JSON for the reviewed
paragraph structure. `original_record` preserves the original OCR; the copied
JSON retains source indices, recognized strings, boxes and confidence scores.
The original `repair_audit` describes the first pass; use `exception_audit`,
`exception_audit.json` and `audit_report.json` for the reviewed exceptions and
consolidated final outcomes. Combined TXT/Markdown retains one-based PDF page
markers, including the approved blank marker for `16_G_800_900.pdf`, page 106.

The two unresolved right-column blocks are `16_E_700_800.pdf`, page 28, and
`16_E_800_900.pdf`, page 94. `review_queue.csv`/`.json` lists these assignment
problems; `recognition_review_queue.csv`/`.json` lists 33 observed recognition
issues. This delivery changes paragraph separation, not recognized text or the
existing parsed person datasets.

Scans, browser viewers, screenshots, resume checkpoints, individual page
TXT/Markdown and ZIP archives are excluded from this sharing copy. The complete
local corpus and ZIP remain intact under `C:\Users\megaj\ocr_runs`. Open
`amws_ed16_paragraph_reviewed/index.html` there for visual comparisons. Viewer
paths in copied audit records refer to the full local corpus, not this reduced
Dropbox folder. The source PDFs remain in the Dropbox project at `Data/amws ed 16`.

Independent copies are staged outside Dropbox, hash-checked and moved into
place atomically, in batches of at most 25 files with two-second pauses.
`delivery_manifest.json` lists the delivered data and README with byte sizes
and SHA-256 hashes; it excludes itself and the operational delivery report.
`delivery_report.json` records local-copy verification, archive preservation
and cloud synchronization separately. A running Dropbox client or matching
local hashes alone does not confirm that cloud synchronization has finished.
The sealed local README and archive manifests remain unchanged.

## Historical Word versus raw PaddleOCR comparison (2026-10-19)

The comparison freezes nine edition 16 PDF/Word pairs (1,360 pages), drawn
from 37 eligible same-stem pairs. With `random.Random(20261019)`, sample six
from alphabetically sorted PDF filenames, then three from the sorted remaining
pool using the same generator. The frozen manifest includes the entire pool,
selection group, original source identities, PDF/DOCX SHA-256 values and counts.

| PDF | Pages | Selection |
|---|---:|---|
| `16_A_0_200.pdf` | 212 | Added |
| `16_B_400_600.pdf` | 198 | Original |
| `16_D_0_200.pdf` | 202 | Added |
| `16_D_200_400.pdf` | 200 | Original |
| `16_E_1100_1200.pdf` | 48 | Original |
| `16_E_200_400.pdf` | 200 | Original |
| `16_E_900_1000.pdf` | 100 | Original |
| `16_F_500_600.pdf` | 100 | Added |
| `16_F_800_900.pdf` | 100 | Original |

`support/compare_ocr_methods.py prepare --root <local-comparison-root>`
freezes the draw, extracts historical Word text without corrections, and renders
audit scans. It refuses to redraw when an existing manifest is present. The
controller accepts `--selection-manifest <root>/comparison_manifest.json
--output-dir <root>/ocr --no-deliver`; source hashes and dynamic totals are
verified before inference. The original default 14-PDF/1,792-page controller
selection and delivery remain unchanged. Comparison delivery is separate and
requires the audit to be finished. All runs use the documented OCR settings.

This chat stages the comparison under
`C:\Users\megaj\Documents\Codex\2026-10-19\based-on-the-c\work\amws_ocr_comparison_20261019`.
Audit pages run first in fresh single-page workers; remaining pages use the
existing ten/two/one-page retry procedure. Geometry exceptions retain source
identity, reviewed image hash, reviewer, notes and gutter points. Failure images
preserve the actual straightened coordinate frame. Blank source pages require
source-bound visual evidence and an explicit review record; task-authorized
agent review is labeled separately from human user approval.
Centered front matter can use `--layout-review <review.json>` for one explicit
PDF/page. The source/image-bound review must identify a non-biography page;
the checkpoint records its actual single-column configuration while the document
keeps its default two-column configuration. This exception changes page geometry,
not models, resolution or straightening. Unreviewed mixed configurations remain
invalid. A title-page review and blank reverse-side review were required for
`16_D_0_200.pdf`, pages 2 and 3 respectively.

The audit seed is `20261020`. For each sorted selected PDF, generate a complete
random permutation of page numbers and select the first two scan-confirmed
biography pages, recording any rejected front matter. On each of 18 pages,
list complete biographies in scan reading order, excluding cross-reference-only
entries and biographies extending to another page. Select five using a local
random seed derived from the first eight bytes (big endian) of SHA-256 of
`20261020:<PDF filename>:<page number>`. Freeze the scan inventory and first
100 whitespace words per selected biography (or the entire shorter biography)
before reading either OCR. Later source-confirmed transcription corrections are
retained in `reference_revisions.json`, together with original drafts and evidence.
All 90 selections are retained. Source-blurred cases
are explicitly unscored rather than replaced.

`align` proposes name/context matches and word-ending excerpt spans; manual
alignment decisions are tied to reference and normalized source-text hashes.
`summarize` produces paired micro-average CER/WER, per-file results and verbatim
field-string checks, with a separate alphanumeric field-preservation diagnostic. Primary
normalization is Unicode NFC and collapsed whitespace only; case and punctuation
remain errors. A secondary alphanumeric CER ignores case, punctuation and spaces
and is labeled as supporting evidence. Omitted text is not filled from the other
method. Large reading-order errors remain in the raw-text comparison; no saved-OCR
paragraph repairs or spelling corrections are applied.

Word extraction visits each original XML text run once, including nested text-box
paragraphs. Recursively reading every descendant of every paragraph would repeat
nested content. The comparison checks non-whitespace text-run conservation and
retains the initial derived extracts and correction ledger. Floating-frame XML
anchor order can still differ from visual column order, so raw-transcript error
rates do not isolate recognition from layout/export damage. The existing Word
OCR engine is unestablished. The production R extraction and datasets are not
changed by this comparison.

In the reviewed 18-page audit, 87 of 90 reference selections are readable
(8,049 words); three on `16_D_0_200.pdf:8` remain explicitly unscored. Raw Paddle
CER/WER are 5.77%/25.79%; Word CER/WER are 7.51%/19.77%. Word has lower CER in
65 paired excerpts, Paddle in 19, with three ties. Twelve sampled Paddle pages
have multi-biography blocks, and three show clear local line-order problems.
These are mixed findings from a small audit, not corpus-wide accuracy estimates.
Completion and delivery remain subject to the gates below.

`finalize --root <root> --outputs <deliverables> --dropbox-dir <new-folder>`
requires all 1,360 checkpoints, unchanged PDF/Word hashes, 18 recorded page reviews,
90 accounted-for reference selections and reviewed alignments. It builds a portable
HTML review folder and a ZIP with source-code provenance and a payload hash
manifest. ZIP CRC and every payload-member SHA-256 are verified before the local
Dropbox copy is hash-checked. Local Dropbox copy verification is distinct from
cloud synchronization. Sources and production AMWS datasets are never rewritten.

Checks: `support/test_paddleocr_ed16.py` and `support/test_compare_ocr_methods.py`.

## Automated field extraction audit (2026-10-19)

This companion audit reuses the completed nine-PDF OCR, with no new OCR run.
Seed `20261021` generates complete page permutations for alphabetically sorted
PDFs. Exclude the previous 18 audit pages, then accept the first two pages with
biography starts. All 18 proposed pages were eligible; image quality did not
trigger replacement. All **304 biography starts** were transcribed from scans
before extraction predictions were generated or reviewed. Incoming continuations
and cross-reference/deceased stubs are inventoried separately. Outgoing
continuations were read on the next page. References were agent-transcribed and
reviewed by the same agent, without independent human verification.

The `manifest.json` freezes PDF/DOCX hashes, full permutations and a snapshot of
the current AMWS extraction code. `references.json`, `aliases.json` and their
freeze ledger precede prediction review. References distinguish readable,
absent and source-unreadable fields. An ambiguous reference century is unscored.
All education providers explicitly printed in the Educ section are in scope,
including attendance and additional qualifications. Printed campuses/units stay
distinct. Employers, memberships, non-educational certification boards and
separate Hon Degrees sections are excluded. Repeated degrees at one provider
count once; fixed documented historical aliases are deduplicated.

`support/extract_ocr_audit.R` applies frozen reusable R definitions identically to
both sources: segmentation, name/birthplace/date parsing, year/city parsing and
generic regex corrections. Input is the complete individual document, using the
single-visit Word XML extraction from the prior comparison or raw Paddle
`prediction.parsing_res_list` in original block order. Existing body layout labels
are retained; previous manual paragraph repairs and person overrides are omitted.
Documents are independent; separate PDF ranges are not joined. Institution
predictions use `education_regex.py` multi_v03 plus
`postsecondary_qualifications.py` postsecondary_v1, all qualification groups,
and the predicted birth year. No references, sampled boundaries, missing values
or identity decisions enter extraction; fragmented predictions are not stitched.
The existing degree parser's inability to recover some bare attendance remains
an extraction failure, rather than prompting sample-specific rule changes.

Prediction records are frozen before one-to-one identity/context alignment.
Fuzzy similarity proposes identities only, never field correctness. Scores ignore
case, punctuation and whitespace, and accept only the pre-review alias table.
All printed birthplace components and the correct campus/unit are required;
spelling errors are not forgiven. Birth years must be exactly four digits.
The separate outcomes are correct, incorrect, missing, wrong-person attribution,
source-unreadable and absent/unscored. Missed biographies fail every readable,
present reference field.

Report names, full birthplaces, years and city-only accuracy; institution recall,
precision and exact-set accuracy; complete four-category records; detection and
duplicates. Institution precision covers predictions attributable to sampled
biographies, including duplicate predictions, not the entire unsampled corpus.
Pooled, per-PDF and per-page numerators/denominators and Paddle-minus-Word
percentage-point differences are retained. Paired page bootstrap intervals use
10,000 draws and seed `20261022`, sampling two pages with replacement within each
PDF and keeping both methods paired. Two pages per stratum severely limit
inference. Single-page intervals cannot be estimated. These are **automated
extraction accuracy on a small paired sample**, incorporating OCR, segmentation
and parser failures. Diagnostic error causes are recorded conservatively and
are not an independent causal decomposition.

Run with the AMWS Python environment, from the repository root:

```powershell
python prep/amws/amws_1986/support/compare_ocr_fields.py prepare --root <new-root>
# Perform and review scan-only annotation; do not inspect extraction predictions.
python prep/amws/amws_1986/support/compare_ocr_fields.py freeze-references --root <root>
python prep/amws/amws_1986/support/compare_ocr_fields.py predict --root <root>
python prep/amws/amws_1986/support/score_ocr_fields.py propose --root <root>
# Review every identity/context decision in alignment_review.json.
python prep/amws/amws_1986/support/score_ocr_fields.py align --root <root>
python prep/amws/amws_1986/support/score_ocr_fields.py score --root <root>
python prep/amws/amws_1986/support/deliver_ocr_fields.py build --root <root>
# Complete tests and browser review; write their evidence ledgers.
python prep/amws/amws_1986/support/deliver_ocr_fields.py finalize --root <root>
```

The extraction runner caches full-document parsed and prediction checkpoints;
unchanged input hashes and its extraction contract are required for resume.
The R adapter needs the documented local R packages. Raw input checkpoints,
Word XML-derived paragraphs, full predictions, frozen parsers and logs are
included in the delivery for reproducibility. Original PDFs/DOCX remain in their
source directory, identified by SHA-256. Scans, source-only annotations, CSV/JSON
decisions, an offline HTML viewer and `report.txt` are included.

Delivery requires 304 reviewed references, 608 reviewed one-to-one alignments,
reproducible metrics, unchanged source/frozen parser hashes, passing tests,
reviewed viewer and preservation of the earlier comparison ZIP. Every archive
member's CRC and SHA-256 and the local Dropbox copy SHA-256 are checked. The new
archive is copied to `output/amws/field_extraction_audit_20261019/`; this verifies
the local Dropbox copy, not cloud synchronization. Earlier outputs and production
datasets remain separate. Tests are in `support/test_compare_ocr_fields.py`.

Observed field-audit results (correct/denominator, percent):

| Outcome | Existing Word | Raw Paddle |
|---|---:|---:|
| Full name | 264/304 (86.8%) | 221/304 (72.7%) |
| Full birthplace | 246/294 (83.7%) | 232/294 (78.9%) |
| Birth year | 272/290 (93.8%) | 255/290 (87.9%) |
| City only | 264/290 (91.0%) | 245/290 (84.5%) |
| Institution recall | 442/568 (77.8%) | 428/568 (75.4%) |
| Institution precision | 442/506 (87.4%) | 428/494 (86.6%) |
| Entire institution set | 210/304 (69.1%) | 205/304 (67.4%) |
| Complete record | 175/288 (60.8%) | 156/288 (54.2%) |
| Biography detection | 297/304 (97.7%) | 281/304 (92.4%) |

The complete-record difference is −6.6 percentage points, with paired page
bootstrap 95% interval [−9.9, −3.2]. Results are mixed across files: Paddle improves
complete records in five of nine PDFs, but suffers large losses in the two
F-range PDFs, whose raw output contains clipped/corrupt surnames and interleaved
fragments. The audit does not identify which preprocessing/model stage caused
these errors. No primary-identity duplicates were found. Nine unmatched output
fragments add 12 Word and two Paddle institution false positives to precision;
they receive no recall credit or manual stitching. Three identity decisions
retain uncertainty flags. Ten birthplaces and fourteen years are absent; none
of the sampled fields was classified source-unreadable.

One post-prediction reference correction is explicit: the scan prints **SMITH,
SHARRON WILLIAMS**, whereas the initial source-only annotation omitted the second
R. The original freeze is retained, the correction is logged separately, and both
sets of metrics/ledgers are delivered. Original frozen-reference Word name and
complete-record scores were 263/304 and 174/288; Paddle scores are unchanged.
Predictions, aliases and extraction rules were not corrected. This is a disclosed
reference-adjudication deviation, not independent human verification.


### Manual whole-entry comparison: 20 fresh pages

The separate `manual_entry_audit_20261020` delivery reviews 339 biographies starting
on 20 fresh pages from the same nine PDFs. Sampling uses seed `20261023`, excludes
all 36 earlier audit pages, takes two biography-containing pages per PDF, then two
additional pages with the same generator. Source-only heading inventories,
permutations, exclusions, continuations and later inventory errata are retained.

This is a manual comparison on a small stratified sample, transcribed and reviewed
by a Codex agent without independent human verification. It reuses raw PaddleOCR;
no OCR, field extractor, paragraph repair or person-specific correction was run.
Every full biography, including career/research text and outgoing continuations,
was compared against its scan. Displaced text is assessed as structure, not as a
recognition omission. Word OOXML-generated list labels are retained separately:
the earlier XML snapshot omitted some automatic date labels. Earlier deliveries
remain unchanged.

Results are mixed. Correct full names: Word 295/339 (87.0%), Paddle 314/339 (92.6%).
Correct complete education institution sets: 256/339 (75.5%) versus 273/339 (80.5%).
Correct full birthplaces: 280/326 (85.9%) versus 270/326 (82.8%). Correct printed
birth-year digits: 299/320 (93.4%) versus 288/320 (90.0%). These are direct manual
transcription checks, not field-extraction performance. Four unreadable birth years
are excluded from both denominators; ten partly unreadable biographies retain
uncertain completeness. Thirteen birthplaces and fifteen birth years are absent.

At least one content error was identified in 244/339 Word entries and 243/339
Paddle entries. Structural defects affected 139/339 versus 167/339. Overall paired
judgments, which also consider structure, were Word better 137, Paddle better 77,
tie 8 and uncertain 117. Errors changing factual strings include recoverable
letter/digit confusions; the report distinguishes these from minor spelling errors.
No whole biography was found wholly absent. Do not infer corpus-wide superiority
or the effect of paragraph repair from this sample.

The portable HTML viewer, full source transcriptions/original DOCX copies, selected
raw Paddle JSON/TXT/Markdown, scan images, CSV/JSON ledgers, per-page/per-PDF metrics,
recheck log and findings report are packaged together. The verified ZIP is copied
to Dropbox `output/amws/manual_entry_audit_20261020/`. Production data and prior
archives are preserved. Reproduction/validation helpers are
`support/manual_entry_audit.py`, `manual_word_evidence.py`,
`build_manual_entry_audit.py` and `test_manual_entry_audit.py`; frozen judgment
sources are included in the archive's provenance folder.

### Full comparison after paragraph repair (2026-10-21)

The `paragraph_repaired_comparison_20261021` run applies the same saved-layout
first pass and scan-reviewed exception assignments to all 1,360 pages in the
nine historical comparison PDFs. It imports no OCR model and changes no
recognized strings. The first pass repairs 1,195 reference blocks; all 122
exceptions on 119 pages have source-bound review decisions. The final 1,317
reference blocks yield 12,168 text paragraph regions, with 122,441 recognized
detections retained in paragraphs exactly once, including their original confidence
and boxes. Another 48 empty exception detections are retained in the audit records.
Two verified blank pages and 455 pages without reference-block repairs remain
explicit. The `fully_repaired` outcome means reference-block assignments are
complete; it does not certify every biography boundary or character.

Seed `20261024` selects two repaired pages per PDF. Those 18 pages and the three
previously documented reading-order failures were reviewed before extraction.
Ordinary text blocks on D0 page 15, E900 page 34 and F800 page 100 retain local
interleaving; left-edge clipping and out-of-order date/heading detections remain
in other passages. No recognized words or parser predictions are manually fixed.

Three complete-document arms use the same historical frozen parser and aliases:
Word, raw Paddle and repaired Paddle. Word evidence retains each XML text run
once and includes generated OOXML list labels; decimal, Roman and letter display
formats are supported. The historical reader's omissions and archives remain
documented and unchanged. Record counts are 23,787 / 21,908 / 23,077. Raw Paddle
predictions reproduce the historical outputs exactly across all nine documents.

The existing 304 adjudicated reference biographies on 18 pages give:

| Automated extraction outcome | Word | Raw Paddle | Repaired Paddle |
|---|---:|---:|---:|
| Full name | 264/304 | 221/304 | 232/304 |
| Full birthplace | 246/294 | 232/294 | 235/294 |
| Birth year | 272/290 | 255/290 | 262/290 |
| Institution recall | 442/568 | 428/568 | 448/568 |
| Institution precision | 442/506 | 428/494 | 448/513 |
| Entire institution set | 210/304 | 205/304 | 218/304 |
| Complete four-field record | 175/288 | 156/288 | 159/288 |
| Biography detection | 297/304 | 281/304 | 297/304 |

Repair improves detection and institution recovery, but the complete-record gain
is modest and Word remains ahead on names and complete records. Confidence
intervals use the original stratified paired-page bootstrap: 10,000 draws, seed
`20261022`. There are only two pages per PDF. References and judgments are
agent-reviewed, without independent human verification; this is not an independent
holdout or a ground-truth audit of every full-document record.

Conservative full-document linkage retains 19,785 raw/repaired pairs, 13,380
Word/raw pairs and 14,065 Word/repaired pairs. Only unique exact normalized
name/year or name/place identities with no conflicting known year are linked.
Unmatched/ambiguous records and 22 duplicate-candidate records are retained.
Agreement is not accuracy; no fuzzy automatic matches or prediction stitching
are used. Institution precision includes reviewed orphan fragments as extras.
`full_document_agreement.csv` reports pooled and per-PDF agreement with explicit
denominators and both-missing counts. Name agreement is required by linkage;
birth year/place help identify pairs, so their conditional agreement is selected
upward and must not be presented as ground-truth field accuracy.

Run the orchestrator from the repository root with the AMWS Python environment:

```powershell
python prep/amws/amws_1986/support/repaired_ocr_comparison.py repair
python prep/amws/amws_1986/support/repaired_ocr_comparison.py contexts
# Record scan-bound exception decisions and the seeded visual review first.
python prep/amws/amws_1986/support/repaired_ocr_comparison.py finalize-repair
python prep/amws/amws_1986/support/repaired_comparison_extract.py
# --paddle-input-dir explicitly selects corrected_prediction.parsing_res_list;
# --word-input-dir selects the faithful evidence JSONs. Existing runners/defaults
# are unchanged. Extraction never reads reference values or alignment decisions.
python prep/amws/amws_1986/support/repaired_comparison_score.py propose
# Review one-to-one identities and unmatched fragments before scoring.
python prep/amws/amws_1986/support/repaired_comparison_score.py score
python prep/amws/amws_1986/support/repaired_comparison_score.py full
```

All new commands accept a separate `--root`. The frozen run, reviewed decision
authoring sources, tests and adapters are retained in the archive. Resume binds
source inputs, parser/adapter versions and repair decisions; changes invalidate
reuse. An aggregate-receipt-only missing import was corrected after all 27
document stages finished, without changing predictions; `metadata_finalization.json`
retains both adapter hashes and the original snapshot.

The delivery includes every raw and derived page triplet, complete predictions,
full-document counts and agreement ledgers, sampled accuracy metrics, scan and
three-arm viewers, reviewed decisions and source hashes. It is staged in this
chat's `outputs/paragraph_repaired_comparison_20261021/`; its verified archive is
copied to Dropbox `output/amws/paragraph_repaired_comparison_20261021/`. Matching
local Dropbox hashes does not confirm cloud synchronization. Completion requires
the delivery receipt, not merely this documentation. Original documents, prior
archives and production datasets remain preserved.
