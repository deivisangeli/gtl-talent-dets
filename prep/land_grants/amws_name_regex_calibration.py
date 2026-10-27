"""Frozen 300/200 AMWS name-regex calibration; operates on sample copies only."""
import argparse
import collections
from datetime import datetime, timezone
import hashlib
import json
from pathlib import Path
import random
import re
import unicodedata

import amws_name_cleaning as base
import amws_name_regex_rules as engine

RELEASE = "amws_name_regex_calibration_20261023"
LABEL_FIELDS = ["sample_id", "source_key", "partition", "decision", "expected_name",
                "evidence", "explanation", "error_pattern", "full_bio_reviewed"]


def destination(root=None):
    return (Path(root) if root else base.data_root()) / "output/land_grants" / RELEASE


def read_json(path):
    return json.loads(Path(path).read_text(encoding="utf-8"))


def body_key(row):
    """Ignore name-heading OCR differences when detecting identical biography copies.

    Start at a standalone lowercase birth marker when present, otherwise keep the
    full biography. Normalize Unicode and whitespace only; do not fuzzy-link people.
    """
    text = row["raw_text_adjusted"]
    marker = re.search(r"(?<![A-Za-z])b\s+", text)
    if marker:
        text = text[marker.start():]
    text = " ".join(unicodedata.normalize("NFKC", text).split())
    return hashlib.sha256(text.encode("utf-8")).hexdigest()


def make_sample(lookup, old):
    old_ids = {r["source_key"] for r in old}
    pool = sorted((r for r in lookup if r["source_key"] not in old_ids), key=lambda r: r["source_key"])
    drawn = random.Random(43).sample(pool, 500)
    groups = collections.defaultdict(list)
    for i, row in enumerate(drawn, 1):
        groups[body_key(row)].append(i)
    old_bodies = {body_key(r) for r in old}
    candidates = sorted(k for k in groups if k not in old_bodies)
    random.Random(44).shuffle(candidates)
    test_ids = set()
    for key in candidates:
        if len(test_ids) + len(groups[key]) <= 200:
            test_ids.update(groups[key])
        if len(test_ids) == 200:
            break
    if len(test_ids) != 200:
        raise ValueError("Cannot produce exactly 200 test records without biography leakage")
    return [dict(sample_id=str(i), partition="test" if i in test_ids else "development",
                 biography_group=body_key(r), **r) for i, r in enumerate(drawn, 1)]


def prepare(root=None):
    dest = destination(root)
    if dest.exists():
        raise ValueError("Release already exists; inspect it instead of resampling")
    source, source_manifest, prior = base.locations(root)
    completion = read_json(prior / "completion_manifest.json")
    for name, checksum in completion["output_checksums"].items():
        if base.digest(prior / name) != checksum:
            raise ValueError(f"Prior release changed: {name}")
    lookup, checksum, _ = base.load_source(root)
    old = base.read_csv(prior / "sample_500.csv")
    sample = make_sample(lookup, old)
    dest.mkdir(parents=True)
    fields = ["sample_id", "partition", "biography_group"] + base.LOOKUP_FIELDS
    base.write_csv(dest / "sample_500.csv", sample, fields)
    for partition in ("development", "test"):
        base.write_csv(dest / f"{partition}_sample.csv", [r for r in sample if r["partition"] == partition], fields)
    protected = [source, source_manifest, prior / "completion_manifest.json"]
    protected += [prior / name for name in completion["output_checksums"]]
    base.write_json(dest / "sample_manifest.json", dict(
        source_sha256=checksum, input_checksums={str(p): base.digest(p) for p in protected},
        sample_seed=43, split_seed=44, pool_rows=len(lookup)-len(old),
        sample_algorithm="Random(43).sample(source_key-sorted pool excluding all original 500 IDs, 500)",
        split_algorithm="Group on NFKC/whitespace-normalized biography from birth marker; exclude original groups from test; shuffle sorted group hashes with Random(44); greedily fill exactly 200 test records; other 300 develop",
        sample_ids=[r["source_key"] for r in sample],
        partition_by_key={r["source_key"]: r["partition"] for r in sample},
        duplicate_groups_within_new_sample=sum(n>1 for n in collections.Counter(r["biography_group"] for r in sample).values()),
        new_records_sharing_original_biography=sum(r["biography_group"] in {body_key(o) for o in old} for r in sample),
        frozen_outputs={p.name: base.digest(p) for p in dest.glob("*_sample.csv")} | {"sample_500.csv": base.digest(dest / "sample_500.csv")},
        prepared_at_utc=datetime.now(timezone.utc).isoformat()))
    (dest / "manual_labels.jsonl").touch()
    return dest


def labels(root=None):
    path = destination(root) / "manual_labels.jsonl"
    return [json.loads(s) for s in path.read_text(encoding="utf-8").splitlines() if s]


def append_labels(decisions, root=None):
    dest = destination(root)
    if (dest / "completion_manifest.json").exists():
        raise ValueError("Completed release is immutable")
    sample = {r["sample_id"]: r for r in base.read_csv(dest / "sample_500.csv")}
    seen = {r["sample_id"] for r in labels(root)}
    new = []
    for item in decisions:
        item = dict(item)
        key = str(item.pop("sample_id"))
        if key not in sample or key in seen:
            raise ValueError("Unknown or repeated review ID")
        row = sample[key]
        frozen = (dest / "frozen_rules.json").exists()
        if row["partition"] == "test" and not frozen:
            raise ValueError("Test biographies cannot be reviewed before rule freeze")
        if row["partition"] == "development" and frozen:
            raise ValueError("Development labels are frozen")
        review = {k: "" for k in LABEL_FIELDS}
        review.update({k: row[k] for k in ("sample_id", "source_key", "partition")})
        if set(item) - set(LABEL_FIELDS):
            raise ValueError("Unknown label fields")
        review.update(item)
        review["full_bio_reviewed"] = "true"
        if review["decision"] == "no_change":
            review["expected_name"] = row["name_cleaned"]
            review["evidence"] = review["evidence"] or row["raw_text_adjusted"][:180]
            review["explanation"] = review["explanation"] or "Full biography read; retain the transcribed name after period replacement."
            review["error_pattern"] = review["error_pattern"] or "none"
        if review["decision"] not in {"no_change", "proposed_fix", "unresolved"}:
            raise ValueError("Invalid label decision")
        if review["decision"] == "proposed_fix" and (not review["expected_name"] or review["expected_name"] == row["name_cleaned"]):
            raise ValueError("Correction needs a changed expected name")
        if review["decision"] == "unresolved" and review["expected_name"]:
            raise ValueError("Unresolved names have no asserted full-name target")
        if not review["evidence"] or review["evidence"] not in row["raw_text_adjusted"] or not review["explanation"]:
            raise ValueError("Label requires literal biography evidence and explanation")
        if any(review[k] != row[k] for k in ("source_key", "partition")):
            raise ValueError("Identity or partition changed")
        new.append(review)
        seen.add(key)
    with (dest / "manual_labels.jsonl").open("a", encoding="utf-8", newline="\n") as f:
        for row in new:
            f.write(json.dumps(row, ensure_ascii=False)+"\n")
    return len(seen)


def show(partition, start, end, root=None):
    if partition == "test" and not (destination(root)/"frozen_rules.json").exists():
        raise ValueError("Test biographies are sealed until rule freeze")
    rows = [r for r in base.read_csv(destination(root)/"sample_500.csv") if r["partition"] == partition]
    for i, row in enumerate(rows, 1):
        if start <= i <= end:
            print(f"\n[{row['sample_id']}] {row['source_key']} NAME {row['name_cleaned']}")
            print(row["raw_text_adjusted"])


def original_labels(root=None):
    prior = base.locations(root)[2]
    return {r["source_key"]: dict(decision=r["decision"],
                                expected_name=r["proposed_full_name"] if r["decision"] == "proposed_fix" else r["name_cleaned"])
            for r in base.read_csv(prior / "review_decisions.csv")}


def outcome(baseline, prediction, label):
    if label["decision"] == "unresolved":
        if prediction == baseline:
            return "unresolved_unchanged"
        if re.sub(r"\s", "", prediction) == re.sub(r"\s", "", baseline):
            return "unresolved_format_only"
        return "unconfirmed_change"
    expected = label["expected_name"]
    if prediction == expected:
        return "correct_repair" if prediction != baseline else "correct_unchanged"
    return "missed_repair" if prediction == baseline else "incorrect_change"


def evaluate_rows(rows, gold, config, disabled=(), overrides=None):
    predictions = []
    for row in rows:
        label = gold[row["source_key"]]
        prediction = engine.apply(row["name_cleaned"], config, disabled)
        cleaned = prediction["name_regex_cleaned"]
        if engine.apply(cleaned, config, disabled)["name_regex_cleaned"] != cleaned:
            raise ValueError(f"Non-idempotent strategy at {row['source_key']}")
        override = (overrides or {}).get(row["source_key"], "")
        predictions.append(dict(source_key=row["source_key"], sample_id=row["sample_id"],
                                partition=row.get("partition", "original"), name_raw=row["name_raw"],
                                name_period_cleaned=row["name_cleaned"], name_regex_cleaned=cleaned,
                                name_sample_final=override or cleaned, manual_override_applied=str(bool(override)).lower(),
                                manual_override_name=override, expected_name=label["expected_name"],
                                review_decision=label["decision"], outcome=outcome(row["name_cleaned"], cleaned, label),
                                rule_ids=json.dumps(prediction["rule_ids"]), rule_trace=json.dumps(prediction["trace"], ensure_ascii=False),
                                review_flags=json.dumps(prediction["review_flags"])))
    return predictions


def metrics(predictions):
    outcomes = collections.Counter(r["outcome"] for r in predictions)
    by_rule = collections.defaultdict(collections.Counter)
    for row in predictions:
        for rule_id in json.loads(row["rule_ids"]):
            by_rule[rule_id][row["outcome"]] += 1
    changed = sum(r["name_regex_cleaned"] != r["name_period_cleaned"] for r in predictions)
    repairs = outcomes["correct_repair"]
    mistakes = outcomes["incorrect_change"] + outcomes["unconfirmed_change"]
    recoverable = sum(r["review_decision"] == "proposed_fix" for r in predictions)
    return dict(rows=len(predictions), changed=changed, correct_repairs=repairs, incorrect_changes=outcomes["incorrect_change"],
                unconfirmed_changes=outcomes["unconfirmed_change"], unresolved_format_only=outcomes["unresolved_format_only"],
                recoverable_name_errors=recoverable, remaining_recoverable_errors=recoverable-repairs,
                repair_precision=repairs/(repairs+mistakes) if repairs+mistakes else None,
                recoverable_error_recall=repairs/recoverable if recoverable else None,
                outcomes=dict(outcomes), by_rule={k: dict(v) for k, v in by_rule.items()})


def development_material(root=None):
    dest = destination(root)
    rows = base.read_csv(dest / "development_sample.csv")
    reviewed = labels(root)
    development = [r for r in reviewed if r["partition"] == "development"]
    if len(development) != 300 or any(r["partition"] == "test" for r in reviewed):
        raise ValueError("Need exactly 300 development reviews and zero test reviews for calibration")
    gold = {r["source_key"]: r for r in development}
    old = base.read_csv(base.locations(root)[2] / "sample_500.csv")
    old_gold = original_labels(root)
    surnames = []
    for row in [*gold.values(), *old_gold.values()]:
        name = row["expected_name"]
        if row["decision"] != "unresolved" and "," in name and engine.structural_ok(name):
            surnames.append(name.split(",")[0])
    return rows, gold, old, old_gold, engine.candidate_configuration(surnames)


def calibrate(root=None):
    dest = destination(root)
    if (dest / "frozen_rules.json").exists():
        raise ValueError("Rules are already frozen")
    rows, gold, old, old_gold, config = development_material(root)
    versions = {"broad_first_token": {**config, "rules": [dict(r, kind="broad_separator") if r["kind"] == "surname_separator" else r for r in config["rules"]]},
                "audited_surnames": config}
    for version, rules in versions.items():
        directory = dest / "development_iterations" / version
        directory.mkdir(parents=True, exist_ok=True)
        base.write_json(directory / "configuration.json", rules)
        results = {}
        for partition, records, truth in [("development", rows, gold), ("original", old, old_gold)]:
            predictions = evaluate_rows(records, truth, rules)
            base.write_csv(directory / f"{partition}_predictions.csv", predictions, predictions[0].keys())
            results[partition] = metrics(predictions)
        base.write_json(directory / "metrics.json", results)
    return {v: read_json(dest/"development_iterations"/v/"metrics.json") for v in versions}


def freeze(root=None):
    dest = destination(root)
    if (dest / "frozen_rules.json").exists():
        raise ValueError("Rules are already frozen")
    rows, gold, old, old_gold, config = development_material(root)
    selected = dest / "development_iterations/audited_surnames"
    if read_json(selected / "configuration.json") != config:
        raise ValueError("Development configuration changed after evaluation")
    for records, truth in [(rows, gold), (old, old_gold)]:
        score = metrics(evaluate_rows(records, truth, config))
        if score["incorrect_changes"] or score["unconfirmed_changes"]:
            raise ValueError("Development or original-sample incorrect/unconfirmed changes prevent freeze")
    score = metrics(evaluate_rows(rows, gold, config))
    if not score["correct_repairs"]:
        raise ValueError("No demonstrated development improvement")
    dev_labels = sorted(gold.values(), key=lambda r: int(r["sample_id"]))
    base.write_csv(dest / "development_labels_frozen.csv", dev_labels, LABEL_FIELDS)
    base.write_json(dest / "frozen_rules.json", dict(configuration=config,
                    frozen_at_utc=datetime.now(timezone.utc).isoformat(), test_reviews_at_freeze=0,
                    engine_sha256=base.digest(Path(engine.__file__)),
                    development_labels_sha256=base.digest(dest / "development_labels_frozen.csv"),
                    development_metrics=score,
                    sample_manifest_sha256=base.digest(dest / "sample_manifest.json")))
    return score


def checked_material(root=None):
    """Check seals and explicit reviews before evaluation or publication."""
    dest = destination(root)
    frozen = read_json(dest / "frozen_rules.json")
    if base.digest(Path(engine.__file__)) != frozen["engine_sha256"]:
        raise ValueError("Frozen regex engine changed")
    if base.digest(dest / "sample_manifest.json") != frozen["sample_manifest_sha256"]:
        raise ValueError("Frozen sample manifest changed")
    manifest = read_json(dest / "sample_manifest.json")
    for name, checksum in manifest["frozen_outputs"].items():
        if base.digest(dest / name) != checksum:
            raise ValueError(f"Frozen sample changed: {name}")
    rows = base.read_csv(dest / "sample_500.csv")
    reviewed = sorted(labels(root), key=lambda r: int(r["sample_id"]))
    if len(reviewed) != 500 or len({r["source_key"] for r in reviewed}) != 500:
        raise ValueError("Need 500 unique explicit reviews")
    gold = {r["source_key"]: r for r in reviewed}
    if collections.Counter(r["partition"] for r in reviewed) != {"development": 300, "test": 200}:
        raise ValueError("Review partition sizes changed")
    for row in rows:
        label = gold[row["source_key"]]
        if any(label[k] != row[k] for k in ("sample_id", "partition")):
            raise ValueError("Review identity changed")
        if label["full_bio_reviewed"] != "true" or not label["explanation"] or not label["error_pattern"]:
            raise ValueError("Incomplete review")
        if not label["evidence"] or label["evidence"] not in row["raw_text_adjusted"]:
            raise ValueError("Review lacks literal biography evidence")
        decision, expected = label["decision"], label["expected_name"]
        if decision == "no_change" and expected == row["name_cleaned"]:
            continue
        if decision == "proposed_fix" and expected and expected != row["name_cleaned"]:
            continue
        if decision == "unresolved" and not expected:
            continue
        raise ValueError("Inconsistent review decision")
    dev = [r for r in reviewed if r["partition"] == "development"]
    if (dev != base.read_csv(dest / "development_labels_frozen.csv") or
            base.digest(dest / "development_labels_frozen.csv") != frozen["development_labels_sha256"]):
        raise ValueError("Development labels changed after freeze")
    return rows, reviewed, gold, frozen


def withheld_rules(predictions):
    """Withhold every rule participating in an incorrect/unconfirmed test change."""
    failed = collections.defaultdict(list)
    for row in predictions:
        if row["outcome"] in {"incorrect_change", "unconfirmed_change"}:
            for rule_id in json.loads(row["rule_ids"]):
                failed[rule_id].append(dict(source_key=row["source_key"], outcome=row["outcome"]))
    return dict(failed)


def evaluate_test(root=None):
    """One held-out evaluation. Subsequent publication reads the cached predictions."""
    dest = destination(root)
    if (dest / "test_evaluation.json").exists() or (dest / "test_predictions.csv").exists():
        raise ValueError("Held-out evaluation already exists; validate cached results instead")
    rows, reviewed, gold, frozen = checked_material(root)
    test = [r for r in rows if r["partition"] == "test"]
    predictions = evaluate_rows(test, gold, frozen["configuration"])
    base.write_csv(dest / "test_predictions.csv", predictions, predictions[0].keys())
    result = dict(evaluated_at_utc=datetime.now(timezone.utc).isoformat(),
                  evaluation_count=1, test_reviews_completed=200,
                  manual_labels_sha256=base.digest(dest / "manual_labels.jsonl"),
                  frozen_rules_sha256=base.digest(dest / "frozen_rules.json"),
                  predictions_sha256=base.digest(dest / "test_predictions.csv"),
                  metrics=metrics(predictions), withheld_rules=withheld_rules(predictions))
    base.write_json(dest / "test_evaluation.json", result)
    return result


def cached_test(root=None):
    dest = destination(root)
    result = read_json(dest / "test_evaluation.json")
    for filename, key in [("manual_labels.jsonl", "manual_labels_sha256"),
                          ("frozen_rules.json", "frozen_rules_sha256"),
                          ("test_predictions.csv", "predictions_sha256")]:
        if base.digest(dest / filename) != result[key]:
            raise ValueError(f"Evaluated material changed: {filename}")
    return base.read_csv(dest / "test_predictions.csv"), result


def publish(root=None):
    dest = destination(root)
    if (dest / "completion_manifest.json").exists():
        raise ValueError("Completed release is immutable; use validate")
    rows, reviewed, gold, frozen = checked_material(root)
    test, evaluation = cached_test(root)
    config = frozen["configuration"]
    disabled = sorted(evaluation["withheld_rules"])
    development = evaluate_rows([r for r in rows if r["partition"] == "development"], gold, config)
    frozen_predictions = sorted(development + test, key=lambda r: int(r["sample_id"]))
    base.write_csv(dest / "sample_frozen_predictions.csv", frozen_predictions, frozen_predictions[0].keys())
    recommended = evaluate_rows(rows, gold, config, disabled) if disabled else frozen_predictions
    base.write_json(dest / "recommended_rules.json", dict(configuration=config, disabled_rule_ids=disabled,
                    reason="Withhold complete failing rules; do not retune on test results",
                    frozen_rules_sha256=base.digest(dest / "frozen_rules.json")))
    vocabulary = engine.surname_alternation(config["audited_surnames"])
    catalog = []
    for order, rule in enumerate(config["rules"], 1):
        entry = dict(order=order, **rule)
        if rule["kind"] == "surname_separator":
            entry.update(compiled_pattern=rf"(?P<surname>{vocabulary})[ \t]+(?P<given>{engine.GIVEN})",
                         replacement=r"\g<surname>, \g<given>", match_mode="fullmatch; no existing comma; structural guard; reject suffix-only given names")
        elif rule["kind"] == "prefix":
            entry.update(compiled_pattern=rule["pattern"] + rf"(?P<name>{vocabulary}, {engine.GIVEN}(?:, {engine.SUFFIX})?)",
                         replacement=r"\g<name>", match_mode="fullmatch; structural guard on surviving name")
        elif rule["kind"] == "word_joins":
            entry["match_mode"] = "surname fullmatch; given-name bounded substitutions; structural guard"
        catalog.append(entry)
    base.write_json(dest / "rule_catalog.json", catalog)
    # The combined copy retains every sampled source field. Only derived fields are added.
    copy_rows = [{**row, **{k: v for k, v in prediction.items() if k not in row}}
                 for row, prediction in zip(rows, recommended)]
    base.write_csv(dest / "sample_cleaned_copy.csv", copy_rows, copy_rows[0].keys())
    base.write_csv(dest / "review_decisions.csv", reviewed, LABEL_FIELDS)
    proposals = []
    for label in reviewed:
        if label["decision"] == "proposed_fix":
            parts = label["expected_name"].split(", ")
            proposals.append(dict(**label, proposed_surname=parts[0],
                                  proposed_given_names=parts[1] if len(parts)>1 else "",
                                  proposed_suffix=", ".join(parts[2:])))
    base.write_csv(dest / "proposed_name_fixes.csv", proposals,
                   LABEL_FIELDS + ["proposed_surname", "proposed_given_names", "proposed_suffix"])
    prior = base.locations(root)[2]
    old = base.read_csv(prior / "sample_500.csv")
    overrides = {r["source_key"]: r["proposed_full_name"] for r in base.read_csv(prior / "proposed_name_fixes.csv")}
    old_predictions = evaluate_rows(old, original_labels(root), config, disabled, overrides)
    base.write_csv(dest / "original_sample_predictions.csv", old_predictions, old_predictions[0].keys())
    old_copy = [{**row, **{k: v for k, v in prediction.items() if k not in row}}
                for row, prediction in zip(old, old_predictions)]
    base.write_csv(dest / "original_sample_cleaned_copy.csv", old_copy, old_copy[0].keys())
    # Copy original override evidence, including the record-specific ANGELL corroboration.
    manual = base.read_csv(prior / "proposed_name_fixes.csv")
    base.write_csv(dest / "original_manual_overrides.csv", manual, manual[0].keys())
    queue = [r for r in copy_rows if r["review_decision"] == "unresolved" or json.loads(r["review_flags"])
             or r["outcome"] in {"missed_repair", "incorrect_change", "unconfirmed_change"}]
    base.write_csv(dest / "review_queue.csv", queue, copy_rows[0].keys())
    changes = [dict(sample_group=group, **r) for group, predictions in
               [("new", recommended), ("original", old_predictions)] for r in predictions
               if r["name_sample_final"] != r["name_period_cleaned"]]
    base.write_csv(dest / "change_log.csv", changes, ["sample_group"] + list(old_predictions[0]))
    scores = dict(development=metrics(development), test=evaluation["metrics"], original=metrics(old_predictions))
    base.write_json(dest / "performance.json", scores)
    by_rule = []
    for partition, score in scores.items():
        for order, rule in enumerate(config["rules"], 1):
            counts = score["by_rule"].get(rule["rule_id"], {})
            by_rule.append(dict(partition=partition, order=str(order), rule_id=rule["rule_id"],
                                affected_records=sum(counts.values()), correct_repairs=counts.get("correct_repair", 0),
                                incorrect_changes=counts.get("incorrect_change", 0),
                                unconfirmed_changes=counts.get("unconfirmed_change", 0),
                                unresolved_format_only=counts.get("unresolved_format_only", 0),
                                withheld=str(rule["rule_id"] in disabled).lower()))
    base.write_csv(dest / "performance_by_rule.csv", by_rule, by_rule[0].keys())
    write_report(dest, scores, reviewed, rows, old, overrides, evaluation, config)
    checks = validate(root, require_complete=False)
    code_files = [Path(__file__), Path(engine.__file__), Path(base.__file__),
                  Path(__file__).with_name("test_amws_name_regex_calibration.py")]
    base.write_json(dest / "completion_manifest.json", dict(completed_at_utc=datetime.now(timezone.utc).isoformat(),
                    validation=checks, code_checksums={str(p.resolve()): base.digest(p) for p in code_files},
                    output_checksums={str(p.relative_to(dest)): base.digest(p) for p in sorted(dest.rglob("*")) if p.is_file()}))
    return scores


def validate(root=None, require_complete=True):
    dest = destination(root)
    rows, reviewed, gold, frozen = checked_material(root)
    test, evaluation = cached_test(root)
    manifest = read_json(dest / "sample_manifest.json")
    for path, checksum in manifest["input_checksums"].items():
        if base.digest(Path(path)) != checksum:
            raise ValueError(f"Protected source/prior release changed: {path}")
    lookup, checksum, _ = base.load_source(root)
    old = base.read_csv(base.locations(root)[2] / "sample_500.csv")
    if checksum != manifest["source_sha256"] or make_sample(lookup, old) != rows:
        raise ValueError("Source values, sample or split not reproducible")
    if len(rows) != 500 or len({r["source_key"] for r in rows}) != 500:
        raise ValueError("Sample size or uniqueness changed")
    if {r["source_key"] for r in rows} & {r["source_key"] for r in old}:
        raise ValueError("New sample overlaps original IDs")
    test_groups = {r["biography_group"] for r in rows if r["partition"] == "test"}
    other_groups = {r["biography_group"] for r in rows if r["partition"] == "development"} | {body_key(r) for r in old}
    if test_groups & other_groups:
        raise ValueError("Biography leakage into test")
    for partition, size in [("development", 300), ("test", 200)]:
        expected = [r for r in rows if r["partition"] == partition]
        if len(expected) != size or base.read_csv(dest / f"{partition}_sample.csv") != expected:
            raise ValueError("Partition data changed")
    config = frozen["configuration"]
    # Deterministic verification of the cached evaluation, never a second rule search.
    expected_test = evaluate_rows([r for r in rows if r["partition"] == "test"], gold, config)
    if test != expected_test or metrics(test) != evaluation["metrics"]:
        raise ValueError("Cached test predictions do not reproduce")
    if withheld_rules(test) != evaluation["withheld_rules"] or evaluation["evaluation_count"] != 1:
        raise ValueError("Invalid test withholding/evaluation metadata")
    recommended = read_json(dest / "recommended_rules.json")
    if recommended["configuration"] != config or recommended["disabled_rule_ids"] != sorted(evaluation["withheld_rules"]):
        raise ValueError("Recommendation retuned frozen rules")
    disabled = recommended["disabled_rule_ids"]
    overrides = {r["source_key"]: r["proposed_full_name"] for r in base.read_csv(base.locations(root)[2] / "proposed_name_fixes.csv")}
    if len(overrides) != 14:
        raise ValueError("Expected exactly 14 original manual overrides")
    for records, truth, manual, filename in [(rows, gold, {}, "sample_cleaned_copy.csv"),
                                            (old, original_labels(root), overrides, "original_sample_cleaned_copy.csv")]:
        predictions = evaluate_rows(records, truth, config, disabled, manual)
        expected = [{**row, **{k: v for k, v in prediction.items() if k not in row}}
                    for row, prediction in zip(records, predictions)]
        if base.read_csv(dest / filename) != expected:
            raise ValueError(f"Copy changed protected fields or predictions: {filename}")
        if any(r["name_cleaned"] != r["name_raw"].replace(".", ",") for r in expected):
            raise ValueError("Literal period baseline changed")
        if manual and sum(p["manual_override_applied"] == "true" for p in predictions) != 14:
            raise ValueError("Manual override coverage changed")
    if require_complete:
        complete = read_json(dest / "completion_manifest.json")
        for name, expected in complete["output_checksums"].items():
            if base.digest(dest / name) != expected:
                raise ValueError(f"Completed output changed: {name}")
        for path, expected in complete["code_checksums"].items():
            if base.digest(Path(path)) != expected:
                raise ValueError(f"Release code changed: {path}")
    return dict(source_rows_1986=len(lookup), new_sample=500, original_sample=500,
                development=300, test=200, completed_reviews=len(reviewed), original_overrides=14,
                sample_reproduced=True, protected_inputs_unchanged=True, duplicate_biography_leakage=False,
                source_fields_preserved=True, idempotence_checked_on_1000_records=True,
                frozen_test_evaluation_verified=True)


def write_report(dest, scores, reviewed, rows, old, overrides, evaluation, config):
    lines = ["# AMWS name regex calibration: 2026-10-23", "",
             "This release changes sample copies only. The v06 source and the 2026-10-22 release are preserved.", "",
             "## Sample and review", "",
             "From all 79,073 v06 1986 records (including missing names), exclude the original 500 IDs. "
             "Sort the remaining 78,573 by source_key and draw 500 without replacement using Python random.Random(43). "
             "Partition with seed 44 into 300 development and 200 test records, grouping duplicate biographies. "
             "The frozen sample contains zero duplicate biography groups and zero biographies shared with the original sample. "
             "Duplicate detection hashes NFKC/whitespace-normalized text beginning at the standalone lowercase birth marker, "
             "or the full text when that marker is absent; it is not fuzzy person deduplication.", "",
             "All 500 biographies were read in full. Labels contain literal evidence and explanations; "
             "no_change means no additional correction is supported by that biography, rather than independently verified spelling. "
             "Unresolved names have no asserted target and never count as successful repairs.", "",
             "| Partition | No change | Supported fix | Unresolved |", "|---|---:|---:|---:|"]
    for part in ("development", "test"):
        counts = collections.Counter(r["decision"] for r in reviewed if r["partition"] == part)
        lines.append(f"| {part} | {counts['no_change']} | {counts['proposed_fix']} | {counts['unresolved']} |")
    lines += ["", "The development labels were completed before candidate evaluation. Rules, audited surname vocabulary "
              "and engine checksum were frozen before any test biography was displayed or labeled. "
              "The 200 test biographies were then reviewed independently of candidate outputs. "
              "One frozen test evaluation is cached in test_evaluation.json; subsequent checks reproduce it without searching new rules.", "",
              "## Results", "", "Counts below assess regex predictions against the period-cleaned baseline, excluding manual overrides. "
              "Changes that only format an unresolved name are reported separately. Repair precision excludes those unresolved "
              "format-only changes and counts any other unconfirmed changes as failures. Unresolved names are excluded from recall.", "",
              "| Sample | Rows | Regex changes | Correct repairs | Incorrect / unconfirmed | Unresolved format only | Recoverable misses |",
              "|---|---:|---:|---:|---:|---:|---:|"]
    for part, m in scores.items():
        lines.append(f"| {part} | {m['rows']} | {m['changed']} | {m['correct_repairs']} | "
                     f"{m['incorrect_changes']} / {m['unconfirmed_changes']} | {m['unresolved_format_only']} | {m['remaining_recoverable_errors']} |")
    lines += ["", "Development repaired 6/6 supported errors; original-sample regression repaired 13/14. "
              "These are calibration/regression results, not independent estimates. "
              f"The held-out test repaired {scores['test']['correct_repairs']}/{scores['test']['recoverable_name_errors']} supported errors. "
              "When a partition has no confirmed or incorrect substantive repairs, repair precision is undefined, not 100%. "
              "The small held-out set does not establish population accuracy.", "",
              "The test miss is `DER, ED WARD LAWSON` → `DER, EDWARD LAWSON`. The bounded join `EDW ARD` "
              "does not cover `ED WARD`. The original surname DER is retained as transcribed; no missing surname prefix is asserted. "
              "This miss is queued for a later development round with a fresh test set; it was not added to the frozen rules.", "",
              f"Withheld rules after test: {', '.join(sorted(evaluation['withheld_rules'])) or 'none'}. "
              "Any rule participating in an incorrect or unconfirmed test change is removed in full from the recommendation, "
              "without changing its pattern or using this test set to tune a replacement.", "",
              "## Ordered rules", "",
              "Every name first receives literal period-to-comma replacement, including initials and suffixes. "
              "The following rules then run in the listed order. Exact patterns and replacement strings are recorded in "
              "frozen_rules.json; dynamic boundary patterns use its frozen audited_surnames alternation. "
              "Each changed record records the affected text and before/after replacement in rule_trace.", ""]
    for order, rule in enumerate(config["rules"], 1):
        lines.append(f"{order}. **{rule['rule_id']}**: {rule['description']}.")
    lines += ["", "S02 keeps the original two given-name joins EDW ARD→EDWARD and LAW RENCE→LAWRENCE "
              "bounded to reviewed words. Development added LOW ELL→LOWELL and W ARREN→WARREN, "
              "and the surname-only joins SCHW ARTZ→SCHWARTZ, W ARNER→WARNER, W ARREN→WARREN. "
              "There is no unrestricted word joining or spelling correction. Damaged or structurally ambiguous headings skip these repairs.", "",
              "S03 is a regex plus a finite audited surname vocabulary, collected only from resolved original/development "
              "labels. It requires an intact known surname followed by plausible uppercase given-name tokens; "
              "longest compound surnames take priority. Unknown boundaries stay flagged. It is not a general surname parser. "
              "S04 removes only the bounded institution/year prefix before an intact known surname-first heading.", "",
              "A broad first-token separator was rejected before freeze: it made an unconfirmed boundary change in "
              "development (`WTTIIAM SANDS`) and another on the original sample. The rejected configuration and "
              "all predictions remain in development_iterations/broad_first_token/. "
              "The selected strategy has zero observed incorrect or unconfirmed changes on either calibration sample.", "",
              "## Rule counts", "",
              "Counts refer to the final name outcome for each record touched by a rule; a record touched by multiple rules "
              "can appear under more than one rule. Zero means the rule was not exercised on that partition.", "",
              "| Partition | Rule | Correct repairs | Incorrect / unconfirmed | Unresolved format only |",
              "|---|---|---:|---:|---:|"]
    for part, score in scores.items():
        for rule in config["rules"]:
            count = score["by_rule"].get(rule["rule_id"], {})
            lines.append(f"| {part} | {rule['rule_id']} | {count.get('correct_repair', 0)} | "
                         f"{count.get('incorrect_change', 0)} / {count.get('unconfirmed_change', 0)} | {count.get('unresolved_format_only', 0)} |")
    lines += ["", "## Copies, overrides and remaining review", "",
              f"The original sample copy applies all {len(overrides)} previously proposed corrections as explicit overrides, "
              "with their original evidence copied to original_manual_overrides.csv. These are recorded separately from "
              "name_regex_cleaned and never enter regex performance counts. The corroborated ANGELL correction remains "
              "record-specific and is not a general letter-deletion rule.", "",
              "The new sample copy applies the recommended regex strategy only. Manual expected names are labels, "
              "not additional automated changes. All 30 unresolved new names, all remaining supported errors and all "
              "regex-flagged cases appear in review_queue.csv with full biographies. Examples include incomplete/suffix-only "
              "headings, missing names, damaged letters and uncertain internal separators. Some alphabetic OCR damage "
              "cannot be detected reliably by regex; manual unresolved decisions are included even when regex flags are empty.", "",
              "sample_500.csv freezes the new draw, full biographies, whole-source-record digests and partition assignments. "
              "sample_cleaned_copy.csv and original_sample_cleaned_copy.csv retain every sampled source field and add "
              "predictions/traces. sample_frozen_predictions.csv preserves the evaluated candidate; recommended_rules.json "
              "records any rule withholding. review_decisions.csv and proposed_name_fixes.csv contain all explicit labels/evidence. "
              "performance.json and performance_by_rule.csv keep development, test and original metrics separate.", "",
              "## Validation", "",
              "The validator reconstructs all 79,073 1986 source lookup rows, the new draw and the exact split. "
              "It checks protected v06/prior-release checksums, record IDs, full biographies and whole-record hashes, "
              "500 disjoint IDs, 300/200 sizes, duplicate leakage, 500 evidence-backed explicit reviews, literal period "
              "replacement and preserved source fields in both sample copies. It checks idempotence on all 1,000 sampled "
              "names, the original 14 overrides, frozen engine/labels, cached test results and output checksums. "
              "Unit tests cover compound surnames, apostrophes, hyphens, expansions, suffixes, internal punctuation, "
              "truncated headings, missing names, bounded joins and neighboring-entry spillover.", ""]
    lines += [f"Period replacement changes {sum(r['period_replaced']=='true' for r in rows)} new-sample names "
              f"and {sum(r['period_replaced']=='true' for r in old)} original-sample names before regex processing.", "",
              "No production name lookup, surname buckets or parent–offspring candidate pairs are published by this calibration.", ""]
    (dest / "calibration_report.md").write_text("\n".join(lines), encoding="utf-8")


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("command", choices=["prepare", "show", "calibrate", "freeze", "evaluate-test", "publish", "validate"])
    parser.add_argument("--root", type=Path)
    parser.add_argument("--partition", choices=["development", "test"], default="development")
    parser.add_argument("--start", type=int, default=1)
    parser.add_argument("--end", type=int, default=30)
    args = parser.parse_args()
    if args.command == "prepare":
        print(prepare(args.root))
    elif args.command == "show":
        show(args.partition, args.start, args.end, args.root)
    elif args.command == "calibrate":
        print(json.dumps(calibrate(args.root), indent=2))
    elif args.command == "freeze":
        print(json.dumps(freeze(args.root), indent=2))
    else:
        command = {"evaluate-test": evaluate_test, "publish": publish, "validate": validate}[args.command]
        print(json.dumps(command(args.root), indent=2))


if __name__ == "__main__":
    main()
