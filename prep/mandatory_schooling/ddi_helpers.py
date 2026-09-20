"""Read an IPUMS DDI 2.5 codebook: variable labels and value labels.

IPUMS ships a DDI (`.xml`) with every extract. It is the only authoritative
description of what a bare CSV extract contains -- variable labels, and the code
-> meaning mapping for every categorical variable. Where it is available it
replaces hand-transcribed documentation, and lets the checked-in notes be
cross-checked rather than merely believed.

Stdlib only, deliberately: `ipumsr::read_ipums_ddi()` is the R-side equivalent,
but the profiler that consumes this is Python and a DDI is a small XML file.

The DDI namespace is `ddi:codebook:2_5`, but IPUMS has shipped files with and
without it over the years, so every lookup here matches on the local tag name.

Use as a library:

    from ddi_helpers import read_ddi, ddi_provenance
    ddi = read_ddi(path)
    ddi["SEX"]["label"]            -> "Sex"
    ddi["SEX"]["values"]["1"]      -> "Male"

Or from the command line:

    python prep/mandatory_schooling/ddi_helpers.py --describe <path to .xml>
"""

import hashlib
import sys
import xml.etree.ElementTree as ET
from pathlib import Path


def _tag(element):
    """Local tag name, with any namespace stripped."""
    return element.tag.rsplit("}", 1)[-1]


def _text(element):
    if element is None or element.text is None:
        return ""
    return " ".join(element.text.split())


def _child(parent, name):
    for child in parent:
        if _tag(child) == name:
            return child
    return None


def _children(parent, name):
    return [child for child in parent if _tag(child) == name]


def _descend(root, *names):
    """Walk a chain of child tags, returning None if any link is missing."""
    node = root
    for name in names:
        if node is None:
            return None
        node = _child(node, name)
    return node


def code_key(value):
    """Normalise a code so a DDI category and an observed CSV value compare equal.

    The DDI may print a code zero-padded (`006`) where the CSV prints it bare
    (`6`), and vice versa. Numeric codes are therefore compared as numbers;
    anything non-numeric is compared as upper-cased text.
    """
    text = str(value).strip()
    if not text:
        return ""
    try:
        number = float(text)
    except ValueError:
        return text.upper()
    if number.is_integer():
        return str(int(number))
    return f"{number:g}"


def read_ddi(path):
    """Parse a DDI into {varname: {"label", "description", "values", "keys"}}.

    `values` is keyed by the code exactly as the DDI prints it, for display.
    `keys` is the same mapping keyed by `code_key()`, for comparison against
    values observed in the data.
    """
    path = Path(path)
    root = ET.parse(path).getroot()
    data_descr = _child(root, "dataDscr")
    if data_descr is None:
        raise ValueError(f"{path.name} has no <dataDscr>; is this a DDI codebook?")

    variables = {}
    for var in _children(data_descr, "var"):
        name = (var.get("name") or var.get("ID") or "").strip()
        if not name:
            continue
        values = {}
        for category in _children(var, "catgry"):
            code = _text(_child(category, "catValu"))
            meaning = _text(_child(category, "labl"))
            if code:
                values[code] = meaning
        variables[name.upper()] = {
            "label": _text(_child(var, "labl")),
            "description": _text(_child(var, "txt")),
            "values": values,
            "keys": {code_key(code): meaning for code, meaning in values.items()},
        }
    if not variables:
        raise ValueError(f"{path.name} parsed to zero variables")
    return variables


def ddi_provenance(path):
    """Identity of the codebook, for the run manifest."""
    path = Path(path)
    digest = hashlib.sha256()
    with path.open("rb") as handle:
        for block in iter(lambda: handle.read(1 << 20), b""):
            digest.update(block)

    root = ET.parse(path).getroot()
    citation = _descend(root, "stdyDscr", "citation")
    notes = [_text(node) for node in _children(root, "notes")]
    if citation is not None:
        notes += [_text(node) for node in _children(citation, "notes")]

    return {
        "file": path.name,
        "path": str(path),
        "bytes": path.stat().st_size,
        "sha256": digest.hexdigest(),
        "n_vars": len(read_ddi(path)),
        "title": _text(_descend(citation, "titlStmt", "titl")) if citation is not None else "",
        "producer": _text(_descend(citation, "prodStmt", "producer")) if citation is not None else "",
        "prod_date": _text(_descend(citation, "prodStmt", "prodDate")) if citation is not None else "",
        "notes": [note for note in notes if note][:4],
    }


def describe(path):
    ddi = read_ddi(path)
    meta = ddi_provenance(path)
    print(f"{meta['file']}  ({meta['bytes'] / 1e6:.2f} MB, sha256 {meta['sha256'][:16]}...)")
    if meta["title"]:
        print(f"  title    : {meta['title']}")
    if meta["prod_date"]:
        print(f"  produced : {meta['prod_date']}")
    print(f"  variables: {len(ddi)}")

    labelled = {name: spec for name, spec in ddi.items() if spec["values"]}
    print(f"  with value labels: {len(labelled)} of {len(ddi)}")
    print()
    for name in sorted(ddi):
        spec = ddi[name]
        n = len(spec["values"])
        print(f"  {name:<14} {spec['label'][:52]:<52} {n if n else '-':>5} codes")

    sample = next((n for n in ("SEX", "SCHOOL", "RELATE") if n in labelled), None)
    if sample:
        print(f"\n  sample decode -- {sample}:")
        for code, meaning in list(ddi[sample]["values"].items())[:12]:
            print(f"    {code:>6} = {meaning}")


if __name__ == "__main__":
    args = sys.argv[1:]
    if len(args) == 2 and args[0] == "--describe":
        describe(args[1])
    elif len(args) == 1:
        describe(args[0])
    else:
        sys.exit("usage: ddi_helpers.py --describe <path to DDI .xml>")
