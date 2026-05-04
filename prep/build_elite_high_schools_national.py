from pathlib import Path

import pandas as pd


ROOT = Path(__file__).resolve().parents[1]
MANUAL = ROOT / "prep" / "input" / "elite_high_schools_national_manual.csv"
ADDITIONS = ROOT / "prep" / "input" / "elite_high_schools_revision_additions.csv"
REVISION_DECISIONS = ROOT / "prep" / "input" / "elite_high_schools_revision_decisions.csv"
REVISION_QUEUE = ROOT / "prep" / "input" / "elite_high_schools_revision_queue.csv"
STATE_BATCHES = ROOT / "prep" / "input" / "elite_high_schools_state_review_batches.csv"
COUNTIES = ROOT / "prep" / "output" / "national_county2020.txt"
COUNTY_PANEL = ROOT / "prep" / "output" / "us_panel_county.csv"
OUT_SCHOOLS = ROOT / "prep" / "output" / "elite_high_schools_national_1800_1930.csv"
OUT_CORE = ROOT / "prep" / "output" / "elite_high_schools_core_1800_1930.csv"
OUT_EXPANDED = ROOT / "prep" / "output" / "elite_high_schools_expanded_1800_1930.csv"
OUT_ROBUSTNESS = ROOT / "prep" / "output" / "elite_high_schools_robustness_only_1800_1930.csv"
OUT_BOUNDARIES = ROOT / "prep" / "output" / "elite_high_schools_benchmarks_and_boundaries.csv"
OUT_STATES = ROOT / "prep" / "output" / "elite_high_schools_state_coverage_1800_1930.csv"
ENROLLMENT = ROOT / "prep" / "output" / "elite_high_schools_enrollment.tsv"


STATE_ORDER = [
    ("Alabama", "AL"),
    ("Alaska", "AK"),
    ("Arizona", "AZ"),
    ("Arkansas", "AR"),
    ("California", "CA"),
    ("Colorado", "CO"),
    ("Connecticut", "CT"),
    ("Delaware", "DE"),
    ("District of Columbia", "DC"),
    ("Florida", "FL"),
    ("Georgia", "GA"),
    ("Hawaii", "HI"),
    ("Idaho", "ID"),
    ("Illinois", "IL"),
    ("Indiana", "IN"),
    ("Iowa", "IA"),
    ("Kansas", "KS"),
    ("Kentucky", "KY"),
    ("Louisiana", "LA"),
    ("Maine", "ME"),
    ("Maryland", "MD"),
    ("Massachusetts", "MA"),
    ("Michigan", "MI"),
    ("Minnesota", "MN"),
    ("Mississippi", "MS"),
    ("Missouri", "MO"),
    ("Montana", "MT"),
    ("Nebraska", "NE"),
    ("Nevada", "NV"),
    ("New Hampshire", "NH"),
    ("New Jersey", "NJ"),
    ("New Mexico", "NM"),
    ("New York", "NY"),
    ("North Carolina", "NC"),
    ("North Dakota", "ND"),
    ("Ohio", "OH"),
    ("Oklahoma", "OK"),
    ("Oregon", "OR"),
    ("Pennsylvania", "PA"),
    ("Rhode Island", "RI"),
    ("South Carolina", "SC"),
    ("South Dakota", "SD"),
    ("Tennessee", "TN"),
    ("Texas", "TX"),
    ("Utah", "UT"),
    ("Vermont", "VT"),
    ("Virginia", "VA"),
    ("Washington", "WA"),
    ("West Virginia", "WV"),
    ("Wisconsin", "WI"),
    ("Wyoming", "WY"),
]


EXAM_ONLY_SCHOOLS = {
    "Stuyvesant High School",
    "Brooklyn Technical High School",
    "Bronx High School of Science",
    "Boston Latin School",
    "Hunter College High School",
}

EXAM_PLUS_SCHOOLS = {
    "Regis High School",
    "Brophy College Preparatory",
    "New Mexico Military Institute",
}

GRADES_TEST_COMBO_SCHOOLS = {
    "Lane Tech College Prep High School",
    "Lowell High School",
    "Central High School",
    "Baltimore City College",
    "Baltimore Polytechnic Institute",
    "duPont Manual High School",
    "Walnut Hills High School",
}

LOTTERY_SCHOOLS = {
    "Louisville Male High School",
}

OPEN_ACCESS_SCHOOLS = {
    "Little Rock Central High School",
    "Omaha Central High School",
    "Shortridge High School",
    "Sumner High School",
    "Booker T. Washington High School",
    "Pearl High School",
    "Howard High School",
}

SENDING_TOWN_OPEN_SCHOOLS = {
    "Burr and Burton Academy",
    "St. Johnsbury Academy",
}

CURRENT_MEDIUM_ACCESS_SCHOOLS = {
    "Brophy College Preparatory",
    "New Mexico Military Institute",
}

SPECIAL_INSTITUTION_SCHOOLS = {
    "University of Chicago Laboratory Schools",
    "New Mexico Military Institute",
}

CURRENT_SOURCE_OVERRIDES = {
    "Stuyvesant High School": {
        "current_admissions_source_url": "https://www.schools.nyc.gov/learning/testing/specialized-high-school-admissions-test",
        "current_access_source_url": "https://www.schools.nyc.gov/enrollment/enroll-grade-by-grade/specialized-high-schools/discovery-programs",
    },
    "Brooklyn Technical High School": {
        "current_admissions_source_url": "https://www.bths.edu/apps/pages/index.jsp?type=d&uREC_ID=229376",
        "current_access_source_url": "https://www.schools.nyc.gov/learning/testing/specialized-high-school-admissions-test",
    },
    "Regis High School": {
        "current_admissions_source_url": "https://regis.org/admissions/application-process",
        "current_access_source_url": "https://www.regis.org/?Fuseaction=Admission",
    },
    "Brophy College Preparatory": {
        "current_admissions_source_url": "https://www.brophyprep.org/admissions/apply/entrance-exam",
        "current_access_source_url": "https://www.brophyprep.org/admissions/tuition-and-financial-assistance",
    },
    "Lane Tech College Prep High School": {
        "current_admissions_source_url": "https://lanetech.org/enrollment/admissions/",
        "current_access_source_url": "https://lanetech.org/enrollment/admissions/",
    },
    "Lowell High School": {
        "current_admissions_source_url": "https://www.sfusd.edu/schools/enroll/apply/applying-lowell-high-school",
        "current_access_source_url": "https://www.sfusd.edu/schools/enroll/apply/applying-lowell-high-school",
    },
    "Central High School": {
        "current_admissions_source_url": "https://centralhs.philasd.org/about-central-high-school/admission-requirements/",
        "current_access_source_url": "https://centralhs.philasd.org/about-central-high-school/admission-requirements/",
    },
    "Baltimore City College": {
        "current_admissions_source_url": "https://www.baltimorecitycollege.us/in-district-admissions",
        "current_access_source_url": "https://www.baltimorecitycollege.us/in-district-admissions",
    },
    "Baltimore Polytechnic Institute": {
        "current_admissions_source_url": "https://www.baltimorecityschools.org/o/poly/page/admissions/",
        "current_access_source_url": "https://www.baltimorecityschools.org/o/poly/page/how-to-apply/",
    },
    "duPont Manual High School": {
        "current_admissions_source_url": "https://admissions.dupontmanual.com/",
        "current_access_source_url": "https://admissions.dupontmanual.com/",
    },
    "Louisville Male High School": {
        "current_admissions_source_url": "https://www.jefferson.kyschools.us/high-school-choices",
        "current_access_source_url": "https://www.jefferson.kyschools.us/high-school-choices",
    },
    "New Mexico Military Institute": {
        "current_admissions_source_url": "https://www.nmmi.edu/admissions/qualifications-for-admission/",
        "current_access_source_url": "https://www.nmmi.edu/admissions/financial-aid-scholarships/",
    },
    "Burr and Burton Academy": {
        "current_admissions_source_url": "https://www.burrburton.org/admissions-and-enrollment/local-enrollment-and-admissions",
        "current_access_source_url": "https://www.burrburton.org/admissions-and-enrollment/local-enrollment-and-admissions/tuition-and-financial-aid",
    },
    "St. Johnsbury Academy": {
        "current_admissions_source_url": "https://stjacademy.org/admissions/applying-as-a-day-student/",
        "current_access_source_url": "https://stjacademy.org/admissions/affording-sja/",
    },
    "Little Rock Central High School": {
        "current_admissions_source_url": "https://www.lrsd.org/article/2516610",
        "current_access_source_url": "https://www.lrsd.org/page/student-registration/",
    },
    "Shortridge High School": {
        "current_admissions_source_url": "https://shortridge.myips.org/admissions",
        "current_access_source_url": "https://shortridge.myips.org/admissions",
    },
    "Omaha Central High School": {
        "current_admissions_source_url": "https://central.ops.org/students-families/attend-central-high",
        "current_access_source_url": "https://central.ops.org/students-families/attend-central-high",
    },
    "Sumner Academy of Arts and Science": {
        "current_admissions_source_url": "https://www.kckps.org/images/departments/student_services/parent_handbook.pdf",
        "current_access_source_url": "https://www.kckps.org/images/departments/student_services/parent_handbook.pdf",
    },
    "Sumner High School": {
        "current_admissions_source_url": "https://sumner.slps.org/",
        "current_access_source_url": "https://sumner.slps.org/",
    },
}

HISTORICAL_SOURCE_OVERRIDES = {
    "Regis High School": "https://www.regis.org/about/",
    "McDonogh School": "https://www.mcdonogh.org/about/history",
    "Burr and Burton Academy": "https://www.burrburton.org/",
    "St. Johnsbury Academy": "https://stjacademy.org/the-sja-experience/history/",
    "Sumner Academy of Arts and Science": "https://sumner.kckschools.org/about/our-school",
    "Sumner High School": "https://sumner.slps.org/about-us/about-sumner-high/our-history",
}


def norm_text(series: pd.Series) -> pd.Series:
    return (
        series.fillna("")
        .astype(str)
        .str.strip()
        .str.lower()
        .str.replace(r"\s+", " ", regex=True)
    )


def derive_current_admission_code(row: pd.Series) -> str:
    school = row["school"]
    prelim = row["admission_model_current_prelim"]
    access_prelim = row["access_model_current_prelim"]
    school_type = row["school_type"].lower()

    if school in EXAM_ONLY_SCHOOLS:
        return "exam_only"
    if school in EXAM_PLUS_SCHOOLS:
        return "exam_plus"
    if school in GRADES_TEST_COMBO_SCHOOLS:
        return "grades_test_combo"
    if school in LOTTERY_SCHOOLS:
        return "lottery"
    if school in OPEN_ACCESS_SCHOOLS:
        return "open_access"
    if school in SENDING_TOWN_OPEN_SCHOOLS:
        return "sending_town_open"
    if prelim == "exam_based":
        return "exam_plus"
    if prelim == "merit_selective_nonexam":
        return "public_selective_nonexam"
    if prelim in {"open_access", "open_access_public"}:
        return "open_access"
    if "public" in school_type and access_prelim == "public_free_selective":
        return "public_selective_nonexam"
    return "holistic_private"


def derive_test_based(admission_code: str) -> str:
    return "yes" if admission_code in {"exam_only", "exam_plus", "grades_test_combo"} else "no"


def derive_current_poor_access(row: pd.Series, admission_code: str) -> str:
    school = row["school"]
    access_prelim = row["access_model_current_prelim"]
    if school in CURRENT_MEDIUM_ACCESS_SCHOOLS:
        return "medium"
    if access_prelim in {"public_free_selective", "free_merit"}:
        return "high"
    if school in SENDING_TOWN_OPEN_SCHOOLS or school in OPEN_ACCESS_SCHOOLS:
        return "high"
    return "low"


def derive_lineage_risk(row: pd.Series) -> str:
    continuity = row["continuity_status"]
    if continuity == "later_merger_use_roots":
        return "high"
    if continuity == "reorganized_continuity":
        return "medium"
    return "low"


def derive_comparability_class(row: pd.Series) -> str:
    school = row["school"]
    school_type = row["school_type"].lower()
    continuity = row["continuity_status"]

    if continuity == "later_merger_use_roots":
        return "later_merger_lineage"
    if school in SPECIAL_INSTITUTION_SCHOOLS:
        return "special_institution"
    if "girls" in school_type:
        return "girls_academy"
    if row["poor_access_historical"] == "high" and "public" in school_type:
        return "public_access_school"
    if row["poor_access_historical"] == "high":
        return "high_access_nonpublic_school"
    return "standard_candidate"


def derive_sample_role(row: pd.Series) -> str:
    # Special-model atypical schools (lab, military) recorded but kept out
    # of the default core sample.
    if row["school"] in SPECIAL_INSTITUTION_SCHOOLS:
        return "robustness_only"
    # Merger predecessors that operated independently for >= 20 years before
    # the merger qualify for core under the active-20-year rule (the merger
    # itself does not reset the institutional clock).
    if row["continuity_status"] == "later_merger_use_roots":
        return "core_with_caution"
    if row["continuity_status"] == "reorganized_continuity":
        return "core_with_caution"
    return "core"


def derive_local_access_relevance(row: pd.Series) -> str:
    access = row["poor_access_historical"]
    if access == "high":
        return "high"
    if access == "medium":
        return "medium"
    return "low"


def derive_historical_poor_access(row: pd.Series) -> tuple[str, str, str]:
    school = row["school"]
    school_type = row["school_type"].lower()
    continuity = row["continuity_status"]

    if school == "Regis High School":
        return (
            "high",
            "Founded as a tuition-free scholarship school; strong direct access for talented poor Catholic boys.",
            "source_checked",
        )
    if school == "McDonogh School":
        return (
            "high",
            "Founded explicitly for poor boys; later evolved into a broader elite private school.",
            "source_checked",
        )
    if school in SENDING_TOWN_OPEN_SCHOOLS:
        return (
            "high",
            "Town-tuition/public-mission structure created broad local access without private-pay tuition.",
            "source_checked",
        )
    if school == "Sumner Academy of Arts and Science":
        return (
            "high",
            "Public academic-screen school with roots as an important Black public high school; low tuition barriers but strong racial and geographic constraints shaped who could access it historically.",
            "source_checked",
        )
    if school == "Sumner High School":
        return (
            "high",
            "Historically important Black public academic high school with low tuition barriers, though segregation and geography strongly shaped access in practice.",
            "source_checked",
        )
    if "public" in school_type:
        return (
            "high",
            "Public or publicly funded model meant low direct tuition barriers, although earlier eras often had sex, race, or residency exclusions.",
            "rule_based_public_model",
        )
    if school == "New Mexico Military Institute":
        return (
            "medium",
            "Publicly chartered but tuition-charging; scholarships widened access but did not eliminate price barriers.",
            "source_checked",
        )
    if continuity == "later_merger_use_roots":
        return (
            "low",
            "Elite lineage predates 1930, but predecessor institutions were generally tuition-charging private schools with limited poor access.",
            "rule_based_private_model",
        )
    return (
        "low",
        "Elite private tuition model; poor access likely depended on limited scholarships, patronage, or church support rather than open low-cost admission.",
        "rule_based_private_model",
    )


def derive_coding_status(row: pd.Series) -> str:
    school = row["school"]
    if school in CURRENT_SOURCE_OVERRIDES and school in HISTORICAL_SOURCE_OVERRIDES:
        return "source_checked_current_and_historical"
    if school in CURRENT_SOURCE_OVERRIDES:
        return "source_checked_current_rule_based_historical"
    if school in HISTORICAL_SOURCE_OVERRIDES:
        return "rule_based_current_source_checked_historical"
    return "rule_based_from_school_type"


###############################################################################
# Inclusion / exclusion criterion dummies
#
# Each criterion is recorded as its own yes/no/unknown column so that any
# composite definition (default high-access, robustness variants) can be
# rebuilt downstream without re-deriving the underlying judgments.
###############################################################################

# Atypical institutional models — recorded but excluded from default high-access.
SPECIAL_MODEL_SCHOOLS = {
    "University of Chicago Laboratory Schools",
    "New Mexico Military Institute",
}

# Private schools founded with explicit tuition-free / poor-mission charters.
TUITION_FREE_PRIVATE_HISTORICAL = {
    "Regis High School",
    "McDonogh School",
}

# Sending-town academies are tuition-free for residents (town pays per pupil)
# but admission is open within the sending towns; coded tuition-free YES,
# selective NO.
SENDING_TOWN_ACADEMIES = {
    "Burr and Burton Academy",
    "St. Johnsbury Academy",
}

# Schools selective by ability at founding. Includes:
#   (a) public exam-admit schools that were selective from day one
#   (b) tuition-free private scholarship schools (Regis, McDonogh) that
#       admitted by merit
# Private elite prep schools (Andover, Hotchkiss, etc.) that admit by test +
# interview are also selective by merit but they are pay-to-attend, so they
# are coded selective=YES here but tuition_free=NO; the strict composite
# excludes them via the tuition_free criterion.
HISTORICALLY_SELECTIVE_PUBLIC = {
    "Stuyvesant High School",
    "Brooklyn Technical High School",
    "Bronx High School of Science",
    "Hunter College High School",
    "Boston Latin School",
    "Lowell High School",
    "Central High School",
    "Baltimore City College",
    "Baltimore Polytechnic Institute",
    "Walnut Hills High School",
    # Dunbar's founding-era model was a selective Black academic HS (1870-1955)
    # with rigorous admission standards. Flagged historically_unclear because
    # selectivity was lost after 1955 integration.
    "Paul Laurence Dunbar High School",
}

# Founding-era admission model is unclear or transitioned (selective today
# but originally manual-training / open-admit, or selective historically and
# open-admit today). Default classification follows the founding-era model.
HISTORICALLY_UNCLEAR_NOTES = {
    "Lane Tech College Prep High School": (
        "Founded 1908 as Albert G. Lane Manual Training High School "
        "(open-admit vocational track for working-class boys); transitioned "
        "to selective college-prep model in mid-to-late 20th century. "
        "Default: not selective historically."
    ),
    "duPont Manual High School": (
        "Founded 1892 as the manual-training arm of Louisville public high "
        "schools; open admission within Louisville public schools historically; "
        "became selective magnet much later. Default: not selective historically."
    ),
    "Sumner Academy of Arts and Science": (
        "Founded 1905 as Sumner High School / Manual Training School "
        "(segregated Black public HS, open admission within Black community); "
        "reorganized 1978 as the current selective Sumner Academy. Default: "
        "not selective historically."
    ),
    "Paul Laurence Dunbar High School": (
        "Selective Black academic HS 1870-1955 with rigorous admission "
        "standards; after DC school integration in 1955 became open-admit "
        "neighborhood school. Default: selective historically (founding-era "
        "model)."
    ),
    "Louisville Male High School": (
        "Founded 1856 as Louisville's public boys' high school; open admission "
        "within Louisville public schools. Today admits via lottery. Default: "
        "not selective historically."
    ),
    "University of Chicago Laboratory Schools": (
        "K-12 laboratory school of the University of Chicago Department of "
        "Education; serves UChicago faculty children disproportionately. "
        "Atypical model; recorded but excluded from default high-access."
    ),
    "New Mexico Military Institute": (
        "Combined public military high school and 2-year junior college. "
        "Atypical model; recorded but excluded from default high-access."
    ),
    "McDonogh School": (
        "Founded 1873 as a free boarding school for poor boys per the John "
        "McDonogh bequest; transitioned to a tuition-charging private elite "
        "school by the early 20th century. Default: tuition-free historically "
        "(founding-era model)."
    ),
}


def derive_crit_secondary_school(row: pd.Series) -> str:
    return "no" if row["school"] in SPECIAL_MODEL_SCHOOLS else "yes"


def derive_crit_in_frame(row: pd.Series) -> str:
    try:
        yr = int(row["founding_year_used"])
    except (ValueError, TypeError):
        return "unknown"
    return "yes" if 1800 <= yr <= 1940 else "no"


def derive_crit_active_20yr(row: pd.Series) -> str:
    # Every school in our curated list has been active >= 20 years (or is a
    # merger product whose predecessors were). Kept as an explicit dummy so
    # future schools-with-shorter-runs can be flagged here directly.
    return "yes"


def derive_crit_size_ge_50(row: pd.Series, lookup: dict) -> str:
    val = lookup.get(row["school"], "")
    if val in ("", "NA", None):
        return "unknown"
    try:
        return "yes" if float(val) >= 50 else "no"
    except (ValueError, TypeError):
        return "unknown"


def derive_crit_tuition_free_historical(row: pd.Series) -> str:
    school = row["school"]
    school_type = row["school_type"].lower()
    if "public" in school_type:
        return "yes"
    if school in TUITION_FREE_PRIVATE_HISTORICAL:
        return "yes"
    if school in SENDING_TOWN_ACADEMIES:
        return "yes"
    return "no"


def derive_crit_selective_historical(row: pd.Series) -> str:
    school = row["school"]
    if school in HISTORICALLY_SELECTIVE_PUBLIC:
        return "yes"
    if school in TUITION_FREE_PRIVATE_HISTORICAL:
        return "yes"  # Regis, McDonogh selective by merit at founding
    school_type = row["school_type"].lower()
    if school in SPECIAL_MODEL_SCHOOLS:
        return "no"
    if school in SENDING_TOWN_ACADEMIES:
        return "no"
    if school in OPEN_ACCESS_SCHOOLS:
        return "no"
    if school in LOTTERY_SCHOOLS:
        return "no"
    if school == "Brophy College Preparatory":
        return "yes"  # private Catholic exam-admit, selective from founding
    if "private" in school_type:
        # Elite private boarding/day schools admit by test + interview from
        # early decades. Coded selective=YES; tuition_free=NO excludes them
        # from the strict composite.
        return "yes"
    return "no"


def derive_crit_not_special_model(row: pd.Series) -> str:
    return "no" if row["school"] in SPECIAL_MODEL_SCHOOLS else "yes"


def derive_crit_high_access_strict(row: pd.Series) -> str:
    """Composite high-access: secondary + in-frame + active >=20yr + size
    not-known-small + tuition-free historical + selective historical +
    not special model. Size unknown is treated as pass (do not drop schools
    we lack enrollment data for)."""
    if row["crit_secondary_school"] != "yes":
        return "no"
    if row["crit_in_frame_1800_1940"] != "yes":
        return "no"
    if row["crit_active_20yr"] != "yes":
        return "no"
    if row["crit_first_decade_size_ge_50"] == "no":
        return "no"
    if row["crit_tuition_free_historical"] != "yes":
        return "no"
    if row["crit_selective_historical"] != "yes":
        return "no"
    if row["crit_not_special_model"] != "yes":
        return "no"
    return "yes"


def derive_historically_unclear(row: pd.Series) -> str:
    return "yes" if row["school"] in HISTORICALLY_UNCLEAR_NOTES else "no"


def derive_historically_unclear_note(row: pd.Series) -> str:
    return HISTORICALLY_UNCLEAR_NOTES.get(row["school"], "")


def derive_default_revision_note(row: pd.Series) -> str:
    if row["continuity_status"] == "later_merger_use_roots":
        return "Later merger using predecessor roots; keep out of the core treatment sample unless lineage is explicitly modeled."
    if row["school"] == "University of Chicago Laboratory Schools":
        return "Prestigious but atypical laboratory-school model; use only in robustness checks."
    if row["school"] == "New Mexico Military Institute":
        return "Public military and junior-college model is not directly comparable to ordinary elite high schools."
    if row["continuity_status"] == "reorganized_continuity":
        return "Institutional continuity is real but the current school form is reorganized relative to the historical predecessor."
    return ""


def main() -> None:
    base = pd.read_csv(MANUAL, dtype=str).fillna("")
    if ADDITIONS.exists():
        additions = pd.read_csv(ADDITIONS, dtype=str).fillna("")
        missing_cols = set(base.columns) ^ set(additions.columns)
        if missing_cols:
            raise ValueError(
                "Revision additions must match the manual input columns exactly. "
                f"Mismatched columns: {sorted(missing_cols)}"
            )
        base = pd.concat([base, additions], ignore_index=True)

    duplicate_mask = base.duplicated(subset=["state_abbr", "school"], keep=False)
    if duplicate_mask.any():
        dupes = base.loc[duplicate_mask, ["state", "state_abbr", "school", "city"]]
        raise ValueError(f"Duplicate school rows in combined manual inputs:\n{dupes.to_string(index=False)}")

    counties = pd.read_csv(COUNTIES, sep="|", dtype=str).fillna("")
    panel = pd.read_csv(COUNTY_PANEL, dtype={"GEOID": str})

    county_centroids = (
        panel.sort_values(["GEOID", "decade"])
        .drop_duplicates("GEOID")
        .loc[:, ["GEOID", "lat_county", "lon_county"]]
    )

    counties["county_name_norm"] = norm_text(counties["COUNTYNAME"])
    base["county_name_norm"] = norm_text(base["county_name"])

    merged = base.merge(
        counties.loc[:, ["STATE", "STATEFP", "COUNTYFP", "COUNTYNAME", "county_name_norm"]],
        left_on=["state_abbr", "county_name_norm"],
        right_on=["STATE", "county_name_norm"],
        how="left",
    )

    missing = merged.loc[merged["STATEFP"].eq(""), ["state", "state_abbr", "school", "county_name"]]
    if not missing.empty:
        raise ValueError(f"Unmatched counties:\n{missing.to_string(index=False)}")

    merged["county_geoid"] = merged["STATEFP"] + merged["COUNTYFP"]
    merged = merged.merge(county_centroids, left_on="county_geoid", right_on="GEOID", how="left")

    merged = merged.drop(columns=["STATE", "STATEFP", "COUNTYFP", "COUNTYNAME", "county_name_norm", "GEOID"])

    merged["admission_selectivity_current"] = merged.apply(derive_current_admission_code, axis=1)
    merged["test_based_admissions_current"] = merged["admission_selectivity_current"].map(derive_test_based)
    merged["poor_access_current"] = merged.apply(lambda r: derive_current_poor_access(r, r["admission_selectivity_current"]), axis=1)

    historical = merged.apply(derive_historical_poor_access, axis=1, result_type="expand")
    historical.columns = [
        "poor_access_historical",
        "historical_access_note",
        "historical_access_evidence_level",
    ]
    merged = pd.concat([merged, historical], axis=1)

    merged["current_admissions_source_url"] = ""
    merged["current_access_source_url"] = ""
    merged["historical_access_source_url"] = ""

    for school, fields in CURRENT_SOURCE_OVERRIDES.items():
        mask = merged["school"].eq(school)
        for key, value in fields.items():
            merged.loc[mask, key] = value

    for school, value in HISTORICAL_SOURCE_OVERRIDES.items():
        merged.loc[merged["school"].eq(school), "historical_access_source_url"] = value

    merged["admissions_access_coding_status"] = merged.apply(derive_coding_status, axis=1)
    merged["lineage_risk"] = merged.apply(derive_lineage_risk, axis=1)
    merged["comparability_class"] = merged.apply(derive_comparability_class, axis=1)
    merged["sample_role"] = merged.apply(derive_sample_role, axis=1)
    merged["include_in_core_sample"] = merged["sample_role"].map(
        lambda x: "yes" if x in {"core", "core_with_caution"} else "no"
    )
    merged["include_in_expanded_sample"] = "yes"
    merged["proper_elite_school"] = merged["include_in_core_sample"]
    merged["elite_tier"] = merged["sample_role"].map(
        lambda x: "core" if x in {"core", "core_with_caution"} else "extended"
    )
    merged["local_access_relevance"] = merged.apply(derive_local_access_relevance, axis=1)
    merged["revision_note"] = merged.apply(derive_default_revision_note, axis=1)

    # Inclusion / exclusion criterion dummies. Each is derived independently
    # so any composite can be reconstructed downstream by ANDing dummies.
    enrollment_lookup: dict = {}
    if ENROLLMENT.exists():
        enr = pd.read_csv(ENROLLMENT, sep="\t", dtype=str).fillna("")
        enrollment_lookup = dict(zip(enr["school"], enr["year10_point"]))
    merged["crit_secondary_school"] = merged.apply(derive_crit_secondary_school, axis=1)
    merged["crit_in_frame_1800_1940"] = merged.apply(derive_crit_in_frame, axis=1)
    merged["crit_active_20yr"] = merged.apply(derive_crit_active_20yr, axis=1)
    merged["crit_first_decade_size_ge_50"] = merged.apply(
        lambda r: derive_crit_size_ge_50(r, enrollment_lookup), axis=1
    )
    merged["crit_tuition_free_historical"] = merged.apply(derive_crit_tuition_free_historical, axis=1)
    merged["crit_selective_historical"] = merged.apply(derive_crit_selective_historical, axis=1)
    merged["crit_not_special_model"] = merged.apply(derive_crit_not_special_model, axis=1)
    merged["crit_high_access_strict"] = merged.apply(derive_crit_high_access_strict, axis=1)
    merged["historically_unclear"] = merged.apply(derive_historically_unclear, axis=1)
    merged["historically_unclear_note"] = merged.apply(derive_historically_unclear_note, axis=1)

    if STATE_BATCHES.exists():
        batches = pd.read_csv(STATE_BATCHES, dtype=str).fillna("")
        batch_cols = [
            "state_abbr",
            "batch_id",
            "review_theme",
            "review_priority",
            "review_status",
        ]
        merged = merged.merge(
            batches.loc[:, batch_cols].rename(
                columns={
                    "batch_id": "review_batch",
                    "review_theme": "review_theme",
                    "review_priority": "review_priority",
                    "review_status": "review_status",
                }
            ),
            on="state_abbr",
            how="left",
        )
    else:
        merged["review_batch"] = ""
        merged["review_theme"] = ""
        merged["review_priority"] = ""
        merged["review_status"] = ""

    if REVISION_DECISIONS.exists():
        decisions = pd.read_csv(REVISION_DECISIONS, dtype=str).fillna("")
        merged = merged.merge(
            decisions,
            on=["state_abbr", "school"],
            how="left",
            suffixes=("", "_decision"),
        )

        decision_cols = [
            "review_batch",
            "sample_role",
            "include_in_core_sample",
            "comparability_class",
            "lineage_risk",
            "local_access_relevance",
            "revision_note",
        ]
        for col in decision_cols:
            override = f"{col}_decision"
            if override in merged.columns:
                merged[override] = merged[override].fillna("")
                merged[col] = merged[override].where(merged[override].ne(""), merged[col])
                merged = merged.drop(columns=[override])

    merged["review_batch"] = merged["review_batch"].fillna("").replace("", "unassigned")
    merged["review_theme"] = merged["review_theme"].fillna("").replace("", "No targeted revision batch yet")
    merged["review_priority"] = merged["review_priority"].fillna("").replace("", "low")
    merged["review_status"] = merged["review_status"].fillna("").replace("", "first_pass_only")

    merged = merged.sort_values(["state", "founding_year_used", "school"]).reset_index(drop=True)

    if merged["lat_county"].isna().any() or merged["lon_county"].isna().any():
        bad = merged.loc[merged["lat_county"].isna() | merged["lon_county"].isna(), ["state", "school", "county_geoid"]]
        raise ValueError(f"Missing county centroids:\n{bad.to_string(index=False)}")

    merged.to_csv(OUT_SCHOOLS, index=False)

    core = merged.loc[merged["include_in_core_sample"].eq("yes")].copy()
    expanded = merged.loc[merged["include_in_expanded_sample"].eq("yes")].copy()
    robustness = merged.loc[merged["sample_role"].eq("robustness_only")].copy()
    core.to_csv(OUT_CORE, index=False)
    expanded.to_csv(OUT_EXPANDED, index=False)
    robustness.to_csv(OUT_ROBUSTNESS, index=False)

    if REVISION_QUEUE.exists():
        boundaries = pd.read_csv(REVISION_QUEUE, dtype=str).fillna("")
        boundaries = boundaries.loc[
            boundaries["merge_status"].isin({"do_not_merge", "needs_source_review"})
            | boundaries["frame_status"].isin({"outside_1800_1930_main_frame", "nonstate_jurisdiction"})
        ].copy()
        boundaries.to_csv(OUT_BOUNDARIES, index=False)

    states = pd.DataFrame(STATE_ORDER, columns=["state", "state_abbr"])
    counts = merged.groupby(["state", "state_abbr"]).size().reset_index(name="identified_school_count")
    core_counts = core.groupby(["state", "state_abbr"]).size().reset_index(name="core_school_count")
    robustness_counts = robustness.groupby(["state", "state_abbr"]).size().reset_index(name="robustness_only_count")
    state_summary = (
        states.merge(counts, on=["state", "state_abbr"], how="left")
        .merge(core_counts, on=["state", "state_abbr"], how="left")
        .merge(robustness_counts, on=["state", "state_abbr"], how="left")
        .fillna({"identified_school_count": 0, "core_school_count": 0, "robustness_only_count": 0})
    )
    state_summary["identified_school_count"] = state_summary["identified_school_count"].astype(int)
    state_summary["core_school_count"] = state_summary["core_school_count"].astype(int)
    state_summary["robustness_only_count"] = state_summary["robustness_only_count"].astype(int)
    state_summary["coverage_status"] = state_summary["identified_school_count"].map(
        lambda n: "identified_1plus" if n > 0 else "no_high_confidence_candidate_in_this_pass"
    )
    state_summary["notes"] = state_summary.apply(
        lambda r: (
            f"{r['core_school_count']} core and {r['robustness_only_count']} robustness-only schools identified"
            if r["identified_school_count"] > 0
            else "No high-confidence school identified in this national first pass"
        ),
        axis=1,
    )
    state_summary.to_csv(OUT_STATES, index=False)

    print(f"Wrote {len(merged)} school rows to {OUT_SCHOOLS.name}")
    print(f"Wrote {len(core)} core rows to {OUT_CORE.name}")
    print(f"Wrote {len(expanded)} expanded rows to {OUT_EXPANDED.name}")
    print(f"Wrote {len(robustness)} robustness-only rows to {OUT_ROBUSTNESS.name}")
    print(f"Wrote {len(state_summary)} state rows to {OUT_STATES.name}")
    print(merged.groupby('state').size().sort_values(ascending=False).head(10).to_string())


if __name__ == "__main__":
    main()
