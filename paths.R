# paths.R — shared data root for gtl-talent-dets prep and analysis scripts
#
# Source from prep/ scripts:      source("../paths.R")
# Source from analysis/ scripts:  source("../paths.R")
#
# Override with env var TALENT_DETS_DATA_DIR if running on a different machine.

TALENT_DETS_DATA_DIR <- Sys.getenv("TALENT_DETS_DATA_DIR",
                                    unset = "C:/Users/deivi/Globtalent Dropbox/gtl_talent_dets")

DATA_INPUT     <- file.path(TALENT_DETS_DATA_DIR, "input")
DATA_OUTPUT    <- file.path(TALENT_DETS_DATA_DIR, "output")
AMWS_INPUT     <- file.path(DATA_INPUT,  "amws")
SCHOOLS_INPUT  <- file.path(DATA_INPUT,  "elite_schools")
AMWS_OUTPUT    <- file.path(DATA_OUTPUT, "amws")
SCHOOLS_OUTPUT <- file.path(DATA_OUTPUT, "elite_schools")

# Reviewed edition-16 supplement: immutable input, staged release, publications.
AMWS_OCR_REVIEWED_INPUT <- file.path(AMWS_OUTPUT, "amws_ed16_paragraph_reviewed_data")
AMWS_OCR_SUPPLEMENT_STAGE <- file.path(AMWS_OUTPUT, "ocr_supplement_20261011")
AMWS_1986_FULL_DIR <- file.path(TALENT_DETS_DATA_DIR, "Data", "intermediary", "amws")
AMWS_1986_PROCESSED_DIR <- file.path(TALENT_DETS_DATA_DIR, "Data", "processed", "amws")
AMWS_ALL_EDITIONS_PERSONS <- file.path(AMWS_OUTPUT, "amws_all_editions_persons.csv")

# Combined AMWS education IDs and university reference coordinates for analysis.
AMWS_OPEN_ALEX <- file.path(TALENT_DETS_DATA_DIR, "Data", "processed", "amws", "amws_open_alex.csv")
