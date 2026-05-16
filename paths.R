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
