###############################################################################
# State / territory token normalizer for AMWS editions.
# normalize_state(x) -> 2-letter USPS code or NA if not a recognized US state.
###############################################################################

USPS <- c("AL","AK","AZ","AR","CA","CO","CT","DE","DC","FL","GA","HI","ID",
          "IL","IN","IA","KS","KY","LA","ME","MD","MA","MI","MN","MS","MO",
          "MT","NE","NV","NH","NJ","NM","NY","NC","ND","OH","OK","OR","PA",
          "RI","SC","SD","TN","TX","UT","VT","VA","WA","WV","WI","WY")

STATE_ALIAS <- c(
  "ala"="AL","ala."="AL","alabama"="AL",
  "alaska"="AK","alas"="AK",
  "ariz"="AZ","ariz."="AZ","arizona"="AZ",
  "ark"="AR","ark."="AR","arkansas"="AR",
  "cal"="CA","cal."="CA","calif"="CA","calif."="CA","california"="CA",
  "colo"="CO","colo."="CO","colorado"="CO","col"="CO",
  "conn"="CT","conn."="CT","connecticut"="CT",
  "del"="DE","del."="DE","delaware"="DE",
  "d. c"="DC","d.c"="DC","d. c."="DC","d.c."="DC","dc"="DC","district of columbia"="DC","washington d. c"="DC",
  "fla"="FL","fla."="FL","florida"="FL",
  "ga"="GA","ga."="GA","georgia"="GA",
  "hawaii"="HI","hawaiian islands"="HI",
  "idaho"="ID","ida"="ID","ida."="ID",
  "ill"="IL","ill."="IL","illinois"="IL","111"="IL",   # 111 = OCR for Ill
  "ind"="IN","ind."="IN","indiana"="IN",
  "ia"="IA","ia."="IA","iowa"="IA",
  "kans"="KS","kans."="KS","kan"="KS","kan."="KS","kansas"="KS",
  "ky"="KY","ky."="KY","kentucky"="KY",
  "la"="LA","la."="LA","louisiana"="LA",
  "me"="ME","me."="ME","maine"="ME",
  "md"="MD","md."="MD","maryland"="MD",
  "mass"="MA","mass."="MA","massachusetts"="MA",
  "mich"="MI","mich."="MI","michigan"="MI",
  "minn"="MN","minn."="MN","minnesota"="MN",
  "miss"="MS","miss."="MS","mississippi"="MS",
  "mo"="MO","mo."="MO","missouri"="MO",
  "mont"="MT","mont."="MT","montana"="MT",
  "nebr"="NE","nebr."="NE","neb"="NE","neb."="NE","nebraska"="NE",
  "nev"="NV","nev."="NV","nevada"="NV",
  "n. h"="NH","n.h"="NH","n. h."="NH","n.h."="NH","nh"="NH","new hampshire"="NH",
  "n. j"="NJ","n.j"="NJ","n. j."="NJ","n.j."="NJ","nj"="NJ","new jersey"="NJ",
  "n. mex"="NM","n. m"="NM","n.m"="NM","n. m."="NM","n.m."="NM","nm"="NM","new mexico"="NM",
  "nmex"="NM","n mex"="NM","n. mex."="NM",
  "n. y"="NY","n.y"="NY","n. y."="NY","n.y."="NY","ny"="NY","new york"="NY",
  "n. c"="NC","n.c"="NC","n. c."="NC","n.c."="NC","nc"="NC","north carolina"="NC",
  "n. d"="ND","n.d"="ND","n. d."="ND","n.d."="ND","nd"="ND","n. dak"="ND","n. dak."="ND",
  "n.dak"="ND","ndak"="ND","n dak"="ND","north dakota"="ND",
  "ohio"="OH","o"="OH","o."="OH",
  "okla"="OK","okla."="OK","oklahoma"="OK","okl"="OK","ok"="OK",
  "ore"="OR","ore."="OR","oreg"="OR","oreg."="OR","oregon"="OR",
  "pa"="PA","pa."="PA","penn"="PA","penn."="PA","penna"="PA","pennsylvania"="PA",
  "r. i"="RI","r.i"="RI","r. i."="RI","r.i."="RI","ri"="RI","rl"="RI","rhode island"="RI",
  "s. c"="SC","s.c"="SC","s. c."="SC","s.c."="SC","sc"="SC","south carolina"="SC",
  "s. d"="SD","s.d"="SD","s. d."="SD","s.d."="SD","sd"="SD","s. dak"="SD","s. dak."="SD",
  "s.dak"="SD","sdak"="SD","s dak"="SD","south dakota"="SD",
  "tenn"="TN","tenn."="TN","tennessee"="TN",
  "tex"="TX","tex."="TX","texas"="TX",
  "utah"="UT","ut"="UT",
  "vt"="VT","vt."="VT","vermont"="VT",
  "va"="VA","va."="VA","virginia"="VA",
  "wash"="WA","wash."="WA","washington"="WA",
  "w. va"="WV","w.va"="WV","w. va."="WV","w.va."="WV","west virginia"="WV","wva"="WV",
  "wis"="WI","wis."="WI","wisc"="WI","wisc."="WI","wisconsin"="WI","wls"="WI",  # Wls = OCR for Wis
  "wyo"="WY","wyo."="WY","wyoming"="WY",
  # Territories that became states later. AMWS edition 1 (1906) entries.
  "indian ter"="OK","ind. ter"="OK","i. t."="OK","i.t."="OK","indian territory"="OK",
  "alaska ter"="AK","alaska territory"="AK",
  "hawaii ter"="HI","hawaii territory"="HI","ter. of hawaii"="HI",
  # Common OCR misreads (observed in 1938 cleaning residuals)
  "iii"="IL","iu"="IL","ii1"="IL","ill,"="IL",
  "ncbr"="NE","ncb"="NE","ncv"="NV",
  "wia"="WI","wls"="WI",
  "maw"="MA","mans"="MA",
  "ya"="VA","va,"="VA","y a"="VA",
  "n. v"="NY","n.v"="NY",
  "p. r"="PR","p.r"="PR","puerto rico"="PR",
  "ind. ter."="OK",
  # Self-mappings for already-canonical 2-letter codes:
  setNames(USPS, USPS)
)
# lowercase keys for case-insensitive lookup
names(STATE_ALIAS) <- tolower(names(STATE_ALIAS))

normalize_state <- function(x) {
  if (is.null(x)) return(NA_character_)
  k <- tolower(trimws(gsub("\\s+", " ", x)))
  k <- gsub("[,;:]+$", "", k)
  out <- unname(STATE_ALIAS[k])
  out
}

normalize_state_vec <- function(x) {
  vapply(x, normalize_state, character(1), USE.NAMES = FALSE)
}
