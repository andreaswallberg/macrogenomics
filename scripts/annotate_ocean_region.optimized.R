#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(tidyverse)
  library(sf)
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(tidygeocoder)
  library(countrycode)
})

# ============================================================
# Ocean / hemisphere classifier for sample metadata TSV
#
# Fast version:
#   - Original input columns are preserved first, in original order.
#   - New columns are appended at the far right.
#   - Marine text rules are applied to unique text keys once, then joined back.
#   - US state rules are applied only to unique United States candidate keys.
#   - Per-sample logging is disabled by default for speed.
# ============================================================

args <- commandArgs(trailingOnly = TRUE)
input_tsv <- ifelse(length(args) >= 1, args[[1]], "samples.tsv")
output_tsv <- ifelse(length(args) >= 2, args[[2]], "samples_ocean_classified.tsv")

message("Input TSV:  ", input_tsv)
message("Output TSV: ", output_tsv)

sf::sf_use_s2(FALSE)

nearest_ocean_threshold_km <- 250
geocode_cache_file <- "geocode_cache.tsv"

# Set FALSE for a dry run that only writes queries_pending_nominatim.tsv
run_nominatim <- TRUE
nominatim_batch_size <- 200
nominatim_pause_seconds <- 10
max_nominatim_queries_per_run <- 10000

# Disabled by default because hundreds of thousands of log lines are slow.
print_sample_classifications <- FALSE
write_classification_log <- FALSE
classification_log_file <- "sample_classification_log.txt"

# ------------------------------------------------------------
# Timing helpers
# ------------------------------------------------------------

time_start <- function(label) {
  message("")
  message(">>> ", label)
  Sys.time()
}

time_end <- function(t0, label) {
  message(
    "<<< Finished ", label, " in ",
    round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 2),
    " minutes."
  )
}

# ------------------------------------------------------------
# Text helpers
# ------------------------------------------------------------

clean_text <- function(x) {
  x |>
    as.character() |>
    stringr::str_squish() |>
    dplyr::na_if("") |>
    dplyr::na_if("NA") |>
    dplyr::na_if("N/A") |>
    dplyr::na_if("na") |>
    dplyr::na_if("n/a") |>
    dplyr::na_if("not collected") |>
    dplyr::na_if("Not recorded - missing") |>
    dplyr::na_if("Not recorded - Not recorded") |>
    dplyr::na_if("missing") |>
    dplyr::na_if("unknown") |>
    dplyr::na_if("Unknown")
}

normalize_location_text <- function(x) {
  x |>
    clean_text() |>
    stringr::str_replace_all(";", ":") |>
    stringr::str_replace_all("Â||‘|’", "'") |>
    stringr::str_replace_all("ĂÂ|AdĂÂlie", "Adelie") |>
    stringr::str_replace_all(regex("Moroco", ignore_case = TRUE), "Morocco") |>
    stringr::str_replace_all(regex("Queenzland", ignore_case = TRUE), "Queensland") |>
    stringr::str_replace_all(regex("Clack reef", ignore_case = TRUE), "Clack Reef") |>
    stringr::str_replace_all(regex("South Chian Sea", ignore_case = TRUE), "South China Sea") |>
    stringr::str_replace_all(regex("Hannan Province", ignore_case = TRUE), "Hainan Province") |>
    stringr::str_replace_all(regex("Shangdong|Shandong Provience", ignore_case = TRUE), "Shandong Province") |>
    stringr::str_replace_all(regex("Zhejing", ignore_case = TRUE), "Zhejiang") |>
    stringr::str_replace_all(regex("Sizuoka", ignore_case = TRUE), "Shizuoka") |>
    stringr::str_replace_all(regex("Sugura Bay", ignore_case = TRUE), "Suruga Bay") |>
    stringr::str_replace_all(regex("Gulf of Carpenteria", ignore_case = TRUE), "Gulf of Carpentaria") |>
    stringr::str_replace_all(regex("Saint Lawrence", ignore_case = TRUE), "St. Lawrence") |>
    stringr::str_replace_all(regex("Hawai'i", ignore_case = TRUE), "Hawaii") |>
    stringr::str_replace_all(regex("O'ahu", ignore_case = TRUE), "Oahu") |>
    stringr::str_replace_all(regex("Kane'ohe", ignore_case = TRUE), "Kaneohe") |>
    stringr::str_replace_all(regex("Baie James", ignore_case = TRUE), "James Bay") |>
    stringr::str_replace_all(regex("Canaries", ignore_case = TRUE), "Canary Islands") |>
    stringr::str_replace_all(regex("\\bresund\\b", ignore_case = TRUE), "Oresund") |>
    stringr::str_replace_all(regex("Coat north of", ignore_case = TRUE), "Coast north of") |>
    stringr::str_replace_all(regex("Pointe Gologie", ignore_case = TRUE), "Pointe Geologie") |>
    stringr::str_replace_all(regex("Ponza islan\\b", ignore_case = TRUE), "Ponza island") |>
    stringr::str_replace_all(regex("Taha$", ignore_case = TRUE), "Taha'a") |>
    stringr::str_squish()
}

normalize_country_synonyms <- function(x) {
  x <- normalize_location_text(x)
  has_prefix <- !is.na(x) & stringr::str_detect(x, ":")
  prefix <- if_else(has_prefix, str_trim(str_extract(x, "^[^:]+")), x)
  rest <- if_else(has_prefix, str_trim(str_replace(x, "^[^:]+:", "")), NA_character_)
  prefix_low <- str_to_lower(prefix)

  prefix_norm <- case_when(
    str_detect(prefix_low, "^(us|u\\.s\\.|usa|u\\.s\\.a\\.|united states|united states of america|america)$") ~ "United States",
    str_detect(prefix_low, "^(uk|u\\.k\\.|united kingdom|great britain|britain|england|scotland|wales)$") ~ "United Kingdom",
    str_detect(prefix_low, "^(russia|russian federation)$") ~ "Russia",
    str_detect(prefix_low, "^(south korea|republic of korea|korea republic|korea, republic of)$") ~ "South Korea",
    str_detect(prefix_low, "^(north korea|democratic people's republic of korea|dprk|korea, democratic people's republic of)$") ~ "North Korea",
    str_detect(prefix_low, "^korea$") ~ "South Korea",
    str_detect(prefix_low, "^(china|people's republic of china|prc|p\\.r\\. china|p r china)$") ~ "China",
    str_detect(prefix_low, "^(hong kong|hong kong sar|hong kong, china)$") ~ "Hong Kong",
    str_detect(prefix_low, "^(taiwan|taiwan, china|chinese taipei)$") ~ "Taiwan",
    str_detect(prefix_low, "^(iran|iran, islamic republic of|islamic republic of iran)$") ~ "Iran",
    str_detect(prefix_low, "^(vietnam|viet nam)$") ~ "Vietnam",
    str_detect(prefix_low, "^(cape verde|cabo verde)$") ~ "Cape Verde",
    str_detect(prefix_low, "^(curacao|curaçao)$") ~ "Curacao",
    str_detect(prefix_low, "^(reunion|réunion)$") ~ "Reunion",
    TRUE ~ prefix
  )

  if_else(has_prefix, paste(prefix_norm, rest, sep = ": "), prefix_norm)
}

normalize_country_with_countrycode <- function(x) {
  x <- normalize_country_synonyms(x)
  has_prefix <- !is.na(x) & str_detect(x, ":")
  prefix <- if_else(has_prefix, str_trim(str_extract(x, "^[^:]+")), x)
  rest <- if_else(has_prefix, str_trim(str_replace(x, "^[^:]+:", "")), NA_character_)

  cc_name <- suppressWarnings(countrycode::countrycode(prefix, "country.name", "country.name", warn = FALSE))
  cc_iso2 <- suppressWarnings(countrycode::countrycode(prefix, "iso2c", "country.name", warn = FALSE))
  cc_iso3 <- suppressWarnings(countrycode::countrycode(prefix, "iso3c", "country.name", warn = FALSE))
  prefix_norm <- coalesce(cc_name, cc_iso2, cc_iso3, prefix)

  if_else(has_prefix, paste(prefix_norm, rest, sep = ": "), prefix_norm)
}

parse_number <- function(x) suppressWarnings(as.numeric(str_replace_all(as.character(x), ",", ".")))

hemisphere_from_lat <- function(lat) {
  case_when(is.na(lat) ~ NA_character_, lat > 0 ~ "North", lat < 0 ~ "South", TRUE ~ "Equator")
}

clean_geocode_query <- function(x) {
  x |>
    clean_text() |>
    str_replace_all("\\s+", " ") |>
    str_replace("^\\s*,\\s*", "") |>
    str_replace_all("\\s+,", ",") |>
    str_replace_all(",\\s*,+", ",") |>
    str_replace_all("\\s*,\\s*", ", ") |>
    str_squish() |>
    na_if("")
}

strip_station_prefix <- function(x) {
  x |>
    clean_text() |>
    str_replace("^\\([^\\)]*\\)\\s*", "") |>
    str_replace("^[A-Za-z0-9]+[IVXivx]*,\\s*", "") |>
    clean_geocode_query()
}

make_geocode_query <- function(country_field, location_field = NA_character_) {
  country_field <- normalize_country_with_countrycode(country_field)
  location_field <- normalize_location_text(location_field)

  query <- case_when(
    !is.na(country_field) & str_detect(country_field, ":") ~ {
      prefix <- str_trim(str_extract(country_field, "^[^:]+"))
      rest <- str_trim(str_replace(country_field, "^[^:]+:", ""))
      paste(rest, prefix, sep = ", ")
    },
    !is.na(country_field) & !is.na(location_field) ~ paste(location_field, country_field, sep = ", "),
    !is.na(country_field) ~ country_field,
    !is.na(location_field) ~ location_field,
    TRUE ~ NA_character_
  )

  clean_geocode_query(query)
}

looks_country_only <- function(x) {
  x <- normalize_country_with_countrycode(x)
  has_prefix <- !is.na(x) & str_detect(x, ":")
  case_when(
    is.na(x) ~ FALSE,
    has_prefix ~ FALSE,
    !str_detect(x, ",") & str_count(x, "\\S+") <= 5 ~ TRUE,
    TRUE ~ FALSE
  )
}

# ------------------------------------------------------------
# Ocean text rules
# ------------------------------------------------------------

simplify_ocean_name <- function(x) {
  x_low <- str_to_lower(coalesce(x, ""))
  case_when(
    str_detect(x_low, "mediterranean") ~ "Mediterranean",
    str_detect(x_low, "atlantic|north sea|baltic|norwegian sea|barents|greenland sea|bay of biscay|gulf of mexico|caribbean|labrador sea|irish sea|english channel|white sea") ~ "Atlantic",
    str_detect(x_low, "pacific|philippine sea|south china sea|east china sea|sea of japan|bering|coral sea|tasman|yellow sea|sulu sea|celebes sea|okhotsk|bohai") ~ "Pacific",
    str_detect(x_low, "indian|arabian sea|bay of bengal|andaman|laccadive|mozambique channel|red sea|persian gulf|gulf of aden") ~ "Indian",
    str_detect(x_low, "southern|antarctic|weddell|ross sea|scotia sea|amundsen|bellingshausen") ~ "Southern",
    str_detect(x_low, "arctic|beaufort|chukchi|laptev|kara sea|east siberian") ~ "Arctic",
    TRUE ~ NA_character_
  )
}

classify_from_prefix <- function(x) {
  x_low <- str_to_lower(coalesce(normalize_location_text(x), ""))
  case_when(
    str_detect(x_low, "^pacific ocean\\b") ~ "Pacific",
    str_detect(x_low, "^atlantic ocean\\b") ~ "Atlantic",
    str_detect(x_low, "^indian ocean\\b") ~ "Indian",
    str_detect(x_low, "^mediterranean sea\\b") ~ "Mediterranean",
    str_detect(x_low, "^southern ocean\\b") ~ "Southern",
    str_detect(x_low, "^arctic ocean\\b") ~ "Arctic",
    TRUE ~ NA_character_
  )
}

classify_ocean_from_marine_text <- function(country, location = NA_character_, description = NA_character_, sample_description = NA_character_) {
  text <- normalize_location_text(paste(country, location, description, sample_description, sep = " "))
  text_low <- str_to_lower(coalesce(text, ""))
  prefix_ocean <- classify_from_prefix(text)

  case_when(
    !is.na(prefix_ocean) ~ prefix_ocean,
    str_detect(text_low, "caspian sea|dead sea") ~ "inland_sea",
    str_detect(text_low, "southern ocean|antarctica|antarctic|sub-antarctic|weddell|ross sea|scotia sea|bransfield|mcmurdo|south shetland|south sandwich|king george island|adelaide island|prydz bay|terra nova bay|rothera|signy island|deception island|elephant island|palmer station|mertz|george v") ~ "Southern",
    str_detect(text_low, "arctic ocean|chukchi|beaufort sea|baffin bay|cumberland sound|pangnirtung|laptev|kara sea|east siberian|northwest passage|eastern arctic|western arctic|salluit fjord") ~ "Arctic",
    str_detect(text_low, "mediterranean|black sea|adriatic|aegean|ionian|tyrrhenian|tyrrenian|ligurian|gulf of aqaba|gulf of eilat") ~ "Mediterranean",
    str_detect(text_low, "atlantic ocean|north atlantic|south atlantic|mid-atlantic ridge|bay of biscay|cantabrian sea|english channel|celtic sea|irish sea|north sea|baltic|bothnian bay|kattegat|skagerrak|norwegian sea|barents|white sea|greenland sea|labrador sea|gulf of mexico|florida keys|florida straits|caribbean|puerto rico|curacao|bermuda|belize|turneffe|glovers atoll|lighthouse atoll|cape verde|azores|canary islands|fuerteventura|lanzarote|bay of fundy|gulf of st\\. lawrence|gulf of saint lawrence|st\\. lawrence estuary|scotian shelf|banquereau bank|newfoundland|nova scotia|grand banks|hudson bay|james bay|baie james|nelson river|nastapoka|saint john river|chesapeake|long island sound|georges bank|woods hole|cape cod|gulf of maine") ~ "Atlantic",
    str_detect(text_low, "pacific ocean|north pacific|south pacific|western pacific|northwest pacific|north-west pacific|northwestern pacific|pacific northwest|east pacific rise|juan de fuca|clarion|clipperton|tonga trench|mariana|daikoku|kermadec|atacama trench|bering sea|sea of okhotsk|sea of japan|east china sea|yellow sea|bohai sea|south china sea|spratly|xisha|zhongsha|nansha|gulf of thailand|philippine sea|gulf of california|baja california|galapagos|hawaii|oahu|kaneohe|midway|pihemanu|guam|palau|fiji|new caledonia|papua new guinea|manus basin|vancouver island|haida gwaii|british columbia|straight of georgia|strait of georgia|puget sound|salish sea|oregon|california coast|great barrier reef|coral sea|queensland|heron island|orpheus|magnetic island|townsville|sagami bay|tokyo bay|suruga bay|mikawa bay|ise-mikawa|uwa sea|amakusa|yatsushiro bay|yamato ridge|okinawa|ryukyu|amami|tanegashima|ogasawara|bonin|chichijima|hatsushima|izu-ogasawara|kagoshima|kanagawa|shizuoka|miyagi|hokkaido|akkeshi|bekanbeushi|shiomi river|kuroshio") ~ "Pacific",
    str_detect(text_low, "indian ocean|central indian ocean|arabian sea|bay of bengal|andaman sea|laccadive|mozambique channel|red sea|persian gulf|gulf of aden|strait of hormuz|maldives|mauritius|kerguelen|crozet|diego garcia|cosmoledo|salomon atoll|amsterdam island|marion island|prince edward islands|broome|shark bay|western australia|christmas island|java trench") ~ "Indian",
    TRUE ~ NA_character_
  )
}

hemisphere_from_marine_text <- function(country, location = NA_character_, description = NA_character_, sample_description = NA_character_) {
  text <- normalize_location_text(paste(country, location, description, sample_description, sep = " "))
  text_low <- str_to_lower(coalesce(text, ""))
  case_when(
    str_detect(text_low, "antarctica|antarctic|southern ocean|south africa|south atlantic|south pacific|australia|new zealand|chile|argentina|french polynesia|fiji|new caledonia|kerguelen|crozet|marion island|prince edward|south georgia") ~ "South",
    str_detect(text_low, "canada|usa|u\\.s\\.a\\.|united states|mexico|belize|panama|puerto rico|curacao|bermuda|japan|china|korea|taiwan|philippines|thailand|vietnam|viet nam|malaysia|india|bangladesh|pakistan|iran|israel|qatar|bahrain|oman|kazakhstan|spain|france|italy|greece|croatia|portugal|norway|sweden|finland|denmark|germany|netherlands|united kingdom|iceland|greenland|russia|arctic|north atlantic|north pacific|northern mariana|guam|palau|hawaii|midway|azores|canary islands|cape verde") ~ "North",
    TRUE ~ NA_character_
  )
}

# ------------------------------------------------------------
# US state rules
# ------------------------------------------------------------

classify_us_state_from_text <- function(country, location = NA_character_, description = NA_character_, sample_description = NA_character_) {
  country_norm <- normalize_country_with_countrycode(country)
  country_prefix <- if_else(!is.na(country_norm) & str_detect(country_norm, ":"), str_trim(str_extract(country_norm, "^[^:]+")), country_norm)

  if (is.na(country_prefix) || country_prefix != "United States") {
    return(tibble(us_state_broad_ocean = NA_character_, us_state_hemisphere = NA_character_, us_state_match = NA_character_, us_state_confidence = NA_character_, us_state_note = NA_character_))
  }

  text <- normalize_location_text(paste(country, location, description, sample_description, sep = " "))
  text_low <- str_to_lower(coalesce(text, ""))

  state_rules <- tribble(
    ~state, ~state_name, ~broad_ocean, ~note,
    "AK", "alaska", "Pacific", "US state rule; Alaska is Pacific/Arctic, locality may matter.",
    "CA", "california", "Pacific", "US state rule; Pacific coast.",
    "OR", "oregon", "Pacific", "US state rule; Pacific coast.",
    "WA", "washington", "Pacific", "US state rule; Pacific coast.",
    "HI", "hawaii", "Pacific", "US state rule; Pacific island state.",
    "ME", "maine", "Atlantic", "US state rule; Atlantic coast.",
    "NH", "new hampshire", "Atlantic", "US state rule; Atlantic coast.",
    "MA", "massachusetts", "Atlantic", "US state rule; Atlantic coast.",
    "RI", "rhode island", "Atlantic", "US state rule; Atlantic coast.",
    "CT", "connecticut", "Atlantic", "US state rule; Atlantic coast.",
    "NY", "new york", "Atlantic", "US state rule; Atlantic coast/Great Lakes possible.",
    "NJ", "new jersey", "Atlantic", "US state rule; Atlantic coast.",
    "DE", "delaware", "Atlantic", "US state rule; Atlantic coast.",
    "MD", "maryland", "Atlantic", "US state rule; Atlantic/Chesapeake.",
    "VA", "virginia", "Atlantic", "US state rule; Atlantic/Chesapeake.",
    "NC", "north carolina", "Atlantic", "US state rule; Atlantic coast.",
    "SC", "south carolina", "Atlantic", "US state rule; Atlantic coast.",
    "GA", "georgia", "Atlantic", "US state rule; Atlantic coast.",
    "FL", "florida", "Atlantic", "US state rule; Atlantic/Gulf/Florida Keys; locality may matter.",
    "AL", "alabama", "Atlantic", "US state rule; Gulf of Mexico, grouped as Atlantic.",
    "MS", "mississippi", "Atlantic", "US state rule; Gulf of Mexico, grouped as Atlantic.",
    "LA", "louisiana", "Atlantic", "US state rule; Gulf of Mexico, grouped as Atlantic.",
    "TX", "texas", "Atlantic", "US state rule; Gulf of Mexico, grouped as Atlantic.",
    "MI", "michigan", "Atlantic", "US state rule; Great Lakes/St. Lawrence drainage, not marine.",
    "OH", "ohio", "Atlantic", "US state rule; Great Lakes/Ohio drainage; not marine.",
    "PA", "pennsylvania", "Atlantic", "US state rule; Atlantic/Great Lakes drainage; not marine.",
    "VT", "vermont", "Atlantic", "US state rule; St. Lawrence/Hudson drainage; not marine.",
    "PR", "puerto rico", "Atlantic", "US territory rule; Caribbean/Atlantic.",
    "VI", "virgin islands", "Atlantic", "US territory rule; Caribbean/Atlantic.",
    "GU", "guam", "Pacific", "US territory rule; Pacific.",
    "MP", "northern mariana islands", "Pacific", "US territory rule; Pacific.",
    "AS", "american samoa", "Pacific", "US territory rule; Pacific."
  )

  matches <- state_rules |>
    rowwise() |>
    mutate(
      abbr_hit = str_detect(text, regex(paste0("(^|[\\s,:;])", state, "([\\s,:;.-]|$)"), ignore_case = FALSE)),
      name_hit = str_detect(text_low, paste0("\\b", state_name, "\\b"))
    ) |>
    ungroup() |>
    filter(abbr_hit | name_hit)

  if (nrow(matches) == 0) {
    return(tibble(us_state_broad_ocean = NA_character_, us_state_hemisphere = NA_character_, us_state_match = NA_character_, us_state_confidence = NA_character_, us_state_note = NA_character_))
  }

  best <- matches |> slice(1)
  confidence <- ifelse(best$state %in% c("MI", "OH", "PA", "VT", "AK", "FL"), "low", "medium")

  tibble(
    us_state_broad_ocean = best$broad_ocean,
    us_state_hemisphere = "North",
    us_state_match = paste0(best$state, " / ", best$state_name),
    us_state_confidence = confidence,
    us_state_note = best$note
  )
}

# ------------------------------------------------------------
# Coordinate parsing from text
# ------------------------------------------------------------

parse_compact_dms_coord <- function(coord, is_lat = TRUE) {
  coord <- str_trim(coord)
  dir <- str_extract(coord, "[NSEWnsew]$")
  num <- str_remove(coord, "[NSEWnsew]$")
  if (is.na(dir) || is.na(num)) return(NA_real_)

  dir <- toupper(dir)
  num <- str_replace_all(num, "\\s+", "")
  deg_digits <- if (is_lat) 2 else 3

  if (str_detect(num, "^\\d+\\.\\d+$")) {
    if (nchar(str_remove(num, "\\..*$")) <= deg_digits) return(NA_real_)
    deg <- as.numeric(substr(num, 1, deg_digits))
    min <- as.numeric(substr(num, deg_digits + 1, nchar(num)))
    val <- deg + min / 60
  } else {
    if (nchar(num) < deg_digits + 2) return(NA_real_)
    deg <- as.numeric(substr(num, 1, deg_digits))
    min <- as.numeric(substr(num, deg_digits + 1, deg_digits + 2))
    sec <- if (nchar(num) > deg_digits + 2) as.numeric(substr(num, deg_digits + 3, nchar(num))) else 0
    val <- deg + min / 60 + sec / 3600
  }

  if (dir %in% c("S", "W")) val <- -val
  val
}

parse_coords_from_text <- function(x) {
  x <- normalize_location_text(x)
  out <- tibble(text_lat = rep(NA_real_, length(x)), text_lon = rep(NA_real_, length(x)))
  if (length(x) == 0) return(out)

  pattern1 <- paste0("([0-9]+(?:\\.[0-9]+)?)\\s*([NSns])", "\\s+", "([0-9]+(?:\\.[0-9]+)?)\\s*([EWew])")
  m1 <- str_match(x, pattern1)
  has_match1 <- !is.na(m1[, 1])

  if (any(has_match1)) {
    lat <- as.numeric(m1[has_match1, 2])
    lat_dir <- toupper(m1[has_match1, 3])
    lon <- as.numeric(m1[has_match1, 4])
    lon_dir <- toupper(m1[has_match1, 5])
    lat <- ifelse(lat_dir == "S", -lat, lat)
    lon <- ifelse(lon_dir == "W", -lon, lon)
    valid <- abs(lat) <= 90 & abs(lon) <= 180
    idx <- which(has_match1)
    out$text_lat[idx[valid]] <- lat[valid]
    out$text_lon[idx[valid]] <- lon[valid]
  }

  pattern2 <- paste0("(\\d{4,7}(?:\\.\\d+)?\\s*[NSns])", "[,\\s]+", "(\\d{5,8}(?:\\.\\d+)?\\s*[EWew])")
  m2 <- str_match(x, pattern2)
  has_match2 <- !is.na(m2[, 1]) & is.na(out$text_lat)

  if (any(has_match2)) {
    lat2 <- purrr::map_dbl(m2[has_match2, 2], parse_compact_dms_coord, is_lat = TRUE)
    lon2 <- purrr::map_dbl(m2[has_match2, 3], parse_compact_dms_coord, is_lat = FALSE)
    valid2 <- abs(lat2) <= 90 & abs(lon2) <= 180
    idx2 <- which(has_match2)
    out$text_lat[idx2[valid2]] <- lat2[valid2]
    out$text_lon[idx2[valid2]] <- lon2[valid2]
  }

  out
}

empty_geocode_hits <- function() {
  tibble(
    row_id = integer(), fallback_broad_ocean = character(), fallback_hemisphere = character(),
    fallback_method = character(), fallback_confidence = character(), fallback_manual_check_required = logical(),
    fallback_notes = character(), fallback_lat = numeric(), fallback_lon = numeric(), fallback_resolved_name = character()
  )
}

safe_geocode_one <- function(query) {
  message("Geocoding: ", query)
  result <- tryCatch(
    {
      tibble(geocode_query = query) |>
        tidygeocoder::geocode(
          address = geocode_query, method = "osm", lat = fallback_lat, long = fallback_lon,
          full_results = TRUE, timeout = 60
        )
    },
    error = function(e) {
      message("WARNING: geocoding failed for query: ", query)
      message("Reason: ", conditionMessage(e))
      tibble(geocode_query = query, fallback_lat = NA_real_, fallback_lon = NA_real_, display_name = NA_character_)
    }
  )
  if (!"display_name" %in% names(result)) result$display_name <- NA_character_
  result |> transmute(geocode_query, fallback_lat, fallback_lon, display_name)
}

log_sample_classifications <- function(classified_df, log_file = NULL, print_to_console = TRUE) {
  log_lines <- classified_df |>
    mutate(
      sample_label = coalesce(SAMPLE, paste0("row_", row_number())),
      log_line = paste0(
        "[sample=", sample_label, "] ",
        "COUNTRY='", coalesce(COUNTRY, "NA"), "'; ",
        "COUNTRY_NORMALIZED='", coalesce(COUNTRY_NORMALIZED, "NA"), "'; ",
        "LOCATION='", coalesce(LOCATION, "NA"), "'; ",
        "LAT=", coalesce(LAT, "NA"), "; LON=", coalesce(LON, "NA"), " | ",
        "method=", coalesce(OCEAN_CLASSIFICATION_METHOD, "unclassified"), "; ",
        "BROAD_OCEAN=", coalesce(BROAD_OCEAN, "NA"), "; ",
        "HEMISPHERE=", coalesce(HEMISPHERE, "NA"), "; ",
        "confidence=", coalesce(OCEAN_CLASSIFICATION_CONFIDENCE, "unclassified"), "; ",
        "manual_check=", as.character(MANUAL_CHECK_REQUIRED), "; ",
        "notes='", coalesce(OCEAN_CLASSIFICATION_NOTES, "NA"), "'"
      )
    ) |>
    pull(log_line)

  if (isTRUE(print_to_console)) for (line in log_lines) message(line)
  if (!is.null(log_file)) writeLines(log_lines, con = log_file)
  invisible(log_lines)
}

# ------------------------------------------------------------
# Country fallback rules
# ------------------------------------------------------------

country_rules <- tribble(
  ~country_name, ~broad_ocean, ~hemisphere, ~confidence, ~manual_check_required, ~rule_note,
  "Germany", "Atlantic", "North", "low", TRUE, "Country-only; may refer to North Sea or Baltic Sea.",
  "United States", "ambiguous", "ambiguous", "low", TRUE, "Country-only; Atlantic, Pacific, Arctic, Gulf of Mexico, Caribbean, and territories possible.",
  "Canada", "ambiguous", "North", "low", TRUE, "Country-only; Atlantic, Pacific, and Arctic possible.",
  "Japan", "Pacific", "North", "medium", TRUE, "Country-only; broad basin mostly Pacific, but marginal sea differs.",
  "China", "Pacific", "North", "low", TRUE, "Country-only; broad basin usually Pacific marginal seas for marine/coastal records, but inland records possible.",
  "Philippines", "Pacific", "North", "medium", TRUE, "Country-only; Pacific and marginal seas possible.",
  "India", "Indian", "North", "medium", TRUE, "Country-only; Arabian Sea or Bay of Bengal side unknown.",
  "Norway", "Atlantic", "North", "medium", TRUE, "Country-only; North Atlantic, Norwegian Sea, Barents Sea possible.",
  "Sweden", "Atlantic", "North", "low", TRUE, "Country-only; Baltic/North Sea context ambiguous.",
  "Denmark", "Atlantic", "North", "low", TRUE, "Country-only; North Sea/Baltic transition.",
  "South Africa", "ambiguous", "South", "low", TRUE, "Country-only; Atlantic/Indian boundary region.",
  "Australia", "ambiguous", "South", "low", TRUE, "Country-only; Indian, Pacific, and Southern possible.",
  "New Zealand", "Pacific", "South", "medium", TRUE, "Country-only; broad basin is Pacific, but local sea differs.",
  "United Kingdom", "Atlantic", "North", "medium", TRUE, "Country-only; Atlantic, North Sea, Irish Sea, and English Channel possible.",
  "France", "ambiguous", "North", "low", TRUE, "Country-only; Atlantic, Mediterranean, Channel, and overseas territories possible.",
  "Spain", "ambiguous", "North", "low", TRUE, "Country-only; Atlantic and Mediterranean possible.",
  "Portugal", "Atlantic", "North", "medium", TRUE, "Country-only; mostly Atlantic, but islands may differ in context.",
  "Italy", "Mediterranean", "North", "medium", TRUE, "Country-only; Mediterranean, but local sea differs.",
  "Greece", "Mediterranean", "North", "medium", TRUE, "Country-only; Mediterranean, but local sea differs.",
  "Croatia", "Mediterranean", "North", "medium", TRUE, "Country-only; Adriatic/Mediterranean.",
  "Indonesia", "ambiguous", "ambiguous", "low", TRUE, "Country-only; Indian and Pacific/marginal seas possible.",
  "Chile", "Pacific", "South", "medium", TRUE, "Country-only; broad basin is Pacific but includes high-latitude regions.",
  "Argentina", "Atlantic", "South", "medium", TRUE, "Country-only; broad basin is Atlantic but local region may differ.",
  "Brazil", "Atlantic", "South", "medium", TRUE, "Country-only; broad basin is Atlantic but local region may differ.",
  "American Samoa", "Pacific", "South", "medium", TRUE, "Territory/island fallback; broad basin is Pacific.",
  "Guam", "Pacific", "North", "medium", TRUE, "Territory/island fallback; broad basin is Pacific.",
  "Palau", "Pacific", "North", "medium", TRUE, "Island-country fallback; broad basin is Pacific.",
  "Northern Mariana Islands", "Pacific", "North", "medium", TRUE, "Territory/island fallback; broad basin is Pacific.",
  "French Polynesia", "Pacific", "South", "medium", TRUE, "Territory/island fallback; broad basin is Pacific.",
  "New Caledonia", "Pacific", "South", "medium", TRUE, "Territory/island fallback; broad basin is Pacific.",
  "Fiji", "Pacific", "South", "medium", TRUE, "Island-country fallback; broad basin is Pacific.",
  "Maldives", "Indian", "North", "medium", TRUE, "Island-country fallback; broad basin is Indian Ocean.",
  "Mauritius", "Indian", "South", "medium", TRUE, "Island-country fallback; broad basin is Indian Ocean.",
  "Cape Verde", "Atlantic", "North", "medium", TRUE, "Island-country fallback; broad basin is Atlantic.",
  "Bermuda", "Atlantic", "North", "medium", TRUE, "Island fallback; broad basin is Atlantic.",
  "Curacao", "Atlantic", "North", "medium", TRUE, "Caribbean island fallback; broad basin is Atlantic.",
  "Martinique", "Atlantic", "North", "medium", TRUE, "Caribbean island fallback; broad basin is Atlantic.",
  "Puerto Rico", "Atlantic", "North", "medium", TRUE, "Caribbean/Atlantic island fallback.",
  "Virgin Islands", "Atlantic", "North", "medium", TRUE, "Caribbean/Atlantic island fallback.",
  "Faroe Islands", "Atlantic", "North", "medium", TRUE, "Island fallback; broad basin is Atlantic.",
  "Greenland", "Arctic", "North", "low", TRUE, "Country-only; Arctic/Atlantic boundary depends on locality."
) |>
  mutate(country_key = str_to_lower(country_name)) |>
  distinct(country_key, .keep_all = TRUE)

# ------------------------------------------------------------
# Load marine polygons
# ------------------------------------------------------------

t0 <- time_start("Loading Natural Earth marine polygons")

marine <- ne_download(scale = 50, type = "geography_marine_polys", category = "physical", returnclass = "sf") |>
  st_transform(4326) |>
  st_make_valid()

marine <- marine[!st_is_empty(marine), ]
marine <- suppressWarnings(st_collection_extract(marine, "POLYGON"))
marine <- st_make_valid(marine)
marine <- marine[!st_is_empty(marine), ]

name_col <- intersect(c("name", "NAME", "name_en", "featurecla", "FEATURECLA"), names(marine))[1]
if (is.na(name_col)) stop("Could not identify a name column in the marine polygon dataset.")

marine <- marine |>
  mutate(marine_name = .data[[name_col]], broad_ocean_from_polygon = simplify_ocean_name(marine_name)) |>
  filter(!is.na(broad_ocean_from_polygon))

if (nrow(marine) == 0) stop("No marine polygons remained after broad-ocean filtering.")
message("Loaded ", nrow(marine), " usable marine polygons.")

marine_proj <- marine |> st_transform(8857)
marine_join_proj <- marine |> select(marine_name, broad_ocean_from_polygon) |> st_transform(8857)

time_end(t0, "marine polygon loading")

# ------------------------------------------------------------
# Coordinate classifier
# ------------------------------------------------------------

classify_points_by_coordinates <- function(df) {
  sf::sf_use_s2(FALSE)
  valid <- df |> filter(!is.na(lat_num), !is.na(lon_num))

  if (nrow(valid) == 0) {
    return(df |> mutate(
      coord_broad_ocean = NA_character_, coord_detailed_region = NA_character_,
      coord_ocean_distance_km = NA_real_, coord_classification_method = NA_character_,
      coord_confidence = NA_character_, coord_manual_check_required = NA
    ))
  }

  pts <- valid |> st_as_sf(coords = c("lon_num", "lat_num"), crs = 4326, remove = FALSE)
  pts_proj <- pts |> st_transform(8857)

  joined <- st_join(pts_proj, marine_join_proj, join = st_within, left = TRUE) |>
    st_drop_geometry() |>
    mutate(
      coord_broad_ocean = broad_ocean_from_polygon,
      coord_detailed_region = marine_name,
      coord_ocean_distance_km = if_else(!is.na(coord_broad_ocean), 0, NA_real_),
      coord_classification_method = if_else(!is.na(coord_broad_ocean), "lat_lon_direct_marine_polygon", NA_character_),
      coord_confidence = if_else(!is.na(coord_broad_ocean), "high", NA_character_),
      coord_manual_check_required = if_else(!is.na(coord_broad_ocean), FALSE, NA)
    )

  unresolved <- joined |> filter(is.na(coord_broad_ocean)) |> select(row_id)

  if (nrow(unresolved) > 0) {
    unresolved_pts <- pts_proj |> filter(row_id %in% unresolved$row_id)
    nearest_idx <- st_nearest_feature(unresolved_pts, marine_proj)
    nearest_poly <- marine_proj[nearest_idx, ]
    dist_km <- as.numeric(st_distance(unresolved_pts, nearest_poly, by_element = TRUE)) / 1000

    nearest_tbl <- unresolved_pts |>
      st_drop_geometry() |>
      transmute(
        row_id,
        nearest_broad_ocean = nearest_poly$broad_ocean_from_polygon,
        nearest_detailed_region = nearest_poly$marine_name,
        nearest_distance_km = dist_km
      ) |>
      mutate(
        coord_broad_ocean = if_else(nearest_distance_km <= nearest_ocean_threshold_km, nearest_broad_ocean, NA_character_),
        coord_detailed_region = if_else(nearest_distance_km <= nearest_ocean_threshold_km, nearest_detailed_region, NA_character_),
        coord_ocean_distance_km = nearest_distance_km,
        coord_classification_method = if_else(nearest_distance_km <= nearest_ocean_threshold_km, "lat_lon_nearest_marine_polygon", "lat_lon_too_far_from_marine_polygon"),
        coord_confidence = case_when(
          nearest_distance_km <= 25 ~ "medium",
          nearest_distance_km <= nearest_ocean_threshold_km ~ "low",
          TRUE ~ "unclassified"
        ),
        coord_manual_check_required = TRUE
      ) |>
      select(row_id, coord_broad_ocean, coord_detailed_region, coord_ocean_distance_km, coord_classification_method, coord_confidence, coord_manual_check_required)

    joined <- joined |>
      select(-coord_broad_ocean, -coord_detailed_region, -coord_ocean_distance_km, -coord_classification_method, -coord_confidence, -coord_manual_check_required) |>
      left_join(nearest_tbl, by = "row_id") |>
      mutate(
        coord_broad_ocean = coalesce(broad_ocean_from_polygon, coord_broad_ocean),
        coord_detailed_region = coalesce(marine_name, coord_detailed_region),
        coord_ocean_distance_km = if_else(!is.na(broad_ocean_from_polygon), 0, coord_ocean_distance_km),
        coord_classification_method = if_else(!is.na(broad_ocean_from_polygon), "lat_lon_direct_marine_polygon", coord_classification_method),
        coord_confidence = if_else(!is.na(broad_ocean_from_polygon), "high", coord_confidence),
        coord_manual_check_required = if_else(!is.na(broad_ocean_from_polygon), FALSE, coord_manual_check_required)
      )
  }

  df |> left_join(
    joined |> select(row_id, coord_broad_ocean, coord_detailed_region, coord_ocean_distance_km, coord_classification_method, coord_confidence, coord_manual_check_required),
    by = "row_id"
  )
}

# ------------------------------------------------------------
# Read input TSV
# ------------------------------------------------------------

t0 <- time_start("Reading input TSV")

samples <- read.delim(
  input_tsv, header = TRUE, sep = "\t", quote = "", comment.char = "",
  stringsAsFactors = FALSE, check.names = FALSE, fill = TRUE, colClasses = "character"
)
samples <- tibble::as_tibble(samples)
original_cols <- names(samples)
message("Read TSV with base::read.delim(fill = TRUE).")
message("Columns detected: ", length(original_cols))

required_cols <- c(
  "SAMPLE", "DATASET_TYPE", "STUDY_ACCESSION", "SAMPLE_TITLE", "PROJECT_NAME",
  "TAXON_ID", "SCIENTIFIC_NAME", "CREATED_DATE", "COLLECTION_DATE", "STRATEGY",
  "SOURCE", "INSTRUMENT", "COUNTRY", "LAT", "LON", "LOCATION", "DESCRIPTION",
  "SAMPLE_DESCRIPTION", "GROUP", "LIBRARY_TYPE", "GEO"
)
missing_cols <- setdiff(required_cols, names(samples))
if (length(missing_cols) > 0) stop("Input TSV is missing required columns: ", paste(missing_cols, collapse = ", "))

samples <- samples |>
  mutate(
    row_id = row_number(),
    COUNTRY_NORMALIZED = normalize_country_with_countrycode(COUNTRY),
    LOCATION_NORMALIZED = normalize_location_text(LOCATION),
    DESCRIPTION_NORMALIZED = normalize_location_text(DESCRIPTION),
    SAMPLE_DESCRIPTION_NORMALIZED = normalize_location_text(SAMPLE_DESCRIPTION),
    LAT_CLEAN = clean_text(LAT),
    LON_CLEAN = clean_text(LON),
    lat_num = parse_number(LAT_CLEAN),
    lon_num = parse_number(LON_CLEAN),
    hemisphere_from_coordinates = hemisphere_from_lat(lat_num)
  )

message("Rows read: ", nrow(samples))
message("Rows with LAT/LON: ", sum(!is.na(samples$lat_num) & !is.na(samples$lon_num)))
time_end(t0, "input TSV reading and normalization")

# ------------------------------------------------------------
# Coordinate diagnostics
# ------------------------------------------------------------

invalid_coords <- samples |> filter(!is.na(lat_num), !is.na(lon_num), abs(lat_num) > 90 | abs(lon_num) > 180)
suspect_swapped <- samples |> filter(!is.na(lat_num), !is.na(lon_num), abs(lat_num) > 90, abs(lon_num) <= 90)

if (nrow(invalid_coords) > 0) {
  readr::write_tsv(invalid_coords, "invalid_coordinate_ranges.tsv")
  message("Rows with invalid coordinate ranges: ", nrow(invalid_coords))
}
if (nrow(suspect_swapped) > 0) {
  readr::write_tsv(suspect_swapped, "suspect_swapped_lat_lon.tsv")
  message("Rows with potentially swapped LAT/LON: ", nrow(suspect_swapped))
}

# ------------------------------------------------------------
# Step 1: classify by coordinates
# ------------------------------------------------------------

t0 <- time_start("Classifying coordinates")
samples_coord <- classify_points_by_coordinates(samples)
message("Rows classified from coordinates: ", sum(!is.na(samples_coord$coord_broad_ocean)))

coord_diagnostics <- samples_coord |>
  mutate(has_lat_lon = !is.na(lat_num) & !is.na(lon_num), classified_by_coord = !is.na(coord_broad_ocean)) |>
  filter(has_lat_lon, !classified_by_coord)

if (nrow(coord_diagnostics) > 0) {
  readr::write_tsv(
    coord_diagnostics |>
      select(SAMPLE, SCIENTIFIC_NAME, COUNTRY, LOCATION, LAT, LON, COUNTRY_NORMALIZED, LOCATION_NORMALIZED, LAT_CLEAN, LON_CLEAN, lat_num, lon_num, coord_ocean_distance_km, coord_classification_method, coord_detailed_region),
    "coordinate_records_not_classified.tsv"
  )
  message("Coordinate records not classified: ", nrow(coord_diagnostics))
  message("Wrote coordinate_records_not_classified.tsv")
}
time_end(t0, "coordinate classification")

# ------------------------------------------------------------
# Step 2: fallback candidates
# ------------------------------------------------------------

t0 <- time_start("Preparing fallback candidates")
needs_fallback <- samples_coord |>
  filter(is.na(coord_broad_ocean)) |>
  mutate(
    geocode_query = make_geocode_query(COUNTRY_NORMALIZED, LOCATION_NORMALIZED),
    country_prefix = case_when(str_detect(coalesce(COUNTRY_NORMALIZED, ""), ":") ~ str_trim(str_extract(COUNTRY_NORMALIZED, "^[^:]+")), TRUE ~ COUNTRY_NORMALIZED),
    country_key = str_to_lower(country_prefix),
    country_only = looks_country_only(COUNTRY_NORMALIZED)
  )
message("Rows needing fallback after coordinate classification: ", nrow(needs_fallback))
time_end(t0, "fallback candidate preparation")

# ------------------------------------------------------------
# Step 2A0: coordinates embedded in text
# ------------------------------------------------------------

t0 <- time_start("Parsing coordinates embedded in text fields")
country_text_coords <- parse_coords_from_text(needs_fallback$COUNTRY_NORMALIZED)
location_text_coords <- parse_coords_from_text(needs_fallback$LOCATION_NORMALIZED)
description_text_coords <- parse_coords_from_text(needs_fallback$DESCRIPTION_NORMALIZED)
sample_description_text_coords <- parse_coords_from_text(needs_fallback$SAMPLE_DESCRIPTION_NORMALIZED)
query_text_coords <- parse_coords_from_text(needs_fallback$geocode_query)

needs_fallback <- needs_fallback |>
  bind_cols(
    country_text_coords |> rename(country_text_lat = text_lat, country_text_lon = text_lon),
    location_text_coords |> rename(location_text_lat = text_lat, location_text_lon = text_lon),
    description_text_coords |> rename(description_text_lat = text_lat, description_text_lon = text_lon),
    sample_description_text_coords |> rename(sample_description_text_lat = text_lat, sample_description_text_lon = text_lon),
    query_text_coords |> rename(query_text_lat = text_lat, query_text_lon = text_lon)
  ) |>
  mutate(
    text_lat = coalesce(country_text_lat, location_text_lat, description_text_lat, sample_description_text_lat, query_text_lat),
    text_lon = coalesce(country_text_lon, location_text_lon, description_text_lon, sample_description_text_lon, query_text_lon),
    has_text_coords = !is.na(text_lat) & !is.na(text_lon)
  )

text_coord_rows <- needs_fallback |> filter(has_text_coords) |> transmute(row_id, lat_num = text_lat, lon_num = text_lon)

if (nrow(text_coord_rows) > 0) {
  text_coord_tmp <- text_coord_rows |> mutate(LAT = as.character(lat_num), LON = as.character(lon_num), hemisphere_from_coordinates = hemisphere_from_lat(lat_num))
  text_coord_classified <- classify_points_by_coordinates(text_coord_tmp)
  text_coord_hits <- text_coord_classified |>
    filter(!is.na(coord_broad_ocean)) |>
    transmute(
      row_id,
      fallback_broad_ocean = coord_broad_ocean,
      fallback_hemisphere = hemisphere_from_lat(lat_num),
      fallback_method = "coordinates_parsed_from_text",
      fallback_confidence = coord_confidence,
      fallback_manual_check_required = coord_manual_check_required,
      fallback_notes = paste0("Coordinates parsed from text; nearest/intersected marine region: ", coalesce(coord_detailed_region, "NA"), "."),
      fallback_lat = lat_num,
      fallback_lon = lon_num,
      fallback_resolved_name = NA_character_
    )
} else {
  text_coord_hits <- empty_geocode_hits()
}
message("Rows classified from coordinates parsed in text fields: ", nrow(text_coord_hits))
time_end(t0, "embedded text-coordinate parsing")

# ------------------------------------------------------------
# Step 2A1: explicit marine/coastal text rules, unique-key optimized
# ------------------------------------------------------------

t0 <- time_start("Explicit marine/coastal text rules using unique text keys")
marine_text_input <- needs_fallback |>
  anti_join(text_coord_hits |> select(row_id), by = "row_id") |>
  mutate(marine_text_key = paste(COUNTRY_NORMALIZED, LOCATION_NORMALIZED, DESCRIPTION_NORMALIZED, SAMPLE_DESCRIPTION_NORMALIZED, sep = " | "))

marine_text_unique <- marine_text_input |>
  distinct(marine_text_key, COUNTRY_NORMALIZED, LOCATION_NORMALIZED, DESCRIPTION_NORMALIZED, SAMPLE_DESCRIPTION_NORMALIZED)
message("Unique marine/coastal text strings to classify: ", nrow(marine_text_unique))

marine_text_unique_classified <- marine_text_unique |>
  mutate(
    marine_text_broad_ocean = classify_ocean_from_marine_text(COUNTRY_NORMALIZED, LOCATION_NORMALIZED, DESCRIPTION_NORMALIZED, SAMPLE_DESCRIPTION_NORMALIZED),
    marine_text_hemisphere = hemisphere_from_marine_text(COUNTRY_NORMALIZED, LOCATION_NORMALIZED, DESCRIPTION_NORMALIZED, SAMPLE_DESCRIPTION_NORMALIZED)
  ) |>
  select(marine_text_key, marine_text_broad_ocean, marine_text_hemisphere)

marine_text_hits <- marine_text_input |>
  left_join(marine_text_unique_classified, by = "marine_text_key") |>
  filter(!is.na(marine_text_broad_ocean)) |>
  transmute(
    row_id,
    fallback_broad_ocean = marine_text_broad_ocean,
    fallback_hemisphere = marine_text_hemisphere,
    fallback_method = "explicit_marine_text_rule",
    fallback_confidence = case_when(fallback_broad_ocean == "inland_sea" ~ "low", TRUE ~ "medium"),
    fallback_manual_check_required = TRUE,
    fallback_notes = paste0("Classified from explicit marine/coastal text. COUNTRY_NORMALIZED='", coalesce(COUNTRY_NORMALIZED, "NA"), "', LOCATION_NORMALIZED='", coalesce(LOCATION_NORMALIZED, "NA"), "'."),
    fallback_lat = NA_real_,
    fallback_lon = NA_real_,
    fallback_resolved_name = NA_character_
  )
message("Rows classified from explicit marine/coastal text rules: ", nrow(marine_text_hits))
time_end(t0, "explicit marine/coastal text rules")

# ------------------------------------------------------------
# Step 2A2: US state/territory rules, US-only unique-key optimized
# ------------------------------------------------------------

t0 <- time_start("US state/territory rules on US-only unique text keys")
us_candidates <- needs_fallback |>
  anti_join(text_coord_hits |> select(row_id), by = "row_id") |>
  anti_join(marine_text_hits |> select(row_id), by = "row_id") |>
  filter(str_detect(COUNTRY_NORMALIZED, "^United States(:|$)"))
message("US state-rule candidate rows: ", nrow(us_candidates))

if (nrow(us_candidates) > 0) {
  us_state_input <- us_candidates |> mutate(us_text_key = paste(COUNTRY_NORMALIZED, LOCATION_NORMALIZED, DESCRIPTION_NORMALIZED, SAMPLE_DESCRIPTION_NORMALIZED, sep = " | "))
  us_state_unique <- us_state_input |> distinct(us_text_key, COUNTRY_NORMALIZED, LOCATION_NORMALIZED, DESCRIPTION_NORMALIZED, SAMPLE_DESCRIPTION_NORMALIZED)
  message("Unique US state text strings to classify: ", nrow(us_state_unique))

  us_state_unique_classified <- us_state_unique |>
    rowwise() |>
    mutate(us_state_result = list(classify_us_state_from_text(COUNTRY_NORMALIZED, LOCATION_NORMALIZED, DESCRIPTION_NORMALIZED, SAMPLE_DESCRIPTION_NORMALIZED))) |>
    tidyr::unnest(us_state_result) |>
    ungroup() |>
    select(us_text_key, us_state_broad_ocean, us_state_hemisphere, us_state_match, us_state_confidence, us_state_note)

  us_state_hits <- us_state_input |>
    left_join(us_state_unique_classified, by = "us_text_key") |>
    filter(!is.na(us_state_broad_ocean)) |>
    transmute(
      row_id,
      fallback_broad_ocean = us_state_broad_ocean,
      fallback_hemisphere = us_state_hemisphere,
      fallback_method = "us_state_text_rule",
      fallback_confidence = us_state_confidence,
      fallback_manual_check_required = TRUE,
      fallback_notes = paste0(us_state_note, " Matched state='", us_state_match, "'. COUNTRY_NORMALIZED='", coalesce(COUNTRY_NORMALIZED, "NA"), "', LOCATION_NORMALIZED='", coalesce(LOCATION_NORMALIZED, "NA"), "'."),
      fallback_lat = NA_real_,
      fallback_lon = NA_real_,
      fallback_resolved_name = us_state_match
    )
} else {
  us_state_hits <- empty_geocode_hits()
}
message("Rows classified from US state/territory text rules: ", nrow(us_state_hits))
time_end(t0, "US state/territory text rules")

# ------------------------------------------------------------
# Step 2A3: country-only rules
# ------------------------------------------------------------

t0 <- time_start("Country-only rules")
country_rule_hits <- needs_fallback |>
  anti_join(text_coord_hits |> select(row_id), by = "row_id") |>
  anti_join(marine_text_hits |> select(row_id), by = "row_id") |>
  anti_join(us_state_hits |> select(row_id), by = "row_id") |>
  left_join(country_rules, by = "country_key") |>
  filter(country_only, !is.na(broad_ocean)) |>
  transmute(
    row_id,
    fallback_broad_ocean = broad_ocean,
    fallback_hemisphere = hemisphere,
    fallback_method = "country_only_rule",
    fallback_confidence = confidence,
    fallback_manual_check_required = manual_check_required,
    fallback_notes = rule_note,
    fallback_lat = NA_real_,
    fallback_lon = NA_real_,
    fallback_resolved_name = NA_character_
  )
message("Rows classified by country-only rules: ", nrow(country_rule_hits))
time_end(t0, "country-only rules")

# ------------------------------------------------------------
# Step 2B: geocode remaining rows with cache and safe batching
# ------------------------------------------------------------

t0 <- time_start("Preparing cached Nominatim geocoding")
to_geocode_all <- needs_fallback |>
  anti_join(text_coord_hits |> select(row_id), by = "row_id") |>
  anti_join(marine_text_hits |> select(row_id), by = "row_id") |>
  anti_join(us_state_hits |> select(row_id), by = "row_id") |>
  anti_join(country_rule_hits |> select(row_id), by = "row_id") |>
  filter(!is.na(geocode_query)) |>
  filter(!has_text_coords) |>
  transmute(row_id, geocode_query_original = geocode_query, geocode_query = strip_station_prefix(geocode_query)) |>
  filter(!is.na(geocode_query))

to_geocode_unique <- to_geocode_all |> distinct(geocode_query)

if (file.exists(geocode_cache_file)) {
  geocode_cache <- readr::read_tsv(geocode_cache_file, col_types = cols(.default = col_character()), show_col_types = FALSE) |>
    mutate(fallback_lat = suppressWarnings(as.numeric(fallback_lat)), fallback_lon = suppressWarnings(as.numeric(fallback_lon)))
} else {
  geocode_cache <- tibble(geocode_query = character(), fallback_lat = numeric(), fallback_lon = numeric(), display_name = character())
}

needed_cache_cols <- c("geocode_query", "fallback_lat", "fallback_lon", "display_name")
for (cc in needed_cache_cols) if (!cc %in% names(geocode_cache)) geocode_cache[[cc]] <- NA
geocode_cache <- geocode_cache |> select(all_of(needed_cache_cols)) |> distinct(geocode_query, .keep_all = TRUE)

to_geocode_new <- to_geocode_unique |> anti_join(geocode_cache |> select(geocode_query), by = "geocode_query") |> arrange(geocode_query)

message("")
message("------------------------------------------------------------")
message("Geocoding cache file: ", geocode_cache_file)
message("Unique fallback geocoding queries: ", nrow(to_geocode_unique))
message("Already cached queries: ", nrow(to_geocode_unique) - nrow(to_geocode_new))
message("New queries pending Nominatim submission: ", nrow(to_geocode_new))
message("------------------------------------------------------------")
message("")

if (nrow(to_geocode_new) > 0) {
  message("Queries pending Nominatim submission:")
  print(to_geocode_new |> mutate(query_number = row_number()) |> select(query_number, geocode_query), n = Inf)
  readr::write_tsv(to_geocode_new, "queries_pending_nominatim.tsv")
  message("Wrote pending queries to: queries_pending_nominatim.tsv")
}
time_end(t0, "Nominatim preparation")

t0 <- time_start("Nominatim geocoding")
if (!run_nominatim && nrow(to_geocode_new) > 0) {
  message("run_nominatim is FALSE, so no new queries were submitted. Continuing with cached results only.")
} else if (nrow(to_geocode_new) > 0) {
  to_geocode_submit <- to_geocode_new |> slice_head(n = max_nominatim_queries_per_run)
  if (nrow(to_geocode_submit) < nrow(to_geocode_new)) message("Submitting only the first ", nrow(to_geocode_submit), " of ", nrow(to_geocode_new), " pending queries in this run.")

  query_batches <- split(to_geocode_submit, ceiling(seq_len(nrow(to_geocode_submit)) / nominatim_batch_size))
  for (i in seq_along(query_batches)) {
    batch <- query_batches[[i]]
    message("Submitting Nominatim batch ", i, " of ", length(query_batches), " with ", nrow(batch), " unique queries.")
    print(batch |> mutate(batch_query_number = row_number()) |> select(batch_query_number, geocode_query), n = Inf)

    geocoded_batch_cache <- purrr::map_dfr(batch$geocode_query, safe_geocode_one)
    geocode_cache <- bind_rows(geocode_cache, geocoded_batch_cache) |> distinct(geocode_query, .keep_all = TRUE)
    readr::write_tsv(geocode_cache, geocode_cache_file)
    message("Updated geocoding cache after batch ", i, ": ", geocode_cache_file)

    if (i < length(query_batches)) {
      message("Pausing for ", nominatim_pause_seconds, " seconds before next batch...")
      Sys.sleep(nominatim_pause_seconds)
    }
  }
}
time_end(t0, "Nominatim geocoding")

# ------------------------------------------------------------
# Step 2C: classify geocoded fallback points
# ------------------------------------------------------------

t0 <- time_start("Classifying geocoded fallback points")
geocoded <- to_geocode_all |> left_join(geocode_cache, by = "geocode_query") |> distinct(row_id, .keep_all = TRUE)

if (nrow(geocoded) > 0) {
  geo_pts <- geocoded |>
    filter(!is.na(fallback_lat), !is.na(fallback_lon)) |>
    st_as_sf(coords = c("fallback_lon", "fallback_lat"), crs = 4326, remove = FALSE)

  if (nrow(geo_pts) > 0) {
    geo_pts_proj <- geo_pts |> st_transform(8857)
    geo_joined <- st_join(geo_pts_proj, marine_join_proj, join = st_within, left = TRUE) |> st_drop_geometry()
    unresolved_geo <- geo_joined |> filter(is.na(broad_ocean_from_polygon))

    if (nrow(unresolved_geo) > 0) {
      unresolved_geo_pts <- geo_pts_proj |> filter(row_id %in% unresolved_geo$row_id)
      nearest_idx <- st_nearest_feature(unresolved_geo_pts, marine_proj)
      nearest_poly <- marine_proj[nearest_idx, ]
      dist_km <- as.numeric(st_distance(unresolved_geo_pts, nearest_poly, by_element = TRUE)) / 1000

      nearest_geo_tbl <- unresolved_geo_pts |>
        st_drop_geometry() |>
        transmute(row_id, nearest_broad_ocean = nearest_poly$broad_ocean_from_polygon, nearest_detailed_region = nearest_poly$marine_name, nearest_distance_km = dist_km)

      geo_joined <- geo_joined |>
        left_join(nearest_geo_tbl, by = "row_id") |>
        mutate(
          fallback_broad_ocean = coalesce(broad_ocean_from_polygon, if_else(nearest_distance_km <= nearest_ocean_threshold_km, nearest_broad_ocean, NA_character_)),
          fallback_method = case_when(!is.na(broad_ocean_from_polygon) ~ "geocoded_point_direct_marine_polygon", nearest_distance_km <= nearest_ocean_threshold_km ~ "geocoded_point_nearest_marine_polygon", TRUE ~ "geocoded_point_too_far_from_marine_polygon"),
          fallback_confidence = case_when(!is.na(broad_ocean_from_polygon) ~ "medium", nearest_distance_km <= 25 ~ "medium-low", nearest_distance_km <= nearest_ocean_threshold_km ~ "low", TRUE ~ "unclassified"),
          fallback_manual_check_required = TRUE,
          fallback_notes = case_when(
            !is.na(broad_ocean_from_polygon) ~ "Geocoded point falls within marine polygon.",
            nearest_distance_km <= nearest_ocean_threshold_km ~ paste0("Geocoded point is on land or freshwater; assigned nearest marine region within ", nearest_ocean_threshold_km, " km."),
            TRUE ~ "Geocoded point is too far from a marine polygon; not assigned."
          )
        )
    } else {
      geo_joined <- geo_joined |>
        mutate(fallback_broad_ocean = broad_ocean_from_polygon, fallback_method = "geocoded_point_direct_marine_polygon", fallback_confidence = "medium", fallback_manual_check_required = TRUE, fallback_notes = "Geocoded point falls within marine polygon.")
    }

    geocode_hits <- geo_joined |>
      transmute(row_id, fallback_broad_ocean, fallback_hemisphere = hemisphere_from_lat(fallback_lat), fallback_method, fallback_confidence, fallback_manual_check_required, fallback_notes, fallback_lat, fallback_lon, fallback_resolved_name = display_name)
  } else {
    geocode_hits <- empty_geocode_hits()
  }
} else {
  geocode_hits <- empty_geocode_hits()
}
message("Rows with geocoding fallback results: ", nrow(geocode_hits))
time_end(t0, "geocoded fallback-point classification")

# ------------------------------------------------------------
# Combine classifications and write output
# ------------------------------------------------------------

t0 <- time_start("Combining classifications and preparing output")
fallback_results <- bind_rows(text_coord_hits, marine_text_hits, us_state_hits, country_rule_hits, geocode_hits)

classified <- samples_coord |>
  left_join(fallback_results, by = "row_id") |>
  mutate(
    BROAD_OCEAN = coalesce(coord_broad_ocean, fallback_broad_ocean),
    HEMISPHERE = coalesce(hemisphere_from_coordinates, fallback_hemisphere),
    OCEAN_CLASSIFICATION_METHOD = coalesce(coord_classification_method, fallback_method),
    OCEAN_CLASSIFICATION_CONFIDENCE = coalesce(coord_confidence, fallback_confidence, "unclassified"),
    MANUAL_CHECK_REQUIRED = case_when(!is.na(coord_manual_check_required) ~ coord_manual_check_required, !is.na(fallback_manual_check_required) ~ fallback_manual_check_required, TRUE ~ TRUE),
    OCEAN_CLASSIFICATION_NOTES = case_when(
      !is.na(coord_broad_ocean) & coord_ocean_distance_km == 0 ~ paste0("LAT/LON point intersects marine region: ", coord_detailed_region, "."),
      !is.na(coord_broad_ocean) & coord_ocean_distance_km > 0 ~ paste0("LAT/LON point assigned to nearest marine region: ", coord_detailed_region, "; distance = ", round(coord_ocean_distance_km, 1), " km."),
      !is.na(fallback_notes) ~ fallback_notes,
      TRUE ~ "Could not classify; manual review required."
    )
  )

new_output_cols <- c(
  "COUNTRY_NORMALIZED", "LOCATION_NORMALIZED", "DESCRIPTION_NORMALIZED", "SAMPLE_DESCRIPTION_NORMALIZED", "LAT_CLEAN", "LON_CLEAN",
  "BROAD_OCEAN", "HEMISPHERE", "OCEAN_CLASSIFICATION_METHOD", "OCEAN_CLASSIFICATION_CONFIDENCE", "MANUAL_CHECK_REQUIRED", "OCEAN_CLASSIFICATION_NOTES",
  "fallback_resolved_name", "fallback_lat", "fallback_lon", "coord_detailed_region", "coord_ocean_distance_km"
)

classified <- classified |> select(all_of(original_cols), any_of(new_output_cols))
time_end(t0, "classification combination")

if (isTRUE(print_sample_classifications) || isTRUE(write_classification_log)) {
  t0 <- time_start("Writing per-sample classification log")
  log_sample_classifications(classified, log_file = if (isTRUE(write_classification_log)) classification_log_file else NULL, print_to_console = isTRUE(print_sample_classifications))
  if (isTRUE(write_classification_log)) message("Per-sample classification log written to: ", classification_log_file)
  time_end(t0, "per-sample classification log")
}

t0 <- time_start("Writing output TSV")
readr::write_tsv(classified, output_tsv)
time_end(t0, "output TSV writing")

message("")
message("Done.")
message("Output written to: ", output_tsv)
message("")
message("Summary:")
print(classified |> count(BROAD_OCEAN, HEMISPHERE, OCEAN_CLASSIFICATION_CONFIDENCE, sort = TRUE), n = Inf)
message("")
message("Manual-check summary:")
print(classified |> count(MANUAL_CHECK_REQUIRED, OCEAN_CLASSIFICATION_METHOD, sort = TRUE), n = Inf)
