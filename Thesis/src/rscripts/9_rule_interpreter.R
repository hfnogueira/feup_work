# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#                   Multi-Algorithm Rule Interpreter
#                   by:  hugonogueira
#
#  Translates rules from all three algorithms (CarenR, RIPPER, M5Rules) into
#  plain, structured English. All three sections share the same feature-name
#  decoder, so encoded features like `iaf_hv_y1_c10` are humanised consistently
#  across algorithms (→ "Leaf Area Index in a 10-day window centred on Harvest
#  (current year)").
#
#  Inputs (all from data/<detrend_method>/, produced by run_all_pipelines.R):
#    - RulesTemp_{region}_{detrend_method}.csv         (CarenR distribution rules)
#    - ripper_rules_structured_{region}.csv            (RIPPER classification)
#    - m5rules_rules_structured_{region}.csv           (M5Rules regression)
#
#  Outputs (one .txt narrative + one .csv table per algorithm per region):
#    Section 1 — CarenR    →  rules_interpreted_{region}.{txt,csv}
#    Section 2 — RIPPER    →  ripper_interpreted_{region}.{txt,csv}
#    Section 3 — M5Rules   →  m5rules_interpreted_{region}.{txt,csv}
#
#  How to read CarenR output:
#    Each rule describes a SUBGROUP of years sharing similar climate conditions
#    where wine production (or its deviation from trend when detrend=TRUE)
#    has a notably different distribution compared to all years.
#    "Support" = fraction of years matching the rule's conditions.
#    "Mean" = mean Wine_mhl (or residual) in that subgroup.
#    Positive mean (detrended) → above-trend production years.
#    Negative mean (detrended) → below-trend production years.
#
#  How to read RIPPER output:
#    Ordered decision list — first matching rule's class wins. Default rule
#    catches everything else. Each rule predicts Low / Medium / High production.
#
#  How to read M5Rules output:
#    Ordered piecewise linear rules — first matching rule's linear model is
#    used for the prediction. The decoded equation shows each feature's
#    coefficient and humanised name.
#
#  Requires: tidyverse
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

library(tidyverse)


# config ------------------------------------------------------------------------------
# Shared params (detrend, detrend_method) come from utils/config.R so they
# automatically match whatever the pipeline scripts used.

source("src/rscripts/utils/config.R")

# Per-run state — change to switch region
file           <- 1     # 1 = RDD  |  other = RVV

# Per-script tuning — CarenR rule filters
# Minimum support (fraction of years) — rules below this are too rare
min_sup_filter <- 0.15
# Maximum p-value — only show statistically relevant rules
max_pval_filter <- 0.10


# load rules --------------------------------------------------------------------------

if (file == 1) { region <- 'RDD' } else { region <- 'RVV' }

rules_file <- if (detrend) {
  file.path('data', detrend_method,
            paste0('RulesTemp_', tolower(region), '_', detrend_method, '.csv'))
} else {
  file.path('data', 'no_detrend',
            paste0('RulesTemp_', tolower(region), '_no_detrend.csv'))
}

if (!file.exists(rules_file)) {
  stop("Rules file not found: ", rules_file,
       "\nRun script 3 first with the same region and detrend_method.")
}

drs_raw <- read.table(rules_file, sep = ";", dec = ".", header = TRUE,
                      stringsAsFactors = FALSE)

cat('Loaded', nrow(drs_raw), 'rules from', rules_file, '\n')


# ==============================================================================
# FEATURE NAME DECODER
# ==============================================================================

# --- Metric lookup -----------------------------------------------------------
#
# Maps the metric token in a feature name to a human label and measurement unit.
# Also records whether the metric is a count (integer) or a continuous average/sum.

metric_labels <- list(

  # ── Mean daily temperature (tm) ─────────────────────────────────────────
  # Averaged over window. Core thermal proxy for vine phenology: drives bud
  # burst timing, berry set, sugar accumulation, and ripening speed.
  "tm"  = list(label  = "Mean daily temperature",
               unit   = "°C",
               type   = "avg",
               domain = "Core thermal proxy — drives bud burst, berry set, sugar accumulation, and ripening speed."),

  # tn used in code as Tmed average for post-harvest window (same interpretation)
  "tn"  = list(label  = "Mean daily temperature",
               unit   = "°C",
               type   = "avg",
               domain = "Core thermal proxy — drives bud burst, berry set, sugar accumulation, and ripening speed."),

  # ── Cold days: Tmed < 15°C (tm<15°C / tm.days.less.15) ─────────────────
  # During flowering, cold conditions below 15°C impair pollen viability and
  # fertilisation, directly reducing berry set and yield potential.
  "tm.days.less.15" = list(label  = "Cold days (mean temperature below 15°C)",
                            unit   = "days",
                            type   = "count",
                            domain = "Impairs pollen viability and fertilisation during flowering — directly reduces berry set and yield potential."),

  # ── Frost days: Tmin < 0°C (ti<0°C) ───────────────────────────────────
  # Frost events around bud break can irreversibly damage newly emerged shoots,
  # causing partial or total yield loss for that growing season.
  "days_minTemp.less.0" = list(label  = "Frost days (minimum temperature below 0°C)",
                                unit   = "days",
                                type   = "count",
                                domain = "Frost around bud break irreversibly damages newly emerged shoots — can cause partial or total yield loss."),
  "days.minTemp.less.0" = list(label  = "Frost days (minimum temperature below 0°C)",
                                unit   = "days",
                                type   = "count",
                                domain = "Frost around bud break irreversibly damages newly emerged shoots — can cause partial or total yield loss."),

  # ── Heat-stress days: Tmax > 35°C (ta>35°C) ────────────────────────────
  # During flowering: flower abortion and poor berry set.
  # During maturation: accelerates sugar rise while impairing acid retention
  # and aroma development.
  "days.maxTemp.above.35" = list(label  = "Heat-stress days (maximum temperature above 35°C)",
                                  unit   = "days",
                                  type   = "count",
                                  domain = "Causes flower abortion and poor berry set during flowering; accelerates sugar rise while impairing acid retention and aroma during maturation."),
  "days.tempMax.above.35" = list(label  = "Heat-stress days (maximum temperature above 35°C)",
                                  unit   = "days",
                                  type   = "count",
                                  domain = "Causes flower abortion and poor berry set during flowering; accelerates sugar rise while impairing acid retention and aroma during maturation."),

  # ── Rainfall sum (rs / rf) ──────────────────────────────────────────────
  # Total accumulated rainfall over the window. Captures overall water input —
  # critical around flowering (dilution risk) and harvest (disease, berry splitting).
  "rf" = list(label  = "Rainfall sum",
              unit   = "mm",
              type   = "sum",
              domain = "Total water input — dilution risk around flowering; disease pressure and berry splitting risk around harvest."),

  # ── Wet days: rain > 1 mm (rc>1mm / days.rf.above.1mm) ─────────────────
  # Frequent wet days increase humidity and disease pressure (downy mildew,
  # botrytis), and during flowering they physically hinder pollination.
  "days.rf.above.1mm" = list(label  = "Wet days (daily rainfall above 1 mm)",
                              unit   = "days",
                              type   = "count",
                              domain = "Increases humidity and disease pressure (downy mildew, botrytis); physically hinders pollination during flowering."),

  # ── Leaf Area Index — IAF (iaf) ─────────────────────────────────────────
  # Averaged over window. Proxy for canopy development and vine vigour.
  "iaf" = list(label  = "Leaf Area Index (IAF)",
               unit   = "m²/m²",
               type   = "avg",
               domain = "Proxy for canopy development and vine vigour — reflects growth conditions and photosynthetic capacity."),

  # ── Available soil water (swa) ──────────────────────────────────────────
  # Averaged over window (Sm column). Reflects soil moisture balance.
  "swa" = list(label  = "Available soil water",
               unit   = "mm",
               type   = "avg",
               domain = "Soil moisture balance — governs water stress and berry growth rate."),

  # ── Drought-stress days: Sm < 1.5 × Wp (sw<1.5Wp / sw.less.15wp) ───────
  # Days where soil water falls below 1.5× the wilting point threshold.
  # (Douro: Wp = 402 mm; Vinho Verde: Wp = 338 mm)
  "sw.less.15wp" = list(label  = "Drought-stress days (soil water below 1.5× wilting point)",
                         unit   = "days",
                         type   = "count",
                         domain = "Prolonged drought stress restricts berry growth and accelerates ripening; severe stress can cause raisin effect and yield loss."),

  # ── Waterlogging days: Sm > 0.9 × Fc (sw>0.9Fc / sw.above.09fc) ────────
  # Days where soil water exceeds 0.9× field capacity.
  # (Douro: Fc = 142.8 mm; Vinho Verde: Fc = 77 mm)
  "sw.above.09fc" = list(label  = "Wet-soil days (soil water above 0.9× field capacity)",
                          unit   = "days",
                          type   = "count",
                          domain = "Near-saturation soil conditions promote vegetative growth and disease, and can dilute berry sugars around harvest.")
)

# --- Phenological stage lookup -----------------------------------------------

stage_labels <- list(
  bb = "Bud Break",
  fl = "Flowering",
  sm = "Start of Maturity",
  hv = "Harvest"
)

# --- Temporal window translator ----------------------------------------------

describe_window <- function(window_code) {
  type <- substr(window_code, 1, 1)
  n    <- as.integer(substr(window_code, 2, nchar(window_code)))
  switch(type,
         c = paste0("in a ", n, "-day window centred on"),
         b = paste0("in the ", n, " days before"),
         a = paste0("in the ", n, " days after"),
         window_code)   # fallback: return raw code
}


# --- Biological-pathway annotation -------------------------------------------
#
# Returns a one-line biological-pathway annotation for a feature, used to
# enrich rule narratives with the mechanism by which the feature affects yield.
#
# Honours the causal-pathway feature-design principle from section §[features]:
#   - Post-harvest y0 windows describe the PREVIOUS-YEAR carbohydrate-reserves
#     pathway, which affects NEXT-vintage potential (not the current vintage).
#   - Pre-harvest or centred y1 windows describe WITHIN-VINTAGE mechanisms
#     (berry splitting, disease pressure, dilution, etc.).
# When no context-specific override applies, the generic metric-level domain
# text from `metric_labels[[metric]]$domain` is used. If even that is absent,
# NA is returned and no annotation is printed.

pathway_note <- function(metric, stage, year, window_code) {

  win_type <- substr(window_code, 1, 1)  # "c", "b", or "a"

  # --- Context-aware overrides ---------------------------------------------

  # (1) Post-harvest y0 + after-window: previous-year reserves pathway.
  #     This is the family the new features rf_hv_y0_a10,
  #     days.rf.above.1mm_hv_y0_a10, and the existing swa_hv_y0_*,
  #     tn_hv_y0_a30 belong to. The pathway is *carry-over*, not within-
  #     vintage damage.
  if (stage == "hv" && year == "y0" && win_type == "a") {

    if (metric %in% c("rf", "days.rf.above.1mm", "swa",
                      "sw.less.15wp", "sw.above.09fc")) {
      return(paste0(
        "post-harvest reserves pathway — moisture during this window in the ",
        "previous calendar year replenishes vine carbohydrate reserves before ",
        "dormancy; affects next-vintage bud break, shoot vigour, and yield ",
        "potential, not the current vintage."))
    }
    if (metric %in% c("tm", "tn")) {
      return(paste0(
        "post-harvest thermal pathway — late-season temperature in the ",
        "previous year's post-harvest window drives autumn carbon fixation, ",
        "reserve accumulation, and the timing of dormancy onset; shapes ",
        "next-vintage potential rather than the current vintage."))
    }
    if (metric == "iaf") {
      return(paste0(
        "post-harvest canopy pathway — previous-year leaf area after harvest ",
        "proxies the duration of active leaf cover, during which the vine ",
        "accumulates reserves for the next vintage."))
    }
  }

  # (2) Pre-/centred-harvest y1 + moisture: late-ripening damage pathway.
  #     Different from the generic 'rf' domain text in that it pins the
  #     mechanism to the current vintage explicitly.
  if (stage == "hv" && year == "y1" && win_type %in% c("b", "c")) {
    if (metric == "rf") {
      return(paste0(
        "late-ripening moisture pathway — rain just before or around current ",
        "harvest causes berry splitting, sugar dilution, and elevated disease ",
        "pressure (botrytis, mildew); directly suppresses current-vintage ",
        "yield and quality."))
    }
    if (metric == "days.rf.above.1mm") {
      return(paste0(
        "late-ripening wet-day pathway — frequent wet days near current ",
        "harvest raise humidity and disease pressure, hinder picking ",
        "logistics, and increase berry-splitting risk for the current ",
        "vintage."))
    }
  }

  # --- Default: generic metric-level domain text ---------------------------
  met_info <- metric_labels[[metric]]
  if (!is.null(met_info) && !is.null(met_info$domain) &&
      nzchar(met_info$domain)) {
    return(met_info$domain)
  }

  NA_character_
}

# Wrapper: feature name -> pathway note string (or NA)
feature_pathway <- function(feat) {
  m <- regmatches(feat,
         regexec("^(.+)_(bb|fl|sm|hv)_(y0|y1)_([cba]\\d+)$",
                 feat, perl = TRUE))[[1]]
  if (length(m) < 5) return(NA_character_)
  pathway_note(metric = m[2], stage = m[3],
               year   = m[4], window_code = m[5])
}

# Helper: given a sentence vector and a parallel feature-name vector, build
# a "    • <sentence>\n        → <pathway>" formatted block per condition.
# The pathway text is word-wrapped to ~70 chars and indented so it reads as a
# clear sub-annotation under the bullet.
format_conditions_with_pathways <- function(sentences, features,
                                            bullet_indent = "    ",
                                            note_indent   = "        ") {
  out <- character()
  for (k in seq_along(sentences)) {
    out <- c(out, paste0(bullet_indent, "• ", sentences[k]))
    if (!is.na(features[k])) {
      note <- feature_pathway(features[k])
      if (!is.na(note)) {
        wrapped <- strwrap(note, width = 72,
                           initial = paste0(note_indent, "→ "),
                           prefix  = paste0(note_indent, "  "))
        out <- c(out, wrapped)
      }
    }
  }
  out
}


# ==============================================================================
# CORE DECODER: feature name → plain English phrase
# ==============================================================================

decode_feature <- function(feat) {

  # Match pattern:  [metric]_(bb|fl|sm|hv)_(y0|y1)_(c|b|a)[digits]
  m <- regmatches(feat,
         regexpr("^(.+)_(bb|fl|sm|hv)_(y0|y1)_([cba]\\d+)$", feat, perl = TRUE))

  if (length(m) == 0) return(feat)   # unrecognised — return as-is

  parts     <- regmatches(feat,
                 regexec("^(.+)_(bb|fl|sm|hv)_(y0|y1)_([cba]\\d+)$", feat, perl = TRUE))[[1]]
  metric    <- parts[2]
  stage_raw <- parts[3]
  year_raw  <- parts[4]
  window    <- parts[5]

  # Look up each component
  met_info  <- metric_labels[[metric]]
  met_label <- if (!is.null(met_info)) met_info$label else metric
  met_unit  <- if (!is.null(met_info)) met_info$unit  else ""

  stage_str <- stage_labels[[stage_raw]]
  if (is.null(stage_str)) stage_str <- stage_raw

  year_str  <- if (year_raw == "y1") "current year" else "previous year"
  win_str   <- describe_window(window)

  paste0(met_label, " ", win_str, " ", stage_str, " (", year_str, ")")
}


# ==============================================================================
# CONDITION TRANSLATOR:  "feature=[lower|upper)" → English sentence
# ==============================================================================

translate_condition <- function(subgroup_str) {

  # Split compound condition on " & "
  raw_conds <- trimws(strsplit(subgroup_str, "\\s*&\\s*")[[1]])

  parsed <- lapply(raw_conds, function(cond) {

    # Parse: feature=[lower|upper)  or  feature=[lower|upper]
    m <- regmatches(cond,
           regexec("^(.+)=\\[([^|]+)\\|([^])|]+)([)|\\]])$", cond, perl = TRUE))[[1]]

    if (length(m) == 0) return(list(sentence = cond, feature = NA_character_))

    feat        <- m[2]
    lower       <- as.numeric(m[3])
    upper       <- as.numeric(m[4])
    right_incl  <- m[5] == "]"

    feat_label  <- decode_feature(feat)

    # Look up unit for inline display
    feat_parts  <- regmatches(feat,
                    regexec("^(.+)_(bb|fl|sm|hv)_(y0|y1)_([cba]\\d+)$", feat,
                            perl = TRUE))[[1]]
    met_info    <- if (length(feat_parts) > 1) metric_labels[[feat_parts[2]]] else NULL
    unit_str    <- if (!is.null(met_info) && nchar(met_info$unit) > 0)
                     paste0(" ", met_info$unit) else ""

    # Phrase the range
    range_phrase <- if (is.infinite(lower) && lower < 0) {
      paste0("below ", upper, unit_str)
    } else if (is.infinite(upper) && upper > 0) {
      paste0("above ", lower, unit_str)
    } else {
      right_sym <- if (right_incl) "inclusive" else "exclusive"
      paste0("between ", lower, unit_str, " and ", upper, unit_str)
    }

    list(sentence = paste0(feat_label, " is ", range_phrase),
         feature  = feat)
  })

  list(
    sentences = vapply(parsed, `[[`, character(1), "sentence", USE.NAMES = FALSE),
    features  = vapply(parsed, `[[`, character(1), "feature",  USE.NAMES = FALSE)
  )
}


# ==============================================================================
# RULE TRANSLATOR:  full drs row → narrative block
# ==============================================================================

translate_rule <- function(row_idx, rules_df, detrend_flag, n_total = NULL) {

  row    <- rules_df[row_idx, ]
  sub    <- as.character(row$Subgroup)
  sup    <- as.numeric(row$Ant_sup)
  pval   <- as.numeric(row$pvalue)
  mean_v <- as.numeric(row$Mean)
  sd_v   <- if ("Stdev" %in% names(row)) as.numeric(row$Stdev) else NA
  skew_v <- if ("Skew"  %in% names(row)) as.numeric(row$Skew)  else NA

  n_years <- if (!is.null(n_total)) round(sup * n_total) else "?"

  parsed     <- translate_condition(sub)
  conditions <- parsed$sentences
  features   <- parsed$features
  cond_block <- format_conditions_with_pathways(conditions, features)

  # Direction of effect
  direction <- if (!is.na(mean_v) && mean_v > 0) "ABOVE-TREND (positive)" else
               if (!is.na(mean_v) && mean_v < 0) "BELOW-TREND (negative)" else
               "NEUTRAL"

  if (!detrend_flag) {
    direction <- if (!is.na(mean_v)) paste0("mean production ", round(mean_v, 1), " mhl") else "?"
  }

  # Build narrative
  lines <- c(
    paste0(strrep("-", 60)),
    paste0("RULE ", row_idx, "  |  Support: ", round(sup * 100, 1), "%",
           "  (≈", n_years, " years)  |  p-value: ", signif(pval, 3)),
    "",
    "  WHEN:",
    cond_block,
    "",
    "  THEN wine production is:  ", direction,
    paste0("    Mean deviation from trend : ", round(mean_v, 2), " mhl"),
    if (!is.na(sd_v))  paste0("    Std deviation in subgroup: ±", round(sd_v, 2), " mhl"),
    if (!is.na(skew_v)) paste0("    Distribution skewness    : ", round(skew_v, 2),
                                if (abs(skew_v) > 0.5) " (skewed)" else " (symmetric)"),
    ""
  )

  # Plain-language summary sentence
  cond_summary <- paste(conditions, collapse = "; and ")
  summary_sent <- paste0(
    "  SUMMARY: In years when ", cond_summary, ", ",
    if (detrend_flag) {
      paste0("wine production tends to be ",
             if (mean_v > 0) paste0(round(mean_v, 1), " mhl above") else paste0(abs(round(mean_v, 1)), " mhl below"),
             " the long-term trend.")
    } else {
      paste0("average wine production is ", round(mean_v, 1), " mhl.")
    }
  )

  c(lines, summary_sent, "")
}


# ==============================================================================
# MAIN PIPELINE: process all rules
# ==============================================================================

translate_rules_df <- function(drs_df,
                                region_label,
                                detrend_flag  = TRUE,
                                min_sup       = 0.15,
                                max_pval      = 0.10,
                                n_total       = NULL) {

  # Clean and filter
  rules <- drs_df %>%
    mutate(Ant_sup = as.numeric(Ant_sup),
           pvalue  = as.numeric(pvalue),
           Mean    = as.numeric(Mean)) %>%
    filter(Ant_sup < 1.0,              # drop global rule
           Ant_sup >= min_sup,
           pvalue  <= max_pval) %>%
    arrange(pvalue)

  cat('\nRules after filtering (sup ≥', min_sup, '| p ≤', max_pval, '):', nrow(rules), '\n\n')

  if (nrow(rules) == 0) {
    return(list(
      text = "No rules passed the filters. Try relaxing min_sup_filter or max_pval_filter.",
      csv  = data.frame()
    ))
  }

  # --- Plain text output -------------------------------------------------------
  header <- c(
    strrep("=", 60),
    paste0("CarenR Distribution Rules — ", region_label),
    paste0("Detrending : ", if (detrend_flag) "YES (rules describe deviations from trend)"
                            else "NO (rules describe raw production levels)"),
    paste0("Filters    : support ≥ ", min_sup * 100, "%  |  p-value ≤ ", max_pval),
    paste0("Rules shown: ", nrow(rules), " (sorted by p-value, most significant first)"),
    strrep("=", 60),
    "",
    "HOW TO READ THESE RULES:",
    "  Each rule identifies a climate subgroup — years sharing similar",
    "  conditions — where wine production is notably different from expected.",
    if (detrend_flag) {
      "  'Mean deviation' is the average distance from the long-term trend:"
    } else {
      "  'Mean' is the average wine production (mhl) in that subgroup:"
    },
    "    Positive → above-trend / higher-than-average production",
    "    Negative → below-trend / lower-than-average production",
    "  'Support' = fraction of years that match this rule's conditions.",
    strrep("=", 60),
    ""
  )

  rule_blocks <- unlist(lapply(seq_len(nrow(rules)), function(i) {
    translate_rule(i, rules, detrend_flag, n_total)
  }))

  all_text <- c(header, rule_blocks)

  # --- Structured CSV output ---------------------------------------------------
  csv_rows <- lapply(seq_len(nrow(rules)), function(i) {
    row   <- rules[i, ]
    conds <- translate_condition(as.character(row$Subgroup))
    data.frame(
      rule_rank      = i,
      support_pct    = round(as.numeric(row$Ant_sup) * 100, 1),
      n_years_approx = if (!is.null(n_total)) round(as.numeric(row$Ant_sup) * n_total) else NA,
      pvalue         = signif(as.numeric(row$pvalue), 3),
      mean_deviation = round(as.numeric(row$Mean), 3),
      stdev          = if ("Stdev" %in% names(row)) round(as.numeric(row$Stdev), 3) else NA,
      direction      = if (as.numeric(row$Mean) > 0) "above-trend" else "below-trend",
      n_conditions   = length(conds),
      condition_1    = if (length(conds) >= 1) conds[1] else NA,
      condition_2    = if (length(conds) >= 2) conds[2] else NA,
      condition_3    = if (length(conds) >= 3) conds[3] else NA,
      raw_subgroup   = as.character(row$Subgroup),
      stringsAsFactors = FALSE
    )
  })

  list(text = all_text, csv = bind_rows(csv_rows))
}


# ==============================================================================
# RUN
# ==============================================================================

# Approximate total number of observations (used for "≈N years" display)
n_obs <- tryCatch({
  path <- if (file == 1) 'data/dataPrep_cont_dataset_rdd.csv'
          else           'data/dataPrep_cont_dataset_rvv.csv'
  nrow(read.csv(path))
}, error = function(e) NULL)

result <- translate_rules_df(
  drs_df      = drs_raw,
  region_label= region,
  detrend_flag= detrend,
  min_sup     = min_sup_filter,
  max_pval    = max_pval_filter,
  n_total     = n_obs
)

# Print to console
cat(paste(result$text, collapse = "\n"))


# ==============================================================================
# SAVE OUTPUTS
# ==============================================================================

out_dir <- file.path('data', if (detrend) detrend_method else 'no_detrend')
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# Plain text
txt_file <- file.path(out_dir, paste0('rules_interpreted_', tolower(region), '.txt'))
writeLines(result$text, txt_file)
cat('\nSaved plain text:', txt_file, '\n')

# Structured CSV
csv_file <- file.path(out_dir, paste0('rules_interpreted_', tolower(region), '.csv'))
write.csv(result$csv, csv_file, row.names = FALSE)
cat('Saved structured CSV:', csv_file, '\n')

cat('\nDone.\n')


# ==============================================================================
# SECTION 2 — RIPPER (JRip) RULE INTERPRETER
# ==============================================================================
#
# Reads:  data/{detrend_method}/ripper_rules_structured_{region}.csv
# Writes: data/{detrend_method}/ripper_interpreted_{region}.txt
#         data/{detrend_method}/ripper_interpreted_{region}.csv
#
# RIPPER produces an ordered decision list where:
#   - Each rule's conditions are: (feature >= val) and (feature2 <= val2)
#   - The consequent is a production class: Low / Medium / High
#   - The default (last) rule covers all remaining observations
#   - Low/Medium/High are defined by quantile thresholds (stored in the CSV)
# ==============================================================================

# --- Condition decoder for JRip format ---------------------------------------
# Input: "(feature >= 0.82) and (feature2 <= 1.3)"  — raw JRip condition string
# Output: vector of plain-English condition phrases

decode_ripper_conditions <- function(cond_raw) {

  if (cond_raw == "(default)") {
    return(list(
      sentences = "(no conditions — applies to all remaining years)",
      features  = NA_character_
    ))
  }

  # Split on ") and (" — JRip uses lowercase "and" between parenthesised conditions
  # (?i) makes the match case-insensitive; needs perl = TRUE to be honoured.
  raw_parts <- strsplit(cond_raw, "(?i)\\)\\s+and\\s+\\(", perl = TRUE)[[1]]
  raw_parts <- gsub("^\\(|\\)$", "", trimws(raw_parts))

  parsed <- lapply(raw_parts, function(cond) {

    m <- regmatches(cond,
           regexec("^(.+?)\\s*(>=|<=|>|<|=)\\s*([0-9.-]+)$", trimws(cond)))[[1]]
    if (length(m) < 4) return(list(sentence = cond, feature = NA_character_))

    feat     <- trimws(m[2])
    op       <- m[3]
    value    <- m[4]

    feat_label <- decode_feature(feat)

    feat_parts <- regmatches(feat,
      regexec("^(.+)_(bb|fl|sm|hv)_(y0|y1)_([cba]\\d+)$", feat, perl = TRUE))[[1]]
    met_info <- if (length(feat_parts) > 1) metric_labels[[feat_parts[2]]] else NULL
    unit_str <- if (!is.null(met_info) && nchar(met_info$unit) > 0)
                  paste0(" ", met_info$unit) else ""

    op_sym <- switch(op, ">=" = "≥", "<=" = "≤", ">" = ">", "<" = "<", "=" = "=", op)

    list(sentence = paste0(feat_label, " ", op_sym, " ", value, unit_str),
         feature  = feat)
  })

  list(
    sentences = vapply(parsed, `[[`, character(1), "sentence", USE.NAMES = FALSE),
    features  = vapply(parsed, `[[`, character(1), "feature",  USE.NAMES = FALSE)
  )
}


# --- Single RIPPER rule → narrative block ------------------------------------

translate_ripper_rule <- function(i, rules_df) {

  row          <- rules_df[i, ]
  cond_raw     <- as.character(row$conditions_raw)
  pred_class   <- as.character(row$predicted_class)
  n_covered    <- as.numeric(row$n_covered)
  n_errors     <- as.numeric(row$n_errors)
  support_pct  <- as.numeric(row$support_pct)
  precision    <- as.numeric(row$precision_pct)
  thr_lm       <- as.numeric(row$threshold_low_med)
  thr_mh       <- as.numeric(row$threshold_med_high)
  is_detrended <- isTRUE(row$target_detrended)

  parsed     <- decode_ripper_conditions(cond_raw)
  conditions <- parsed$sentences
  features   <- parsed$features
  cond_block <- format_conditions_with_pathways(conditions, features)

  # Class label with threshold context
  class_desc <- switch(pred_class,
    "High"   = paste0("HIGH production",
                      if (!is.na(thr_mh)) paste0(" (above ", thr_mh,
                        if (is_detrended) " mhl above trend)" else " mhl)") else ""),
    "Low"    = paste0("LOW production",
                      if (!is.na(thr_lm)) paste0(" (below ", thr_lm,
                        if (is_detrended) " mhl below trend)" else " mhl)") else ""),
    "Medium" = paste0("MEDIUM production",
                      if (!is.na(thr_lm) && !is.na(thr_mh))
                        paste0(" (", thr_lm, " to ", thr_mh,
                               if (is_detrended) " mhl from trend)" else " mhl)") else ""),
    pred_class
  )

  lines <- c(
    strrep("-", 60),
    paste0("RULE ", i,
           if (!as.logical(row$is_default)) paste0("  |  Support: ", support_pct, "%",
             "  (", round(n_covered), " years)  |  Precision: ", precision, "%")
           else "  (DEFAULT — covers all remaining years)"),
    "",
    "  WHEN:",
    cond_block,
    "",
    paste0("  THEN wine production is:  ", class_desc),
    ""
  )

  cond_summary <- paste(conditions, collapse = "; and ")
  summary_sent <- paste0("  SUMMARY: In years when ", cond_summary,
                         ", wine production tends to be ", tolower(pred_class), ".")
  c(lines, summary_sent, "")
}


# --- RIPPER full pipeline ----------------------------------------------------

ripper_file <- file.path('data', if (detrend) detrend_method else 'no_detrend',
                         paste0('ripper_rules_structured_', tolower(region), '.csv'))

if (!file.exists(ripper_file)) {

  cat('\n[RIPPER] Structured rules file not found:', ripper_file, '\n')
  cat('  Run script 4 first to generate it.\n\n')

} else {

  ripper_rules <- read.csv(ripper_file, stringsAsFactors = FALSE)
  cat('\n[RIPPER] Loaded', nrow(ripper_rules), 'rules from', ripper_file, '\n')

  ripper_header <- c(
    strrep("=", 60),
    paste0("RIPPER (JRip) Rules — ", region),
    paste0("Detrending : ", if (detrend) paste0("YES (", detrend_method, ")") else "NO"),
    paste0("Target     : ", if (detrend) "Wine_mhl residuals (Low/Med/High relative to trend)"
                            else "Raw Wine_mhl (Low/Med/High by quantile)"),
    paste0("Rules      : ", nrow(ripper_rules), " (ordered — first match wins)"),
    strrep("=", 60), "",
    "HOW TO READ THESE RULES:",
    "  Rules are evaluated in order. For a given year, the FIRST rule",
    "  whose conditions are satisfied determines the predicted class.",
    "  The default rule (last) catches all years not covered above.",
    "  Precision = % of covered training years correctly classified.",
    strrep("=", 60), ""
  )

  ripper_blocks <- unlist(lapply(seq_len(nrow(ripper_rules)), translate_ripper_rule,
                                 rules_df = ripper_rules))

  ripper_text <- c(ripper_header, ripper_blocks)
  cat(paste(ripper_text, collapse = "\n"))

  # Save
  ripper_txt <- file.path(out_dir, paste0('ripper_interpreted_', tolower(region), '.txt'))
  writeLines(ripper_text, ripper_txt)
  cat('\nSaved RIPPER plain text:', ripper_txt, '\n')

  # Structured CSV
  ripper_csv_rows <- lapply(seq_len(nrow(ripper_rules)), function(i) {
    row   <- ripper_rules[i, ]
    conds <- decode_ripper_conditions(as.character(row$conditions_raw))
    data.frame(
      rule_rank       = i,
      predicted_class = row$predicted_class,
      support_pct     = row$support_pct,
      precision_pct   = row$precision_pct,
      n_covered       = row$n_covered,
      is_default      = row$is_default,
      n_conditions    = length(conds),
      condition_1     = if (length(conds) >= 1) conds[1] else NA,
      condition_2     = if (length(conds) >= 2) conds[2] else NA,
      condition_3     = if (length(conds) >= 3) conds[3] else NA,
      raw_conditions  = row$conditions_raw,
      stringsAsFactors = FALSE
    )
  })
  ripper_csv_file <- file.path(out_dir, paste0('ripper_interpreted_', tolower(region), '.csv'))
  write.csv(bind_rows(ripper_csv_rows), ripper_csv_file, row.names = FALSE)
  cat('Saved RIPPER structured CSV:', ripper_csv_file, '\n')
}


# ==============================================================================
# SECTION 3 — M5RULES INTERPRETER
# ==============================================================================
#
# Reads:  data/{detrend_method}/m5rules_rules_structured_{region}.csv
# Writes: data/{detrend_method}/m5rules_interpreted_{region}.txt
#         data/{detrend_method}/m5rules_interpreted_{region}.csv
#
# M5Rules produces an ordered set of piecewise linear rules where:
#   - Each rule's conditions use: feature <= val AND feature > val
#   - The consequent is a linear equation: Wine_mhl = c1*feat + c2*feat2 + intercept
#   - The default rule (no conditions) acts as a catch-all linear model
# ==============================================================================

# --- Condition decoder for M5Rules format ------------------------------------
# Input: "iaf_hv_y1_c10 <= 0.815 AND rf_fl_y1_a10 > 5.3"
# Output: vector of plain-English condition phrases

decode_m5_conditions <- function(cond_raw) {

  if (cond_raw == "(default)") {
    return(list(
      sentences = "(no conditions — applies to all remaining years)",
      features  = NA_character_
    ))
  }

  raw_parts <- trimws(strsplit(cond_raw, "\\s+AND\\s+")[[1]])

  parsed <- lapply(raw_parts, function(cond) {

    m <- regmatches(cond,
           regexec("^(.+?)\\s*(>=|<=|>|<)\\s*([0-9.-]+)$", trimws(cond)))[[1]]
    if (length(m) < 4) return(list(sentence = cond, feature = NA_character_))

    feat     <- trimws(m[2])
    op       <- m[3]
    value    <- m[4]

    feat_label <- decode_feature(feat)

    feat_parts <- regmatches(feat,
      regexec("^(.+)_(bb|fl|sm|hv)_(y0|y1)_([cba]\\d+)$", feat, perl = TRUE))[[1]]
    met_info <- if (length(feat_parts) > 1) metric_labels[[feat_parts[2]]] else NULL
    unit_str <- if (!is.null(met_info) && nchar(met_info$unit) > 0)
                  paste0(" ", met_info$unit) else ""

    op_sym <- switch(op, ">=" = "≥", "<=" = "≤", ">" = ">", "<" = "<", op)

    list(sentence = paste0(feat_label, " ", op_sym, " ", value, unit_str),
         feature  = feat)
  })

  list(
    sentences = vapply(parsed, `[[`, character(1), "sentence", USE.NAMES = FALSE),
    features  = vapply(parsed, `[[`, character(1), "feature",  USE.NAMES = FALSE)
  )
}


# --- Linear model decoder ----------------------------------------------------
# Input:  "Wine_mhl = 0.41 * tm_sm_y1_c20 + (-0.28) * swa_hv_y0_a20 + 142.3"
# Output: multi-line human-readable equation

decode_m5_linear_model <- function(lm_raw) {

  # Strip "Wine_mhl = " prefix
  lm_str <- trimws(gsub("^.*Wine_mhl\\s*=\\s*", "", lm_raw))

  # Split into additive terms on " + "
  terms <- trimws(strsplit(lm_str, "\\s*\\+\\s*")[[1]])
  terms <- terms[nchar(terms) > 0]

  decoded <- vapply(terms, function(term) {

    # Match: optional-paren coef * feature_name
    m <- regmatches(term,
           regexec("^\\(?(-?[0-9.]+)\\)?\\s*\\*\\s*(.+)$", trimws(term)))[[1]]

    if (length(m) >= 3) {
      coef  <- m[2]
      feat  <- trimws(m[3])
      label <- decode_feature(feat)
      feat_parts <- regmatches(feat,
        regexec("^(.+)_(bb|fl|sm|hv)_(y0|y1)_([cba]\\d+)$", feat, perl = TRUE))[[1]]
      met_info <- if (length(feat_parts) > 1) metric_labels[[feat_parts[2]]] else NULL
      unit_str <- if (!is.null(met_info) && nchar(met_info$unit) > 0)
                    paste0(" [", met_info$unit, "]") else ""
      paste0(coef, " × ", label, unit_str)
    } else {
      paste0(trimws(term), "  (intercept)")
    }

  }, character(1), USE.NAMES = FALSE)

  paste(decoded, collapse = "\n             + ")
}


# --- Single M5Rules rule → narrative block -----------------------------------

translate_m5_rule <- function(i, rules_df, detrend_flag) {

  row        <- rules_df[i, ]
  cond_raw   <- as.character(row$conditions_raw)
  lm_raw     <- as.character(row$linear_model_raw)
  n_inst     <- as.numeric(row$n_instances)
  err_pct    <- as.numeric(row$error_pct)

  parsed     <- decode_m5_conditions(cond_raw)
  conditions <- parsed$sentences
  features   <- parsed$features
  cond_block <- format_conditions_with_pathways(conditions, features)
  lm_decoded <- decode_m5_linear_model(lm_raw)

  target_label <- if (detrend_flag) "Wine_mhl residual (mhl above/below trend)"
                  else              "Wine_mhl (mhl)"

  lines <- c(
    strrep("-", 60),
    paste0("RULE ", i,
           if (!is.na(n_inst)) paste0("  |  Coverage: ", n_inst, " training years") else "",
           if (!is.na(err_pct)) paste0("  |  Error: ", round(err_pct, 1), "%") else ""),
    "",
    "  WHEN:",
    cond_block,
    "",
    paste0("  THEN  ", target_label, "  ="),
    paste0("             ", lm_decoded),
    ""
  )

  cond_summary <- paste(conditions, collapse = "; and ")
  summary_sent <- paste0("  SUMMARY: In years when ", cond_summary,
                         ", production is estimated by the linear model above.")
  c(lines, summary_sent, "")
}


# --- M5Rules full pipeline ---------------------------------------------------

m5_struct_file <- file.path('data', if (detrend) detrend_method else 'no_detrend',
                             paste0('m5rules_rules_structured_', tolower(region), '.csv'))

if (!file.exists(m5_struct_file)) {

  cat('\n[M5Rules] Structured rules file not found:', m5_struct_file, '\n')
  cat('  Run script 5 first to generate it.\n\n')

} else {

  m5_rules <- read.csv(m5_struct_file, stringsAsFactors = FALSE)
  cat('\n[M5Rules] Loaded', nrow(m5_rules), 'rules from', m5_struct_file, '\n')

  m5_header <- c(
    strrep("=", 60),
    paste0("M5Rules — ", region),
    paste0("Detrending : ", if (detrend) paste0("YES (", detrend_method, ")") else "NO"),
    paste0("Target     : ", if (detrend) "Wine_mhl residuals (deviation from trend)"
                            else "Raw Wine_mhl (mhl)"),
    paste0("Rules      : ", nrow(m5_rules), " (ordered — first match wins)"),
    strrep("=", 60), "",
    "HOW TO READ THESE RULES:",
    "  Rules are evaluated in order. Each rule defines a climate subgroup",
    "  AND a local linear model active within it.",
    "  'x' coefficients indicate direction and magnitude of each feature's",
    "  contribution within that subgroup — positive = more production,",
    "  negative = less production.",
    strrep("=", 60), ""
  )

  m5_blocks <- unlist(lapply(seq_len(nrow(m5_rules)), translate_m5_rule,
                             rules_df = m5_rules, detrend_flag = detrend))

  m5_text <- c(m5_header, m5_blocks)
  cat(paste(m5_text, collapse = "\n"))

  # Save plain text
  m5_txt <- file.path(out_dir, paste0('m5rules_interpreted_', tolower(region), '.txt'))
  writeLines(m5_text, m5_txt)
  cat('\nSaved M5Rules plain text:', m5_txt, '\n')

  # Structured CSV
  m5_csv_rows <- lapply(seq_len(nrow(m5_rules)), function(i) {
    row   <- m5_rules[i, ]
    conds <- decode_m5_conditions(as.character(row$conditions_raw))
    data.frame(
      rule_rank        = i,
      n_instances      = row$n_instances,
      error_pct        = row$error_pct,
      n_conditions     = length(conds),
      condition_1      = if (length(conds) >= 1) conds[1] else NA,
      condition_2      = if (length(conds) >= 2) conds[2] else NA,
      condition_3      = if (length(conds) >= 3) conds[3] else NA,
      linear_model_raw = row$linear_model_raw,
      raw_conditions   = row$conditions_raw,
      stringsAsFactors = FALSE
    )
  })
  m5_csv_file <- file.path(out_dir, paste0('m5rules_interpreted_', tolower(region), '.csv'))
  write.csv(bind_rows(m5_csv_rows), m5_csv_file, row.names = FALSE)
  cat('Saved M5Rules structured CSV:', m5_csv_file, '\n')
}

cat('\nAll interpreters complete.\n')


# ==============================================================================
# UTILITY: translate a single feature name (useful for quick lookups)
# ==============================================================================
# Example usage (run interactively):
#   decode_feature("iaf_hv_y1_c10")
#   decode_feature("swa_hv_y0_a20")
#   decode_feature("days.rf.above.1mm_sm_y1_b20")
