# 10_rule_overlap_analysis.R
# Computes pairwise feature overlap and unique-condition statistics
# for the CarenR rule sets (RDD and RVV).
# Results reported in Section 4.7 of the thesis.

library(dplyr)
library(stringr)

DATA_DIR <- "data/linear"

# ── Helper: extract base feature names from raw subgroup string ────────────────
# Raw format: "feat1=[lo|hi)|feat2=[lo|hi)|..."
# We split on | and keep only tokens that start with a letter (feature names).
extract_features <- function(raw_str) {
  if (is.na(raw_str) || raw_str == "") return(character(0))
  tokens <- str_split(raw_str, "\\|")[[1]]
  # Remove bin interval parts (=[ ... )
  names <- str_remove(tokens, "=\\[.*")
  # Keep only tokens that start with a letter
  names <- names[str_detect(names, "^[a-zA-Z]")]
  unique(trimws(names))
}

# ── Pairwise overlap ───────────────────────────────────────────────────────────
compute_overlap <- function(feature_list) {
  n <- length(feature_list)
  pairs <- combn(n, 2)
  shared <- apply(pairs, 2, function(idx) {
    length(intersect(feature_list[[idx[1]]], feature_list[[idx[2]]])) > 0
  })
  list(
    n_rules      = n,
    total_pairs  = ncol(pairs),
    shared_pairs = sum(shared),
    pct_shared   = round(100 * mean(shared), 1)
  )
}

# ── Unique-condition analysis ──────────────────────────────────────────────────
# A "condition" here is the full feature=bin string.
# A rule has a unique condition if at least one of its condition-bin strings
# does not appear in any other rule.
extract_conditions <- function(raw_str) {
  if (is.na(raw_str) || raw_str == "") return(character(0))
  tokens <- str_split(raw_str, "\\|")[[1]]
  # Keep only tokens that look like a full condition (contain '=')
  tokens[str_detect(tokens, "=")]
}

compute_unique <- function(condition_list) {
  all_conditions <- unlist(condition_list)
  condition_counts <- table(all_conditions)
  n <- length(condition_list)
  has_unique <- sapply(condition_list, function(conds) {
    any(condition_counts[conds] == 1)
  })
  list(
    n_rules       = n,
    rules_unique  = sum(has_unique),
    pct_unique    = round(100 * mean(has_unique), 1)
  )
}

# ── Run for both regions ───────────────────────────────────────────────────────
regions <- list(
  RDD = list(file = "rules_interpreted_rdd.csv", col = "conditions_raw"),
  RVV = list(file = "rules_interpreted_rvv.csv", col = "raw_subgroup")
)

for (region in names(regions)) {
  cfg <- regions[[region]]
  df  <- read.csv(file.path(DATA_DIR, cfg$file), stringsAsFactors = FALSE)
  raw <- df[[cfg$col]]

  feat_list <- lapply(raw, extract_features)
  cond_list <- lapply(raw, extract_conditions)

  ov  <- compute_overlap(feat_list)

  # Distinct feature-name combinations (ignoring bin intervals)
  feat_sets      <- lapply(feat_list, function(f) paste(sort(f), collapse="|"))
  n_distinct     <- length(unique(feat_sets))

  # Subset rules: rules whose full condition set is a strict subset of another rule
  cond_sets <- lapply(cond_list, function(cs) paste(sort(cs), collapse="|"))
  is_subset <- sapply(seq_along(cond_list), function(i) {
    any(sapply(seq_along(cond_list), function(j) {
      if (i == j) return(FALSE)
      all(cond_list[[i]] %in% cond_list[[j]]) && length(cond_list[[i]]) < length(cond_list[[j]])
    }))
  })
  n_subsets <- sum(is_subset)

  cat(sprintf("\n=== %s ===\n", region))
  cat(sprintf("Rules: %d\n", ov$n_rules))
  cat(sprintf("Pairwise overlap: %d / %d pairs share ≥1 feature (%.1f%%)\n",
              ov$shared_pairs, ov$total_pairs, ov$pct_shared))
  cat(sprintf("Distinct feature-name sets: %d / %d\n", n_distinct, ov$n_rules))
  cat(sprintf("Condition-subset rules: %d / %d (%.1f%%)\n",
              n_subsets, ov$n_rules, 100*n_subsets/ov$n_rules))
}
