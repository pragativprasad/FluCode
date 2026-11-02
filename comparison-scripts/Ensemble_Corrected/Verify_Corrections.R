# Compares corrected outputs with the originals (MeanBased-Ensemble, Visualization/*.csv).
# Expected (mean_of_quantiles: ENS intervals identical to the originals; linear_pool: wider):
#  - metrics: everything identical except Averted Number for Hosp/Deaths, which is smaller by
#    100000/3068 = 32.595 (SymIllness unchanged); ranks identical
#  - tables: model rows identical; ENS interval changes (mean unchanged)
#  - ensemble trajectories: team rows and ENS means identical; ENS bands change
#  - base_age: identical
#  - age_interval: model rows identical to Visualization CSVs; ENS rows are new (pooled)

local({
  d <- getwd()
  cand <- c(file.path(d, "helpers.R"), file.path(d, "Ensemble_Corrected", "helpers.R"),
            file.path(d, "comparison-scripts", "Ensemble_Corrected", "helpers.R"))
  source(cand[file.exists(cand)][1])
})
old_dir <- file.path(forecast_path, "MeanBased-Ensemble")
vis_dir <- file.path(repo_root, "comparison-scripts", "Visualization")
nms <- c("symillness", "hospitalization", "death")

cat("== metrics\n")
for (tag in names(analyses)) for (nm in nms) {
  old <- read.csv(file.path(old_dir, paste0(analyses[[tag]]$tag, "_", nm, "_metrics.csv")))[-1]
  new <- read.csv(file.path(out_path, paste0(analyses[[tag]]$tag, "_", nm, "_metrics.csv")))
  m <- merge(old, new, by = c("Team", "Scenario", "Output"), suffixes = c(".old", ".new"))
  res <- sapply(split(m, m$Output), function(d) c(
    n = nrow(d),
    max_rel_diff = max(abs(d$Reduction.new - d$Reduction.old) / pmax(abs(d$Reduction.old), 1e-12), na.rm = TRUE),
    rank_mismatch = sum(d$Rank.old != d$Rank.new),
    median_ratio = median(d$Reduction.old / d$Reduction.new, na.rm = TRUE)))
  cat("\n", tag, nm, " (rows old/new:", nrow(old), "/", nrow(new), ")\n")
  print(round(t(res), 6))
  # Original Write_Ensemble_EPI1.R computes Cml11/Cml12 before updating `cml`, so the
  # team-level Averted Percent/Number for HP11 and HP12 in PAN1 are shifted by one scenario.
  bad <- m[m$Output == "Averted Percent" & abs(m$Reduction.old - m$Reduction.new) > 1e-6, ]
  if (nrow(bad)) cat("  Averted Percent differs (original script bug) in scenarios:",
                     paste(sort(unique(bad$Scenario)), collapse = ", "), "\n")
}

cat("\n== tables: model rows changed?\n")
for (tag in names(analyses)) for (nm in nms) {
  f <- paste0(analyses[[tag]]$tag, "_", nm, "_table.csv")
  old <- read.csv(file.path(old_dir, f))[-1]; new <- read.csv(file.path(out_path, f))
  mm <- merge(old, new, by = "team", suffixes = c(".old", ".new"))
  col <- names(old)[2]
  cat(tag, nm, "model rows differing:",
      sum(mm[[paste0(col, ".old")]] != mm[[paste0(col, ".new")]] & mm$team != "ENS"),
      "| ENS old:", mm[[paste0(col, ".old")]][mm$team == "ENS"],
      "new:", mm[[paste0(col, ".new")]][mm$team == "ENS"], "\n")
}

cat("\n== trajectories: model + ENS mean differences, ENS band width ratio\n")
for (tag in names(analyses)) {
  old <- read.csv(file.path(old_dir, paste0(analyses[[tag]]$tag, "_symillness_ensemble.csv")))[-1]
  new <- read.csv(file.path(out_path, paste0(analyses[[tag]]$tag, "_symillness_ensemble.csv")))[-1]
  mm <- merge(old, new, by = c("team", "week"), suffixes = c(".o", ".n"))
  id <- analyses[[tag]]$ids
  mean_cols <- paste0(id, "_mean")
  dm <- max(abs(unlist(mapply(function(a) mm[[paste0(a, ".o")]] - mm[[paste0(a, ".n")]], mean_cols))))
  w <- function(s) with(mm[mm$team == "ENS", ], (get(paste0(id[1], "_perc97p5.", s)) - get(paste0(id[1], "_perc2p5.", s))))
  cat(tag, ": max |mean diff| =", signif(dm, 3), "; ENS base band width new/old (median) =",
      round(median(w("n") / w("o"), na.rm = TRUE), 2), "\n")
}

cat("\n== base_age identical to original?\n")
for (tag in c("PAN1", "PAN2")) for (nm in c("symillness", "death")) {
  f <- paste0(tag, "_", nm, "_base_age.csv")
  old <- read.csv(file.path(old_dir, f))[-1]; new <- read.csv(file.path(out_path, f))[-1]
  mm <- merge(old, new, by = c("team", "Agegroup"))
  cat(tag, nm, ": rows", nrow(old), "/", nrow(new), "; max diff", max(abs(mm$Cml.x - mm$Cml.y)), "\n")
}

cat("\n== age_interval model rows vs Visualization CSVs\n")
vis <- c("2009_symillness" = "SympIllnesses_2009_RL01", "2009_death" = "Deaths_2009_RL01",
         "PAN1_symillness" = "SympIllnesses_EPI1_HP01", "PAN1_death" = "Deaths_EPI1_HP01",
         "PAN2_symillness" = "SympIllnesses_EPI2_HP13", "PAN2_death" = "Deaths_EPI2_HP13")
for (k in names(vis)) {
  old <- read.csv(file.path(vis_dir, paste0(vis[[k]], ".csv")))[-1]
  new <- read.csv(file.path(out_path, paste0(k, "_age_interval.csv")))
  mm <- merge(old, new, by = c("team", "agegroup"))
  cat(k, ": matched", nrow(mm), "rows; max diff cml/lower/upper =",
      signif(max(abs(mm$cml.x - mm$cml.y)), 3), signif(max(abs(mm$lower.x - mm$lower.y)), 3),
      signif(max(abs(mm$upper.x - mm$upper.y)), 3), "\n")
}
