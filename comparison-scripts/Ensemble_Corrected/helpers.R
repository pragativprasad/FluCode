# Shared helpers for the corrected ensemble pipeline (2009, PAN1, PAN2).
# Depends only on dplyr and tidyr; no setwd() and no reshape/plyr (avoids the melt() collision).

library(dplyr)
library(tidyr)

# Guard against masking when plyr/data.table/magrittr were attached earlier in the session
# (the original scripts load all of them). Pin the dplyr/tidyr versions.
for (fn in c("arrange", "count", "desc", "mutate", "rename", "summarise", "summarize",
             "first", "last", "between", "filter", "select", "group_by", "transmute"))
  assign(fn, getExportedValue("dplyr", fn), envir = globalenv())
assign("extract", getExportedValue("tidyr", "extract"), envir = globalenv())

find_repo_root <- function(start = getwd()) {
  d <- normalizePath(start)
  while (!dir.exists(file.path(d, "model-forecasts"))) {
    parent <- dirname(d)
    if (parent == d) stop("Run from inside the FluCode repository (model-forecasts/ not found).")
    d <- parent
  }
  d
}

repo_root <- find_repo_root()
forecast_path <- file.path(repo_root, "model-forecasts")

# Ensemble method -------------------------------------------------------------
# "mean_of_quantiles": the method described in the paper (mean of model means, 2.5th and 97.5th
#                      percentiles across models). DEFAULT.
# "linear_pool":       equal-weight mixture of the model distributions (sensitivity analysis).
# Select with options(flucode.ensemble_method = "linear_pool") before sourcing a script.
ensemble_dirs <- c(mean_of_quantiles = "MeanOfQuantiles-Corrected", linear_pool = "LinearPool-Ensemble")
ensemble_method <- match.arg(getOption("flucode.ensemble_method", "mean_of_quantiles"),
                             names(ensemble_dirs))
out_path <- file.path(forecast_path, ensemble_dirs[[ensemble_method]])

# US population in units of 100,000. Rates per 100,000 * 3,068 = national counts.
US_POP <- 306.8e6
US_POP_100K <- US_POP / 1e5

# Analyses ---------------------------------------------------------------------
# folders: COL, NEU, UTA, UVA, IMP, COL2 (in that order)
analyses <- list(
  `2009` = list(
    tag = "2009", ids = sprintf("RL%02d", 1:8), start = as.Date("2020-06-24"),
    folders = c("Columbia/COL/RL/", "Northeastern/RL3/", "UTAustin/", "UVirginia/",
                "Imperial/", "Columbia/COL2/RL/"),
    label = function(id) sub("^RL0", "2009-", id), neu_age_label = "NEU3"),
  PAN1 = list(
    tag = "PAN1", ids = sprintf("HP%02d", 1:12), start = as.Date("2020-08-25"),
    folders = c("Columbia/COL/EPI1/", "Northeastern/EPI1/", "UTAustin/", "UVirginia/",
                "Imperial/", "Columbia/COL2/EPI1/"),
    label = identity, neu_age_label = "NEU"),
  PAN2 = list(
    tag = "PAN2", ids = sprintf("HP%02d", 13:24), start = as.Date("2020-08-25"),
    folders = c("Columbia/COL/EPI2/", "Northeastern/EPI2/", "UTAustin/", "UVirginia/",
                "Imperial/", "Columbia/COL2/EPI2/"),
    label = identity, neu_age_label = "NEU")
)

# team: file suffix; label: name used in outputs; sep: separator style in file names
teams <- tibble::tribble(
  ~team,  ~label, ~sep, ~death_word,
  "COL",  "COL",  "_",  "Deaths",
  "NEU",  "NEU3", "_",  "Deaths",
  "UTA",  "UTA",  "_",  "Deaths",
  "UVA",  "UVA",  "_",  "Deaths",
  "IMP",  "IMP",  "",   "Death",
  "COL2", "COL2", "_",  "Deaths"
)
ens_levels <- c("ENS", "UVA", "UTA", "NEU3", "IMP", "COL2", "COL")

stat_probs <- c(sm.perc2p5 = 0.025, sm.perc5 = 0.05, sm.perc25 = 0.25, sm.median = 0.5,
                sm.perc75 = 0.75, sm.perc95 = 0.95, sm.perc97p5 = 0.975)

forecast_file <- function(an, i, id, outcome) {
  t <- teams[i, ]
  id_f <- if (t$team == "UVA") sub("^(RL|HP)", "\\1_", id) else id
  word <- if (outcome == "Deaths") t$death_word else outcome
  file.path(forecast_path, an$folders[i], paste0(id_f, t$sep, word, t$sep, t$team, ".csv"))
}

# Summary (sm.*) rows for US National. Overall keeps all measures (Cml, Peak, weeks 1-52);
# age groups keep Cml only (all that the age-specific figures need).
# Columns: team, scn, agegroup (raw label), stat, measure, value
read_summary <- function(an, i, id, outcome) {
  f <- forecast_file(an, i, id, outcome)
  if (!file.exists(f)) stop("Missing file: ", f)
  s <- read.csv(f)
  names(s)[names(s) == "cml"] <- "Cml"
  names(s)[names(s) == "PeakMagnitude"] <- "Peak.Magnitude"
  if ("Location" %in% names(s)) s <- s[s$Location == "US National", ]
  s <- s[grepl("^sm\\.", s$Bin), ]
  ov <- s[s$Agegroup == "Overall", ] |>
    select(Agegroup, stat = Bin, Cml, Peak = Peak.Magnitude, starts_with("Week")) |>
    pivot_longer(-c(Agegroup, stat), names_to = "measure", values_to = "value")
  ag <- s[s$Agegroup != "Overall", ] |>
    select(Agegroup, stat = Bin, value = Cml) |>
    mutate(measure = "Cml")
  bind_rows(ov, ag) |>
    mutate(measure = sub("^Week", "", measure), team = teams$label[i], scn = id)
}

read_all <- function(an, outcome) {
  grid <- expand.grid(i = seq_len(nrow(teams)), id = an$ids, stringsAsFactors = FALSE)
  bind_rows(Map(function(i, id) read_summary(an, i, id, outcome), grid$i, grid$id)) |>
    mutate(agegroup = sub(" yr$", " years", Agegroup))
}

# Linear opinion pool -----------------------------------------------------------
# Q: matrix (models x 7 quantiles, ordered as stat_probs). Each model's CDF is the
# piecewise-linear interpolation through its quantiles, with each tail extended by one
# segment width (0.025 of mass) and floored at 0. The pooled CDF is the equal-weight
# average of the model CDFs; pooled quantiles are its inverse. Quantiles are sorted first
# so non-monotone inputs (e.g. 2.5th > 5th percentile) cannot break the interpolation.
linear_pool_quantiles <- function(Q, probs_out = stat_probs) {
  Q <- Q[stats::complete.cases(Q), , drop = FALSE]
  if (nrow(Q) == 0) return(rep(NA_real_, length(probs_out)))
  Q <- t(apply(Q, 1, sort))
  knots_p <- c(0, stat_probs, 1)
  Qx <- cbind(pmax(0, Q[, 1] - (Q[, 2] - Q[, 1])), Q, Q[, 7] + (Q[, 7] - Q[, 6]))
  grid <- sort(unique(as.vector(Qx)))
  if (length(grid) == 1) return(rep(grid, length(probs_out)))
  model_cdf <- function(x) {
    if (length(unique(x)) == 1) return(as.numeric(grid >= x[1]))  # point mass
    stats::approx(x, knots_p, xout = grid, ties = max, rule = 2)$y
  }
  F <- rowMeans(vapply(seq_len(nrow(Qx)), function(m) model_cdf(Qx[m, ]),
                       numeric(length(grid))))
  stats::approx(F, grid, xout = probs_out, ties = min, rule = 2)$y
}

# Summarise across teams for every (scn, agegroup, measure), using the selected method.
# Mean = average of model means (identical under both methods). Quantile stats are either
# averaged across models (paper's method) or taken from the linear pool.
pool_ensemble <- function(dat, method = ensemble_method) {
  dat |>
    group_by(scn, agegroup, measure) |>
    group_modify(function(d, key) {
      w <- d |> select(team, stat, value) |> pivot_wider(names_from = stat, values_from = value)
      if (method == "mean_of_quantiles") {
        stats_out <- c("sm.mean", names(stat_probs))
        return(tibble(stat = stats_out, value = vapply(stats_out, function(s) mean(w[[s]]), numeric(1))))
      }
      Q <- as.matrix(w[, names(stat_probs)])
      tibble(stat = c("sm.mean", names(stat_probs)),
             value = c(mean(w$sm.mean, na.rm = TRUE), linear_pool_quantiles(Q)))
    }) |>
    ungroup()
}
