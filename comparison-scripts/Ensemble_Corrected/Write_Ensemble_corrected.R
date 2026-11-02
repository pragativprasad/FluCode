# Corrected ensemble for 2009, PAN1 and PAN2
# (replaces Write_Ensemble_2009.R, Write_Ensemble_EPI1.R, Write_Ensemble_EPI2.R).
#
# Fixes
#  1. Ensemble interval: NOT a bug fix. The paper defines the ensemble as the mean of model
#     means, 2.5th and 97.5th percentiles ("mean_of_quantiles"), which is the default and
#     reproduces the published numbers. A linear opinion pool ("linear_pool", built from the
#     sm.* summary rows, which are unaffected by the truncated bin grid) is written alongside
#     as a sensitivity analysis, at the suggestion of an external reader.
#  2. Units: averted hospitalisations/deaths used Cml*100000; a rate per 100,000 times the
#     US population needs * 3,068 (306.8e6 / 1e5).
#  3. No setwd(); paths resolve from the repo root. No reshape::melt namespace collision.
#
# Run from inside the repository (any subfolder). Outputs: model-forecasts/LinearPool-Ensemble/
# File schemas match the originals in MeanBased-Ensemble/ so the figure code needs no changes
# beyond the input folder.

local({
  d <- getwd()
  cand <- c(file.path(d, "helpers.R"),
            file.path(d, "Ensemble_Corrected", "helpers.R"),
            file.path(d, "comparison-scripts", "Ensemble_Corrected", "helpers.R"))
  source(cand[file.exists(cand)][1])
})

# Methods to run. Both are written by default, to separate folders (see ensemble_dirs in
# helpers.R). The paper's method is the default for all downstream scripts.
methods_to_run <- c("mean_of_quantiles", "linear_pool")

outcomes <- c(symillness = "SymIllness", hospitalization = "Hosp", death = "Deaths")

compute_metrics <- function(dat, an, outcome) {
  ids <- an$ids
  base_id <- ids[1]
  lab <- an$label
  mean_of <- function(measure) {
    dat |> filter(stat == "sm.mean", measure == !!measure) |> select(team, scn, value)
  }

  # Cumulative burden: ENS = mean of team means (= pooled mean)
  cml <- mean_of("Cml")
  cml <- bind_rows(cml, cml |> group_by(scn) |> summarise(value = mean(value), .groups = "drop") |>
                     mutate(team = "ENS"))
  base <- cml |> filter(scn == base_id) |> select(team, base = value)
  rel <- cml |> filter(scn != base_id) |> left_join(base, by = "team") |>
    mutate(frac = (base - value) / base)

  scale_factor <- if (outcome == "SymIllness") (1 / 100) * US_POP else US_POP_100K
  averted_n <- rel |> mutate(Reduction = frac * base * scale_factor, Output = "Averted Number")
  averted_p <- rel |> mutate(Reduction = frac * 100, Output = "Averted Percent")

  # Peak magnitude reduction: ENS = mean of team reductions (as in the original)
  pk <- mean_of("Peak")
  pk_base <- pk |> filter(scn == base_id) |> select(team, base = value)
  pk_red <- pk |> filter(scn != base_id) |> left_join(pk_base, by = "team") |>
    mutate(frac = (base - value) / base)
  pk_ens <- pk_red |> group_by(scn) |> summarise(frac = mean(frac), .groups = "drop") |>
    mutate(team = "ENS")
  mag <- bind_rows(pk_red |> select(team, scn, frac), pk_ens) |>
    mutate(Reduction = frac * 100, Output = "Magnitude Reduction")

  # Peak delay (weeks) from the mean trajectory; teams only
  pw <- dat |> filter(stat == "sm.mean", !measure %in% c("Cml", "Peak")) |>
    mutate(week = as.integer(measure)) |>
    group_by(team, scn) |> summarise(peak = week[which.max(value)], .groups = "drop")
  pw0 <- pw |> filter(scn == base_id) |> select(team, peak0 = peak)
  delay <- pw |> filter(scn != base_id) |> left_join(pw0, by = "team") |>
    mutate(Reduction = peak - peak0, Output = "Peak Delay")

  bind_rows(averted_n, averted_p, mag, delay) |>
    group_by(team, Output) |>
    mutate(Rank = if (first(Output) == "Peak Delay") 0 else rank(-Reduction)) |>
    ungroup() |>
    transmute(Team = team, Scenario = lab(scn), Reduction, Rank, Output)
}

# "mean (2.5%, 97.5%)" table for base-scenario cumulative burden; ENS from the linear pool
compute_table <- function(dat, pooled, an, outcome) {
  pct <- if (outcome == "SymIllness") "%" else ""
  base_id <- an$ids[1]
  all_cml <- bind_rows(dat |> filter(scn == base_id, measure == "Cml") |> select(team, stat, value),
                       pooled |> filter(scn == base_id, measure == "Cml") |>
                         transmute(team = "ENS", stat, value))
  all_cml |>
    filter(stat %in% c("sm.mean", "sm.perc2p5", "sm.perc97p5")) |>
    pivot_wider(names_from = stat, values_from = value) |>
    mutate(!!outcome := paste0(round(sm.mean), pct, " (", round(sm.perc2p5), pct, ", ",
                               round(sm.perc97p5), pct, ")"),
           team = factor(team, levels = ens_levels)) |>
    arrange(team) |>
    select(team, all_of(outcome))
}

# Wide weekly trajectories (team, week, <id>_mean, <id>_perc2p5, <id>_perc97p5, Date)
compute_trajectory <- function(dat, pooled, an) {
  bind_rows(dat, pooled |> mutate(team = "ENS")) |>
    filter(!measure %in% c("Cml", "Peak"),
           stat %in% c("sm.mean", "sm.perc2p5", "sm.perc97p5")) |>
    mutate(week = as.integer(measure), stat = sub("^sm\\.", "", stat),
           col = paste(scn, stat, sep = "_")) |>
    select(team, week, col, value) |>
    pivot_wider(names_from = col, values_from = value) |>
    mutate(Date = an$start + week * 7,
           team = factor(team, levels = c("COL", "COL2", "IMP", "NEU3", "UTA", "UVA", "ENS"))) |>
    arrange(team, week) |>
    mutate(team = as.character(team)) |>
    select(team, week, starts_with(an$ids[1]), Date, everything())
}

# Base-scenario age-specific burden, one row per team x age group (original base_age schema)
compute_base_age <- function(dat_all, an) {
  dat_all |>
    filter(scn == an$ids[1], Agegroup != "Overall", stat == "sm.mean") |>
    mutate(team = ifelse(team == "NEU3", an$neu_age_label, team)) |>
    transmute(Agegroup, Bin_cml = stat, Cml = value, team)
}

# Base-scenario age-specific mean and pooled 95% interval, same schema as the
# Visualization/*_RL01|HP01|HP13.csv files, with a pooled ENS added.
# Symptomatic illness is a fraction (Cml/100); deaths are per 100,000.
compute_age_interval <- function(dat_all, pooled_all, an, outcome) {
  base_id <- an$ids[1]
  div <- if (outcome == "SymIllness") 100 else 1
  models <- dat_all |> filter(scn == base_id, measure == "Cml") |>
    transmute(team, agegroup, stat, value)
  ens <- pooled_all |> filter(scn == base_id, measure == "Cml") |>
    transmute(team = "ENS", agegroup, stat, value)
  bind_rows(models, ens) |>
    filter(stat %in% c("sm.mean", "sm.perc2p5", "sm.perc97p5")) |>
    pivot_wider(names_from = stat, values_from = value) |>
    mutate(scenario = paste0(base_id, "_"),
           team = ifelse(team == "NEU3", an$neu_age_label, team)) |>
    transmute(scenario, team, agegroup, cml = sm.mean / div,
              lower = sm.perc2p5 / div, upper = sm.perc97p5 / div)
}

for (method in methods_to_run) {
 out_path <- file.path(forecast_path, ensemble_dirs[[method]])
 dir.create(out_path, showWarnings = FALSE)
 for (an in analyses) {
  for (nm in names(outcomes)) {
    outcome <- outcomes[[nm]]
    message(method, " / ", an$tag, ": ", outcome)
    dat_all <- read_all(an, outcome)
    dat <- dat_all |> filter(Agegroup == "Overall")
    pooled_all <- pool_ensemble(dat_all |> filter(scn == an$ids[1] | Agegroup == "Overall"), method)
    pooled <- pooled_all |> filter(agegroup == "Overall")

    pre <- file.path(out_path, paste0(an$tag, "_", nm))
    write.csv(compute_trajectory(dat, pooled, an), paste0(pre, "_ensemble.csv"))
    write.csv(compute_metrics(dat, an, outcome), paste0(pre, "_metrics.csv"))
    write.csv(compute_table(dat, pooled, an, outcome), paste0(pre, "_table.csv"))
    write.csv(pooled |> mutate(scenario = an$label(scn)) |> select(scenario, measure, stat, value),
              paste0(pre, "_pooled_summary.csv"), row.names = FALSE)
    if (outcome %in% c("SymIllness", "Deaths")) {
      write.csv(compute_base_age(dat_all, an), paste0(pre, "_base_age.csv"))
      write.csv(compute_age_interval(dat_all, pooled_all, an, outcome),
                paste0(pre, "_age_interval.csv"))
    }
  }
}
 }
