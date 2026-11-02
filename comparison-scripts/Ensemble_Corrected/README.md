# Corrected ensemble pipeline (2009, PAN1, PAN2)

Run from anywhere inside the repository. Originals are untouched.

## Ensemble method switch

| `ensemble_method` | Definition | Output folder |
|---|---|---|
| `mean_of_quantiles` (**default**) | The paper's method: mean across models of the means, 2.5th and 97.5th percentiles | `model-forecasts/MeanOfQuantiles-Corrected/` |
| `linear_pool` | Equal-weight mixture of the model distributions (sensitivity analysis) | `model-forecasts/LinearPool-Ensemble/` |

`Write_Ensemble_corrected.R` writes both (edit `methods_to_run` to change). Every downstream
script uses `options(flucode.ensemble_method = "linear_pool")` to switch; unset = paper's method.
Figures go to `figures/<method>/`, supplementary tables to `supp_tables/<method>/`.

## Scripts

| Script | Purpose |
|---|---|
| `Write_Ensemble_corrected.R` | Replaces `Write_Ensemble_{2009,EPI1,EPI2}.R`; same file schemas as `MeanBased-Ensemble/`, plus `*_age_interval.csv`. |
| `Verify_Corrections.R` | Diffs corrected vs original outputs for the selected method. |
| `Audit_Distributions.R` | Read-only audit: files with all mass in the open top bin, or non-monotone quantiles (`distribution_audit.csv`). |
| `Write_Supp_Tables.R` | Rebuilds the three supplementary tables. |
| `Figures_{2009,EPI1,EPI2}_corrected.R`, `Ensemble_*_Figure_corrected.R` | Corrected copies of the figure scripts. |

## Code errors fixed (both methods)

1. **Units**: averted hospitalisations/deaths used `Cml*100000`; a rate per 100,000 times the US
   population needs `* 3,068` (306.8e6 / 1e5). Old values were 32.595x too high. Hospitalisation
   and death averted-count panels are now plotted in thousands. Symptomatic illness (millions) was
   already correct.
2. **PAN1 HP11/HP12**: in `Write_Ensemble_EPI1.R` the `Cml11`/`Cml12` lines run before `cml` is
   updated, so team-level averted percent/number for HP11 and HP12 (and ranks) were those of
   HP10 and HP11 (visible in paper Fig. 4B, where HP10 and HP11 are identical for several
   models). ENS rows were unaffected.
3. No `setwd()`, no `reshape`/`khroma`; dplyr functions are pinned against plyr/data.table masking.

With `mean_of_quantiles`, all ENS intervals, trajectories and tables reproduce the originals
exactly; only the two fixes above change (see `Verify_Corrections.R`).

## Not a bug, but flagged by an external reader

The ensemble interval averages model quantiles (as stated in the paper's Methods), so it reflects
within-model rather than between-model uncertainty. `linear_pool` gives intervals roughly 2-3x
wider (e.g. PAN1 hospitalisations 121-493 vs 243-337 per 100,000). Whether to adopt it is a
decision for the authors.

## Limitations

- Binned probabilities in files whose mass sits in the open top bin cannot be repaired. All
  numbers here use the `sm.*` summary rows, which are unaffected. `Ensemble.R`,
  `Probablities.R` (bin-based) and the Visualization Rmd/html files were not regenerated.
- Linear-pool outer quantiles rely on a tail extension (one segment width, floored at 0); see
  `linear_pool_quantiles()` in `helpers.R`.
