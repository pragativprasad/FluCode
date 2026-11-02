# Regenerates the three supplementary tables (base-scenario burden, "mean (2.5%, 97.5%)") with
# ENS intervals from the selected ensemble_method (default mean_of_quantiles). Layout, titles and model rows are copied from the originals
# (comparison-scripts/*_supp_tables.xlsx); only the cell values are replaced from
# the selected method's *_table.csv. Output: Ensemble_Corrected/supp_tables/<method>/

library(readxl)
library(writexl)

local({
  d <- getwd()
  cand <- c(file.path(d, "helpers.R"), file.path(d, "Ensemble_Corrected", "helpers.R"),
            file.path(d, "comparison-scripts", "Ensemble_Corrected", "helpers.R"))
  source(cand[file.exists(cand)][1])
})
cs <- file.path(repo_root, "comparison-scripts")
lp <- out_path  # from helpers.R, per ensemble_method
out_dir <- file.path(cs, "Ensemble_Corrected", "supp_tables", ensemble_method)
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

specs <- list(c(file = "2009_supp_tables.xlsx", tag = "2009"),
              c(file = "pan1_supp_tables.xlsx", tag = "PAN1"),
              c(file = "pan2_supp_tables.xlsx", tag = "PAN2"))

for (sp in specs) {
  orig <- read_excel(file.path(cs, sp[["file"]]), col_names = FALSE, .name_repair = "minimal")
  tab <- as.data.frame(orig, stringsAsFactors = FALSE)
  teams <- tab[[1]]
  for (j in 2:4) {
    nm <- c("symillness", "hospitalization", "death")[j - 1]
    new <- read.csv(file.path(lp, paste0(sp[["tag"]], "_", nm, "_table.csv")))
    vals <- new[[ncol(new)]][match(teams, new$team)]
    tab[[j]][!is.na(vals)] <- vals[!is.na(vals)]
  }
  names(tab) <- names(orig)
  write_xlsx(tab, file.path(out_dir, sp[["file"]]), col_names = FALSE)
}
