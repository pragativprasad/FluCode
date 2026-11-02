# Audit of shipped forecast distributions (read-only; nothing is modified).
# Flags, per file x age group x measure (Cml, Peak, Weekly):
#   top_bin   : essentially all probability mass (>= 0.999) sits in the open top bin ("...-Inf)")
#   nonmono   : summary quantiles (sm.perc2p5 ... sm.perc97p5) are not non-decreasing
# Mass-in-top-bin cannot be repaired after the fact (the real distribution is not recoverable
# from the binned file); those entries should be regenerated from model output with a bin grid
# that covers the range, or excluded. The linear pool sorts quantiles before use; the paper-method ensemble
# averages them as given.
#
# Output: Ensemble_Corrected/distribution_audit.csv (+ file-level summary printed)

local({
  d <- getwd()
  cand <- c(file.path(d, "helpers.R"), file.path(d, "Ensemble_Corrected", "helpers.R"),
            file.path(d, "comparison-scripts", "Ensemble_Corrected", "helpers.R"))
  source(cand[file.exists(cand)][1])
})
audit_path <- file.path(repo_root, "comparison-scripts", "Ensemble_Corrected")

files <- list.files(forecast_path, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
files <- files[!grepl("MeanBased-Ensemble|LinearPool-Ensemble|MeanOfQuantiles-Corrected", files)]
qstats <- paste0("sm.", c("perc2p5", "perc5", "perc25", "perc75", "perc95", "perc97p5"))

audit_file <- function(f) {
  s <- tryCatch(read.csv(f), error = function(e) NULL)
  if (is.null(s) || !all(c("Agegroup", "Bin") %in% names(s))) return(NULL)
  names(s)[names(s) == "cml"] <- "Cml"
  names(s)[names(s) == "PeakMagnitude"] <- "Peak.Magnitude"
  measures <- intersect(c("Cml", "Peak.Magnitude", grep("^Week", names(s), value = TRUE)), names(s))
  if (!"Bin_cml" %in% names(s)) s$Bin_cml <- s$Bin
  is_sm <- grepl("^sm\\.", s$Bin)
  out <- list()
  for (ag in unique(s$Agegroup)) {
    a <- s[s$Agegroup == ag, ]
    sm <- a[grepl("^sm\\.", a$Bin), ]
    bins <- a[!grepl("^sm\\.", a$Bin), ]
    top_cml <- grepl("Inf\\)$", bins$Bin_cml)
    top_wk <- grepl("Inf\\)$", bins$Bin)
    for (m in measures) {
      top <- if (m == "Cml") top_cml else top_wk
      top_mass <- if (any(top)) sum(bins[[m]][top], na.rm = TRUE) else NA_real_
      q <- as.numeric(unlist(lapply(qstats, function(st) sm[[m]][sm$Bin == st][1])))
      mono <- if (anyNA(q)) NA else all(diff(q) >= -1e-9)
      out[[length(out) + 1]] <- data.frame(
        file = sub(paste0(forecast_path, "/"), "", f, fixed = TRUE), agegroup = ag, measure = m,
        top_bin_mass = top_mass, top_bin = isTRUE(top_mass >= 0.999),
        nonmono = identical(mono, FALSE))
    }
  }
  do.call(rbind, out)
}

audit <- do.call(rbind, lapply(files, audit_file))
write.csv(audit, file.path(audit_path, "distribution_audit.csv"), row.names = FALSE)

per_file <- aggregate(cbind(top_bin, nonmono) ~ file, data = audit, FUN = any)
cat("Files audited:", nrow(per_file), "\n")
cat("Files with any age group/measure entirely in open top bin:", sum(per_file$top_bin), "\n")
cat("Files with non-monotone quantiles:", sum(per_file$nonmono), "\n")
cat("\nOverall age group only:\n")
ov <- audit[audit$agegroup == "Overall", ]
pf <- aggregate(cbind(top_bin, nonmono) ~ file, data = ov, FUN = any)
cat("  top-bin:", sum(pf$top_bin), " non-monotone:", sum(pf$nonmono), " of", nrow(pf), "files\n")
