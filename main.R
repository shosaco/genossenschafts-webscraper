suppressPackageStartupMessages(library(checkpoint))
checkpoint("2020-01-01", checkpointLocation = "/", verbose = FALSE)
suppressPackageStartupMessages(library(rvest))

out_file <- file("C:\\onedrive\\OneDrive - consus clinicmanagement GmbH\\Desktop\\GENOSSENSCHAFTEN_RESULT.txt")

# helper function because results come as UTF-8 but our strings are latin1
tryCatch(
  source("script.r", encoding = "UTF-8"),
  error = function(e) writeLines(e$message, out_file),
  finally = if (!is.null(results)) writeLines(paste(results, collapse = "\n"), out_file)
)

close(out_file)
