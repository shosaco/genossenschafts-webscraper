suppressPackageStartupMessages(library(rvest))

args = commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  stop("At least one argument must be supplied.")
} else if (length(args) == 2) {
  input = args[1]
  if (!file.exists(input)) stop("Working folder ", input, " does not exist.")
  
  output = args[2]
  if (!file.exists(output)) stop("Output folder ", output, " does not exist.")
} else {
  stop("Too many or too few arguments.")
}

out_file <- file(file.path(output, "GENOSSENSCHAFTEN_RESULT.txt"))

# helper function because results come as UTF-8 but our strings are latin1
tryCatch(
  source(file.path(input, "script.r"), encoding = "UTF-8"),
  error = function(e) writeLines(e$message, out_file),
  finally = if (!is.null(results)) writeLines(results, out_file)
)

close(out_file)
