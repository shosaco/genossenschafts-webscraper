this_file = gsub("--file=", "", commandArgs()[grepl("--file", commandArgs())])
if (length(this_file) == 0){
  wd <- getwd()
}else{
  wd <- paste(head(strsplit(this_file, '[/|\\]')[[1]], -1), collapse = .Platform$file.sep)
}

# helper function because results come as UTF-8 but our strings are latin1
tryCatch(
  source(file.path(wd, "script.r"), encoding = "UTF-8"),
  error = function(e){ cat(toString(e)); Sys.sleep(600)}
)

