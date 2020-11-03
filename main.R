.libPaths("C:\\onedrive\\OneDrive - consus clinicmanagement GmbH\\Dokumente\\R\\win-library\\4.0")

tryCatch(
  source("script.r", encoding = "UTF-8"),
  error = function(e){ cat(toString(e)); Sys.sleep(30)}
)

