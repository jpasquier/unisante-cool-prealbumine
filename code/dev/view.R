# View data frame in Spreadsheet application
view <- function(df) {

    # Check if required packages are installed
    if (system.file(package = "writexl") == "") {
        stop(paste("The 'writexl' package is required for writing an",
                   "Excel temporary file."))
    }
    if (system.file(package = "later") == "") {
        stop("The 'later' package is required for delayed cleanup.")
    }

    # Capture the object name passed in
    raw_name <- deparse(substitute(df))
    ## if it's the magrittr-pipe placeholder ".", or something else weird,
    ## fall back to "piped_dataframe"
    df_name <- if (identical(raw_name, ".") || grepl("^\\.+$", raw_name)) {
        "piped_dataframe"
    } else {
        raw_name
    }

    # Create temporary directory and file
    tmp_dir <- system("mktemp -d ~/.cache/R/tmpdir.XXXXXX", intern = TRUE)
    pattern  <- paste0(shQuote(paste0(df_name, ".XXXXXX.csv")))
    tmp_file <- system(paste("mktemp -p", shQuote(tmp_dir), pattern),
                       intern = TRUE)
    writexl::write_xlsx(df, tmp_file)

    # Open the file with the default spreadsheet application
    cmd <- paste("xdg-open", shQuote(tmp_file), ">/dev/null 2>&1 &")
    system(cmd)

    # Delay file deletion to allow LibreOffice to read the file
    later::later(function() {
        if (file.exists(tmp_file)) file.remove(tmp_file)
        if (dir.exists(tmp_dir)) unlink(tmp_dir, recursive = TRUE)
    }, delay = 10)
}
