# ================================================================
# LAND-COVER PATH INTERSECTION
# CSB AND SITE - MANANJARY (MNJ)
# ================================================================

# ================================================================
# 1. SET WORKING DIRECTORY
# ================================================================

setwd(file.path(getwd(), "data"))

# ================================================================
# 2. LOAD REQUIRED PACKAGE
# ================================================================

library(data.table)

# ================================================================
# 3. CONFIGURATION
# ================================================================

# Number of data.table threads
setDTthreads(14)

# Input directories
csb_dir <- "./MNJ/csbCSV_MNJ"
site_dir <- "./MNJ/siteCSV_MNJ"

# Output directory
output_dir <- "./landcover/"

# Create output directory if necessary
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Columns required from the CSV files
cols_length <- c(
  "track",
  "row",
  "distance",
  "GRIDCODE",
  "occ",
  "length"
)

cols_lenght <- c(
  "track",
  "row",
  "distance",
  "GRIDCODE",
  "occ",
  "lenght"
)

# ================================================================
# 4. FUNCTION TO READ CSV FILES
# ================================================================

read_csv_files <- function(files, columns, rename_length = FALSE) {
  
  if (length(files) == 0) {
    stop("No CSV files found.")
  }
  
  result <- rbindlist(
    lapply(files, function(file) {
      
      message("Reading: ", basename(file))
      
      # Check available columns
      available_columns <- names(fread(file, nrows = 0))
      
      missing_columns <- setdiff(columns, available_columns)
      
      if (length(missing_columns) > 0) {
        stop(
          "Missing column(s) in file ",
          basename(file),
          ": ",
          paste(missing_columns, collapse = ", ")
        )
      }
      
      # Read selected columns
      dt <- fread(
        file,
        select = columns
      )
      
      # Rename "lenght" to "length" if necessary
      if (rename_length && "lenght" %in% names(dt)) {
        setnames(dt, "lenght", "length")
      }
      
      # Store source file
      dt[, file_name := basename(file)]
      
      dt
    }),
    use.names = TRUE,
    fill = TRUE
  )
  
  return(result)
}

# ================================================================
# 5. CSB INTERSECTION - MNJ
# ================================================================

cat("\n")
cat("============================================================\n")
cat("CSB LAND-COVER INTERSECTION - MNJ\n")
cat("============================================================\n")

# List CSV files
csb_files <- list.files(
  path = csb_dir,
  pattern = "\\.csv$",
  full.names = TRUE
)

cat("Number of CSB files:", length(csb_files), "\n")

# Read and combine files
dfMNJ_csb_all <- read_csv_files(
  files = csb_files,
  columns = cols_length
)

cat(
  "Number of CSB records:",
  format(nrow(dfMNJ_csb_all), big.mark = ","),
  "\n"
)

# ------------------------------------------------
# Aggregate land-cover length by row and occ
# ------------------------------------------------

dfMNJ_csb_allT <- dcast(
  dfMNJ_csb_all,
  row ~ occ,
  value.var = "length",
  fun.aggregate = sum,
  fill = 0
)

# Round numeric columns to 2 decimals
numeric_cols <- setdiff(
  names(dfMNJ_csb_allT),
  "row"
)

dfMNJ_csb_allT[
  ,
  (numeric_cols) := lapply(
    .SD,
    round,
    digits = 2
  ),
  .SDcols = numeric_cols
]

# ------------------------------------------------
# Export
# ------------------------------------------------

csb_output <- file.path(
  output_dir,
  "pathCsbMNJCompletLandcoverdTransposer.csv"
)

fwrite(
  dfMNJ_csb_allT,
  csb_output
)

cat("Output:", csb_output, "\n")


# ================================================================
# 6. SITE INTERSECTION - MNJ
# ================================================================

cat("\n")
cat("============================================================\n")
cat("SITE LAND-COVER INTERSECTION - MNJ\n")
cat("============================================================\n")

# List CSV files
site_files <- list.files(
  path = site_dir,
  pattern = "\\.csv$",
  full.names = TRUE
)

cat("Number of SITE files:", length(site_files), "\n")

# ------------------------------------------------
# Separate files
# ------------------------------------------------

# The 7th file uses "lenght" instead of "length"
if (length(site_files) >= 7) {
  
  site_files_length <- site_files[-7]
  site_files_lenght <- site_files[7]
  
} else {
  
  stop(
    "The SITE directory contains fewer than 7 files. ",
    "The expected file with 'lenght' cannot be identified."
  )
}

cat(
  "Files using 'length':",
  length(site_files_length),
  "\n"
)

cat(
  "File using 'lenght':",
  basename(site_files_lenght),
  "\n"
)

# ------------------------------------------------
# Read files with correct "length" column
# ------------------------------------------------

dfMNJ_site_all_length <- read_csv_files(
  files = site_files_length,
  columns = cols_length
)

# ------------------------------------------------
# Read file containing "lenght"
# ------------------------------------------------

dfMNJ_site_all_lenght <- read_csv_files(
  files = site_files_lenght,
  columns = cols_lenght,
  rename_length = TRUE
)

# ------------------------------------------------
# Combine all SITE files
# ------------------------------------------------

dfMNJ_site_all <- rbindlist(
  list(
    dfMNJ_site_all_length,
    dfMNJ_site_all_lenght
  ),
  use.names = TRUE,
  fill = TRUE
)

cat(
  "Number of SITE records:",
  format(nrow(dfMNJ_site_all), big.mark = ","),
  "\n"
)

# ================================================================
# 7. CHECK LENGTH VARIABLE
# ================================================================

# Ensure length is numeric
if (!is.numeric(dfMNJ_site_all$length)) {
  
  dfMNJ_site_all[
    ,
    length := as.numeric(length)
  ]
}

# Check for NA values
n_na_length <- sum(is.na(dfMNJ_site_all$length))

if (n_na_length > 0) {
  
  warning(
    n_na_length,
    " NA value(s) detected in 'length'."
  )
}

# ================================================================
# 8. TRANSPOSE SITE DATA
# ================================================================

dfMNJ_site_allT <- dcast(
  dfMNJ_site_all,
  row ~ occ,
  value.var = "length",
  fun.aggregate = sum,
  fill = 0
)

# ================================================================
# 9. ROUND VALUES
# ================================================================

numeric_cols <- setdiff(
  names(dfMNJ_site_allT),
  "row"
)

dfMNJ_site_allT[
  ,
  (numeric_cols) := lapply(
    .SD,
    round,
    digits = 2
  ),
  .SDcols = numeric_cols
]

# ================================================================
# 10. EXPORT SITE RESULTS
# ================================================================

site_output <- file.path(
  output_dir,
  "pathSiteMNJCompletLandcoverdTransposer.csv"
)

fwrite(
  dfMNJ_site_allT,
  site_output
)

cat("Output:", site_output, "\n")


# ================================================================
# 11. SUMMARY
# ================================================================

cat("\n")
cat("============================================================\n")
cat("PROCESS COMPLETED\n")
cat("============================================================\n")

cat(
  "CSB records:",
  format(nrow(dfMNJ_csb_all), big.mark = ","),
  "\n"
)

cat(
  "CSB tracks/rows:",
  format(nrow(dfMNJ_csb_allT), big.mark = ","),
  "\n"
)

cat(
  "SITE records:",
  format(nrow(dfMNJ_site_all), big.mark = ","),
  "\n"
)

cat(
  "SITE tracks/rows:",
  format(nrow(dfMNJ_site_allT), big.mark = ","),
  "\n"
)

cat("\nFiles generated:\n")
cat("-", csb_output, "\n")
cat("-", site_output, "\n")