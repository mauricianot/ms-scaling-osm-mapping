########################################################################
# HOUSEHOLD ACCESSIBILITY ANALYSIS USING OSRM
#
# Study area: Mananjary District
#
# Workflow:
#   1. Load household and service location data
#   2. Split households into manageable chunks
#   3. Calculate road-network distances using OSRM
#   4. Identify the nearest service for each household
#   5. Add household and nearest-service coordinates
#   6. Export the results as CSV files
#
# The script is used for:
#   - Household -> CSB
#   - Household -> Fokontany chief village/site
#
# IMPORTANT:
#   The OSRM server must be running before executing this script.
#
# Example:
#   sudo osrm-routed --max-table-size=50000 OSMIfanadiana.osrm
########################################################################


########################################################################
# 1. INITIALIZATION
########################################################################

# Define the data directory.
# The script assumes that the current working directory contains
# the "data" folder.
data_dir <- file.path(getwd(), "data")

if (!dir.exists(data_dir)) {
  stop(
    "The 'data' directory was not found in: ",
    getwd()
  )
}


# Only the osrm package is required for this script.
library(osrm)
library(future)
library(parallel)
library(future.apply)

########################################################################
# 2. OSRM SERVER CONFIGURATION
########################################################################

# Define the local OSRM server.
#
# Example:
# sudo osrm-routed --max-table-size=50000 OSMIfanadiana.osrm
#
# --max-table-size=50000 defines the maximum number of locations
# that can be processed by the OSRM Table service.
OSRM_SERVER <- "http://localhost:5005/"

options(
  osrm.server = OSRM_SERVER
)

# Define the OSRM routing profile.
#
# Possible profiles depend on the OSRM server configuration:
#   "car"
#   "foot"
#   "bike"
#
# Make sure this profile matches the profile used to prepare
# the OSRM data.
options(
  osrm.profile = "foot"
)


########################################################################
# 3. FUNCTION: PREPARE POINT DATA
########################################################################

# Convert the original data into a standard format required by OSRM.
#
# Expected original structure:
#   column 5 = ID
#   column 3 = longitude (X)
#   column 4 = latitude  (Y)
#
# Output:
#   ID = point identifier
#   X  = longitude
#   Y  = latitude
prepare_points <- function(
    data,
    id_col = 5,
    x_col = 3,
    y_col = 4
) {
  
  # Select ID, X and Y columns
  points <- data[
    ,
    c(id_col, x_col, y_col),
    drop = FALSE
  ]
  
  # Assign standardized column names
  names(points) <- c(
    "ID",
    "X",
    "Y"
  )
  
  # Keep IDs as character values
  points$ID <- as.character(points$ID)
  
  # Convert coordinates to numeric
  points$X <- as.numeric(points$X)
  points$Y <- as.numeric(points$Y)
  
  # Check for missing coordinates
  if (anyNA(points$X) || anyNA(points$Y)) {
    warning(
      "Some points contain missing X or Y coordinates."
    )
  }
  
  # Check longitude range
  if (any(
    points$X < -180 |
    points$X > 180,
    na.rm = TRUE
  )) {
    stop(
      "Some X coordinates are outside the valid longitude range."
    )
  }
  
  # Check latitude range
  if (any(
    points$Y < -90 |
    points$Y > 90,
    na.rm = TRUE
  )) {
    stop(
      "Some Y coordinates are outside the valid latitude range."
    )
  }
  
  # Use point IDs as row names.
  # These IDs are retained in the OSRM distance matrix.
  rownames(points) <- points$ID
  
  return(points)
}


########################################################################
# 4. FUNCTION: CALCULATE NEAREST SERVICE USING OSRM
########################################################################

# Calculate the nearest destination for every source point.
#
# Arguments:
#   src:
#     Source points, e.g. households.
#
#   dst:
#     Destination points, e.g. CSBs.
#
#   chunk_size:
#     Number of households processed in each OSRM request.
#
#   service_label:
#     Text used for progress messages.
#
# The function automatically divides source points into chunks.
# This avoids manually specifying ranges and prevents accidental
# overlaps or missing rows.

n.threads <- max(
  1,
  parallel::detectCores(logical = TRUE) - 2
)

calculate_nearest_osrm <- function(
    src,
    dst,
    chunk_size = 25000,
    service_label = "service_proche",
    n_threads = n.threads,
    osrm_server = OSRM_SERVER
) {
  
  ######################################################################
  # Check input data
  ######################################################################
  
  stopifnot(
    all(c("ID", "X", "Y") %in% names(src)),
    all(c("ID", "X", "Y") %in% names(dst))
  )
  
  if (nrow(src) == 0) {
    stop("The source dataset contains no points.")
  }
  
  if (nrow(dst) == 0) {
    stop("The destination dataset contains no points.")
  }
  
  if (chunk_size < 1) {
    stop("chunk_size must be greater than 0.")
  }
  
  if (n_threads < 1) {
    stop("n_threads must be greater than 0.")
  }
  
  
  ######################################################################
  # Display calculation information
  ######################################################################
  
  message(
    "\nOSRM calculation: ",
    nrow(src),
    " households x ",
    nrow(dst),
    " ",
    service_label
  )
  
  message(
    "Chunk size: ",
    format(chunk_size, big.mark = ",")
  )
  
  message(
    "Parallel workers: ",
    n_threads
  )
  
  
  ######################################################################
  # Create non-overlapping chunks
  ######################################################################
  
  chunk_start <- seq(
    from = 1,
    to = nrow(src),
    by = chunk_size
  )
  
  chunk_end <- pmin(
    chunk_start + chunk_size - 1,
    nrow(src)
  )
  
  n_chunks <- length(chunk_start)
  
  message(
    "Number of chunks: ",
    n_chunks,
    "\n"
  )
  
  
  ######################################################################
  # Prepare source and destination coordinates
  #
  # Row names are explicitly set to IDs so that OSRM results can be
  # linked reliably to the original household and infrastructure IDs.
  ######################################################################
  
  dst_coords <- dst[, c("X", "Y"), drop = FALSE]
  
  rownames(dst_coords) <- as.character(dst$ID)
  
  
  ######################################################################
  # Function executed by each parallel worker
  ######################################################################
  
  process_chunk <- function(i) {
    
    message(
      "Worker processing chunk ",
      i,
      "/",
      n_chunks,
      " (rows ",
      chunk_start[i],
      "-",
      chunk_end[i],
      ")"
    )
    
    
    ####################################################################
    # Extract source chunk
    ####################################################################
    
    src_chunk <- src[
      chunk_start[i]:chunk_end[i],
      ,
      drop = FALSE
    ]
    
    src_coords <- src_chunk[, c("X", "Y"), drop = FALSE]
    
    rownames(src_coords) <- as.character(src_chunk$ID)
    
    
    ####################################################################
    # Configure OSRM server inside the worker
    #
    # This is important when using multisession workers because each
    # worker runs in a separate R process.
    ####################################################################
    
    if (!is.null(OSRM_SERVER)) {
      options(
        osrm.server = OSRM_SERVER
      )
    }
    
    
    ####################################################################
    # Calculate OSRM distance matrix
    #
    # Distances are returned in meters.
    ####################################################################
    
    osrm_result <- osrm::osrmTable(
      src = src_coords,
      dst = dst_coords,
      measure = "distance"
    )
    
    
    ####################################################################
    # Extract distance matrix
    ####################################################################
    
    distance_matrix <- osrm_result$distances
    
    
    if (is.null(distance_matrix)) {
      stop(
        "OSRM returned no distance matrix for chunk ",
        i
      )
    }
    
    
    ####################################################################
    # Replace missing distances with Inf
    #
    # Inf means that no valid route was found.
    ####################################################################
    
    distance_matrix[
      is.na(distance_matrix)
    ] <- Inf
    
    
    ####################################################################
    # Find nearest destination for each household
    ####################################################################
    
    nearest_index <- max.col(
      -distance_matrix,
      ties.method = "first"
    )
    
    
    ####################################################################
    # Extract minimum road distance
    ####################################################################
    
    distance_min <- distance_matrix[
      cbind(
        seq_len(nrow(distance_matrix)),
        nearest_index
      )
    ]
    
    
    ####################################################################
    # Extract nearest destination ID
    ####################################################################
    
    service_id <- colnames(
      distance_matrix
    )[nearest_index]
    
    
    ####################################################################
    # Identify households without a reachable destination
    ####################################################################
    
    unreachable <- !is.finite(
      distance_min
    )
    
    service_id[
      unreachable
    ] <- NA_character_
    
    
    ####################################################################
    # Return only the required information
    #
    # The complete distance matrix is released after this step,
    # reducing memory consumption.
    ####################################################################
    
    setNames(
      data.frame(
        ID_menage = rownames(distance_matrix),
        distance_min = distance_min,
        service_id = service_id,
        stringsAsFactors = FALSE
      ),
      c("ID_menage", "distance_min", service_label)
    )
    
  }
  
  ######################################################################
  # Parallel processing
  ######################################################################
  
  if (n_threads > 1 && n_chunks > 1) {
    
    # Load future.apply only when parallel processing is requested.
    if (!requireNamespace("future", quietly = TRUE)) {
      stop(
        "Package 'future' is required for parallel processing."
      )
    }
    
    if (!requireNamespace("future.apply", quietly = TRUE)) {
      stop(
        "Package 'future.apply' is required for parallel processing."
      )
    }
    
    
    ####################################################################
    # Start parallel workers
    ####################################################################
    
    future::plan(
      future::multisession,
      workers = min(n_threads, n_chunks)
    )
    
    
    ####################################################################
    # Process all chunks in parallel
    ####################################################################
    
    results <- future.apply::future_lapply(
      seq_len(n_chunks),
      process_chunk,
      future.packages = "osrm",
      future.seed = TRUE
    )
    
    
    ####################################################################
    # Return to sequential execution after processing
    ####################################################################
    
    future::plan(
      future::sequential
    )
    
  } else {
    
    ####################################################################
    # Sequential fallback
    ####################################################################
    
    results <- lapply(
      seq_len(n_chunks),
      process_chunk
    )
  }
  
  
  ######################################################################
  # Combine all chunks
  ######################################################################
  
  result <- do.call(
    rbind,
    results
  )
  
  rownames(result) <- NULL
  
  
  ######################################################################
  # Convert distance from meters to kilometers
  ######################################################################
  
  result$distance_min_km <-
    result$distance_min / 1000
  
  
  ######################################################################
  # Display completion information
  ######################################################################
  
  message(
    "\nOSRM calculation completed: ",
    nrow(result),
    " households processed."
  )
  
  return(result)
}


########################################################################
# 5. FUNCTION: ADD POINT COORDINATES
########################################################################

# Add the coordinates of:
#   - the household
#   - the nearest service
#
# This function can be reused for CSBs, chief villages,
# schools, health facilities, etc.
add_coordinates <- function(
    result,
    src,
    dst,
    service_id_name = "service_proche"
) {
  
  # Match household coordinates
  src_coord <- src[
    match(
      result$ID_menage,
      src$ID
    ),
    c("ID", "X", "Y"),
    drop = FALSE
  ]
  
  names(src_coord) <- c(
    "ID_menage",
    "XMenage",
    "YMenage"
  )
  
  
  # Match coordinates of the nearest service
  dst_coord <- dst[
    match(
      result[[service_id_name]],
      dst$ID
    ),
    c("ID", "X", "Y"),
    drop = FALSE
  ]
  
  names(dst_coord) <- c(
    service_id_name,
    "XService",
    "YService"
  )
  
  
  # Add household coordinates
  result$XMenage <- src_coord$XMenage
  result$YMenage <- src_coord$YMenage
  
  
  # Add nearest-service coordinates
  result$XService <- dst_coord$XService
  result$YService <- dst_coord$YService
  
  return(result)
}


########################################################################
# 6. HOUSEHOLD -> CSB
########################################################################

# Import CSB data
CsbMnj <- read.csv(
  file.path(
    data_dir,
    "input",
    "csb_mnj.csv"
  ),
  stringsAsFactors = FALSE
)


# Import household data
menagesMnj <- read.csv(
  file.path(
    data_dir,
    "input",
    "menage_mnj.csv"
  ),
  stringsAsFactors = FALSE
)


# Standardize point data
CsbMnj.tab <- prepare_points(
  CsbMnj
)

menagesMnj.tab <- prepare_points(
  menagesMnj
)


# Sort points by ID
CsbMnj.tab <- CsbMnj.tab[
  order(CsbMnj.tab$ID),
  ,
  drop = FALSE
]

menagesMnj.tab <- menagesMnj.tab[
  order(menagesMnj.tab$ID),
  ,
  drop = FALSE
]


########################################################################
# 7. CALCULATE HOUSEHOLD -> CSB DISTANCES
########################################################################

menages_to_csb <- calculate_nearest_osrm(
  src = menagesMnj.tab,
  dst = CsbMnj.tab,
  chunk_size = 25000,
  service_label = "CSBs_proche",
  osrm_server = OSRM_SERVER
)

# Add household and CSB coordinates
menages_to_csb.coords <- add_coordinates(
  result = menages_to_csb,
  src = menagesMnj.tab,
  dst = CsbMnj.tab,
  service_id_name = "CSBs_proche"
)


# Rename the nearest-service ID
names(menages_to_csb.coords)[
  names(menages_to_csb.coords) == "CSBs_proche"
] <- "ID_csb"


# Keep and organize the final variables
menagesMnj.distanceCompletCsb <- menages_to_csb.coords[
  ,
  c(
    "ID_csb",
    "ID_menage",
    "distance_min",
    "distance_min_km",
    "XMenage",
    "YMenage",
    "XService",
    "YService"
  )
]


# Rename CSB coordinates
names(menagesMnj.distanceCompletCsb)[
  names(menagesMnj.distanceCompletCsb) == "XService"
] <- "XCsb"

names(menagesMnj.distanceCompletCsb)[
  names(menagesMnj.distanceCompletCsb) == "YService"
] <- "YCsb"


########################################################################
# 8. EXPORT HOUSEHOLD -> CSB RESULTS
########################################################################

output_csb <- file.path(
  data_dir,
  "table",
  "distance_menagesToCsbMnjComplet.csv"
)

write.csv(
  menagesMnj.distanceCompletCsb,
  file = output_csb,
  row.names = FALSE
)

message(
  "\nCSB result saved to: ",
  output_csb
)


########################################################################
# 9. HOUSEHOLD -> CHIEF VILLAGE/SITE
########################################################################

# Import chief village/site data
chefLieuVillageMnj <- read.csv(
  file.path(
    data_dir,
    "input",
    "chef_lieu_fkt_mnj.csv"
  ),
  stringsAsFactors = FALSE
)


# Prepare chief village/site points
chefLieuVillageMnj.tab <- prepare_points(
  chefLieuVillageMnj
)


# Prepare household points
menagesMnj.tab <- prepare_points(
  menagesMnj
)


# Sort by ID
chefLieuVillageMnj.tab <- chefLieuVillageMnj.tab[
  order(chefLieuVillageMnj.tab$ID),
  ,
  drop = FALSE
]

menagesMnj.tab <- menagesMnj.tab[
  order(menagesMnj.tab$ID),
  ,
  drop = FALSE
]


########################################################################
# 10. CALCULATE HOUSEHOLD -> CHIEF VILLAGE/SITE DISTANCES
########################################################################

menages_to_chef_lieu <- calculate_nearest_osrm(
  src = menagesMnj.tab,
  dst = chefLieuVillageMnj.tab,
  chunk_size = 25000,
  service_label = "chiefVillagesSites_proche",
  osrm_server = OSRM_SERVER
)

# Add household and chief village/site coordinates
menages_to_chef_lieu <- add_coordinates(
  result = menages_to_chef_lieu,
  src = menagesMnj.tab,
  dst = chefLieuVillageMnj.tab,
  service_id_name = "chiefVillagesSites_proche"
)


# Rename the nearest destination
names(menages_to_chef_lieu)[
  names(menages_to_chef_lieu) == "chiefVillagesSites_proche"
] <- "ID_chef_lieu_fkt"


########################################################################
# 11. ORGANIZE CHIEF VILLAGE/SITE RESULTS
########################################################################

menagesMnj.distanceComplet <- menages_to_chef_lieu[
  ,
  c(
    "ID_chef_lieu_fkt",
    "distance_min",
    "distance_min_km",
    "ID_menage",
    "XMenage",
    "YMenage",
    "XService",
    "YService"
  )
]


# Rename chief village/site coordinates
names(menagesMnj.distanceComplet)[
  names(menagesMnj.distanceComplet) == "XService"
] <- "XChefLSite"

names(menagesMnj.distanceComplet)[
  names(menagesMnj.distanceComplet) == "YService"
] <- "YChefLSite"


########################################################################
# 12. EXPORT HOUSEHOLD -> CHIEF VILLAGE/SITE RESULTS
########################################################################

output_chef_lieu <- file.path(
  data_dir,
  "table",
  "distance_menagesToChefLieuSiteMnjComplet.csv"
)

write.csv(
  menagesMnj.distanceComplet,
  file = output_chef_lieu,
  row.names = FALSE
)

message(
  "\nChief village/site result saved to: ",
  output_chef_lieu
)


########################################################################
# 13. FINAL QUALITY CHECKS
########################################################################

message("\n==============================================")
message("OSRM ACCESSIBILITY ANALYSIS COMPLETED")
message("==============================================")


# Number of households
message(
  "Number of households: ",
  nrow(menagesMnj.tab)
)


# Number of household -> CSB results
message(
  "Household -> CSB results: ",
  nrow(menagesMnj.distanceCompletCsb)
)


# Number of household -> chief village/site results
message(
  "Household -> chief village/site results: ",
  nrow(menagesMnj.distanceComplet)
)


# Check unreachable households
message(
  "Unreachable households -> CSB: ",
  sum(
    !is.finite(
      menagesMnj.distanceCompletCsb$distance_min
    )
  )
)

message(
  "Unreachable households -> chief village/site: ",
  sum(
    !is.finite(
      menagesMnj.distanceComplet$distance_min
    )
  )
)


# Check for duplicate household IDs
message(
  "Duplicate household IDs -> CSB: ",
  sum(
    duplicated(
      menagesMnj.distanceCompletCsb$ID_menage
    )
  )
)

message(
  "Duplicate household IDs -> chief village/site: ",
  sum(
    duplicated(
      menagesMnj.distanceComplet$ID_menage
    )
  )
)

message("==============================================")
