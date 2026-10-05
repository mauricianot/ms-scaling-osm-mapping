# ================================================================
# OSRM ROUTE CALCULATION - HOUSEHOLDS TO NEAREST CSB
# MNJ STUDY
# ================================================================
#
# Purpose:
#   Calculate road routes between each household and its nearest
#   CSB (health center) using the OSRM routing backend.
#
# Workflow:
#   1. Import the household-to-CSB distance table
#   2. Sort households by household ID
#   3. Split households into several groups
#   4. Calculate OSRM routes for each group
#   5. Run the groups in parallel
#   6. Save route coordinates as CSV files
#   7. Convert routes to line geometries
#   8. Save route geometries as Shapefiles
#
# OSRM backend:
#   http://localhost:5050/
#
# ================================================================


# ================================================================
# 1. SET WORKING DIRECTORY
# ================================================================

setwd(paste0(getwd(), "/data"))


# ================================================================
# 2. LOAD REQUIRED PACKAGES
# ================================================================

library(sp)
library(sf)
library(osrm)
library(shiny)
library(future)
library(ipc)

# ================================================================
# 4. IMPORT HOUSEHOLD-TO-CSB DISTANCE DATA
# ================================================================

# Import the table containing the nearest CSB for each household.
#
# Main variables used later:
#
#   ID_menage = household ID
#   ID_csb    = CSB ID
#   csb_proche = name/identifier of the nearest CSB
#
#   XMenage = household longitude
#   YMenage = household latitude
#
#   XCsb = CSB longitude
#   YCsb = CSB latitude

distanceCsbProche <- read.csv(
  "./table/distance_menagesToCsbMnjComplet.csv"
)


# Sort households by household ID
distanceCsbProche <- distanceCsbProche[
  order(distanceCsbProche$ID_menage),
]


# ================================================================
# 5. INITIALIZE SHINY COMMUNICATION QUEUE
# ================================================================

# Create a communication queue that allows parallel workers
# to send information to the Shiny application.
queue <- shinyQueue()

# Start the queue consumer.
queue$consumer$start(100)


# ================================================================
# 6. CREATE REACTIVE VARIABLES
# ================================================================

# These reactive variables store the household ID currently
# being processed by each parallel job.

var.ID20000mnj  <- reactiveVal()
var.ID40000mnj  <- reactiveVal()
var.ID60000mnj  <- reactiveVal()
var.ID80000mnj  <- reactiveVal()
var.ID100000mnj <- reactiveVal()


# ================================================================
# 7. FUNCTION TO CALCULATE OSRM ROUTES
# ================================================================

fun.parcours <- function(varData, varLabel, varPahtOut) {
  
  
  # --------------------------------------------------------------
  # 7.1 INITIALIZE OUTPUT TABLE
  # --------------------------------------------------------------
  
  # Each row represents one coordinate of an OSRM route.
  #
  # Columns:
  #
  #   idmenage = household ID
  #   site     = nearest CSB identifier
  #   long     = longitude
  #   lat      = latitude
  #   distance = OSRM route distance
  #   track    = unique route identifier
  
  taj <- data.frame(
    idmenage = character(),
    idcsb_proche = character(),
    long = numeric(),
    lat = numeric(),
    distance = numeric(),
    track = character(),
    stringsAsFactors = FALSE
  )
  
  
  # Starting row position in the output table
  pos.initial <- 1
  
  
  # Get household IDs
  varID <- as.factor(varData$ID_menage)
  
  
  # --------------------------------------------------------------
  # 7.2 CALCULATE ROUTE FOR EACH HOUSEHOLD
  # --------------------------------------------------------------
  
  for (i in varID) {
    
    
    # ------------------------------------------------------------
    # Update the corresponding reactive variable
    # ------------------------------------------------------------
    
    queue$producer$fireAssignReactive(
      paste0("var.ID", varLabel),
      i
    )
    
    
    # Display the household currently being processed.
    print(
      paste(
        "Retrieving route for household ID:",
        i,
        "to CSB ->",
        varData[varData$ID_menage == i, ]$csb_proche
      )
    )
    
    
    # ------------------------------------------------------------
    # Calculate OSRM route
    # ------------------------------------------------------------
    
    tryCatch({
      
      
      # Set the local OSRM backend.
      options(
        osrm.server = "http://localhost:5005/"
      )
      
      
      # Calculate the route from the household to the CSB.
      #
      # src:
      #   Household coordinates.
      #
      # dst:
      #   CSB coordinates.
      #
      # overview = "simplified":
      #   Return a simplified route geometry.
      #
      # returnclass = "sp":
      #   Return the route as an sp spatial object.
      current.data <- varData[
        varData$ID_menage == i,
      ]
      
      src.current <-
        data.frame(
          long=current.data$XMenage,
          lat=current.data$YMenage
        )
      
      dst.current <- data.frame(
        long=current.data$XCsb,
        lat=current.data$YCsb
      )
      
      routecsb <- osrmRoute(
        
        src = src.current,
        dst = dst.current,
        
        overview = "simplified"
      )
      
      
      # ----------------------------------------------------------
      # Extract route coordinates
      # ----------------------------------------------------------
      
      # Extract the coordinates of the calculated route.
      parcours <- st_coordinates(routecsb)
      
      
      # Number of coordinate points in the route.
      my.length <- nrow(parcours)
      
      
      # Current position in the output table.
      pos.actuelle <- pos.initial
      
      
      # ----------------------------------------------------------
      # Store household ID
      # ----------------------------------------------------------
      
      taj[
        pos.actuelle:(pos.actuelle + my.length - 1),
        1
      ] <- as.character(
        varData[
          varData$ID_menage == i,
        ]$ID_menage
      )
      
      
      # ----------------------------------------------------------
      # Store CSB identifier
      # ----------------------------------------------------------
      
      taj[
        pos.actuelle:(pos.actuelle + my.length - 1),
        2
      ] <- as.character(
        varData[
          varData$ID_menage == i,
        ]$ID_csb
      )
      
      
      # ----------------------------------------------------------
      # Store longitude
      # ----------------------------------------------------------
      
      taj[
        pos.actuelle:(pos.actuelle + my.length - 1),
        3
      ] <- parcours[, "X"]
      
      
      # ----------------------------------------------------------
      # Store latitude
      # ----------------------------------------------------------
      
      taj[
        pos.actuelle:(pos.actuelle + my.length - 1),
        4
      ] <- parcours[, "Y"]
      
      
      # ----------------------------------------------------------
      # Store OSRM route distance
      # ----------------------------------------------------------
      
      taj[
        pos.actuelle:(pos.actuelle + my.length - 1),
        5
      ] <- routecsb$distance
      
      
      # ----------------------------------------------------------
      # Create unique route ID
      # ----------------------------------------------------------
      
      taj[
        pos.actuelle:(pos.actuelle + my.length - 1),
        6
      ] <- paste0(
        "track",
        i
      )
      
      
      # ----------------------------------------------------------
      # Small delay between OSRM requests
      # ----------------------------------------------------------
      
      # This helps reduce pressure on the local OSRM backend.
      Sys.sleep(0.5)
      
      
      # Update the starting position for the next route.
      pos.initial <- pos.actuelle + my.length
      
      
    }, error = function(e) {
      
      
      # ----------------------------------------------------------
      # Handle OSRM errors
      # ----------------------------------------------------------
      
      # If the route calculation fails, display the error
      # and continue with the next household.
      cat(
        "ERROR:",
        conditionMessage(e),
        "\n"
      )
      
    })
  }
  
  
  # ==============================================================
  # 8. SAVE ROUTE COORDINATES AS CSV
  # ==============================================================
  
  write.csv(
    taj,
    
    file = paste0(
      varPahtOut, 
      "var.ID",
      varLabel,
      "Complet.csv"
    ),
    
    row.names = FALSE
  )
  
  
  # ==============================================================
  # 9. CONVERT ROUTE DATA TO A SPATIAL OBJECT
  # ==============================================================
  
  tajShp <- st_as_sf(
    taj,
    coords = c("long", "lat"),
    crs = 4326,
    remove = FALSE
  )
  
  # ---------------------------------------------------------
  # CREATE SPATIAL LINES
  # ---------------------------------------------------------
  
  # Create one LINESTRING geometry for each household route
  linesTaj <- tajShp |>
    group_by(track) |>
    summarise(
      ID_menage = first(track),
      geometry = st_combine(geometry),
      .groups = "drop"
    ) |>
    st_cast("LINESTRING") |>
    select(ID_menage, geometry)
  
  # Set coordinate reference system
  st_crs(linesTaj) <- 4326
  
  
  # ---------------------------------------------------------
  # SAVE ROUTES AS GEOPACKAGE
  # ---------------------------------------------------------
  
  st_write(
    linesTaj,
    dsn = paste0(
      varPahtOut, "/",
      "pathCsb_", varLabel, ".gpkg"
    ),
    driver = "GPKG",
    layer = paste0("pathCsb_", varLabel),
    delete_layer = TRUE,
    quiet = TRUE
  )
  
}


# ================================================================
# 14. PARALLEL ROUTE PROCESSING
# ================================================================
#
# The households are divided into five groups:
#
#   Group 1 :       1 - 20,000
#   Group 2 :  20,001 - 40,000
#   Group 3 :  40,001 - 60,000
#   Group 4 :  60,001 - 80,000
#   Group 5 :  80,001 - remaining households
#
# Each group is submitted as a separate parallel job.
#
# ================================================================

# Launch all parallel jobs
future::plan(
  future::multisession,
  workers = parallel::detectCores(logical = TRUE)-2
)

varPahtOut <- "./parcours/pathCsb/"

dir.create(
  varPahtOut, 
  recursive = TRUE, 
  showWarnings = FALSE
)

# ------------------------------------------------
# Group 1: households 1-20,000
# ------------------------------------------------

tempjob1 %<-% fun.parcours(
  
  distanceCsbProche[
    distanceCsbProche$ID_menage %in% c(1:20000),
  ],
  
  "20000mnj",
  varPahtOut
  
)


# ------------------------------------------------
# Group 2: households 20,001-40,000
# ------------------------------------------------

tempjob2 %<-% fun.parcours(
  
  distanceCsbProche[
    distanceCsbProche$ID_menage %in% c(20001:40000),
  ],
  
  "40000mnj",
  varPahtOut
  
)


# ------------------------------------------------
# Group 3: households 40,001-60,000
# ------------------------------------------------

tempjob3 %<-% fun.parcours(
  
  distanceCsbProche[
    distanceCsbProche$ID_menage %in% c(40001:60000),
  ],
  
  "60000mnj",
  varPahtOut
  
)


# ------------------------------------------------
# Group 4: households 60,001-80,000
# ------------------------------------------------

tempjob4 %<-% fun.parcours(
  
  distanceCsbProche[
    distanceCsbProche$ID_menage %in% c(60001:80000),
  ],
  
  "80000mnj",
  varPahtOut
  
)


# ------------------------------------------------
# Group 5: households 80,001-end
# ------------------------------------------------

tempjob5 %<-% fun.parcours(
  
  distanceCsbProche[
    distanceCsbProche$ID_menage %in%
      c(80001:nrow(distanceCsbProche)),
  ],
  
  "100000mnj",
  varPahtOut
  
)

# # Stop multisession workers
# future::plan(
#   future::sequential
# )