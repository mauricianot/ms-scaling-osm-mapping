# ================================================================
# OSRM ROUTE CALCULATION - MNJ
# ================================================================
#
# Purpose:
#   Calculate road routes between each household and its nearest
#   MNJ site using the OSRM routing backend.
#
# Workflow:
#   1. Import the household-to-MNJ-site distance table
#   2. Split households into several groups
#   3. Calculate OSRM routes for each group
#   4. Process the groups in parallel
#   5. Save:
#        - route coordinates as CSV files
#        - route geometries as Shapefiles
#
# OSRM backend:
#   http://localhost:5005/
#
# ================================================================

########################################################################
# 2. OSRM SERVER CONFIGURATION
########################################################################


# ================================================================
# 1. SET WORKING DIRECTORY
# ================================================================

setwd(paste0(getwd(), "/data"))


# ================================================================
# 2. LOAD REQUIRED PACKAGES
# ================================================================

library(sp)
library(osrm)
library(shiny)
library(future)
library(ipc)
library(sf)

# ================================================================
# 4. IMPORT DISTANCE DATA
# ================================================================

# Import the table containing the nearest MNJ site for each household.
#
# Main variables used later:
#   ID_menage        = household ID
#   ID_chef_lieu_fkt = MNJ site ID
#   XMenage          = household longitude
#   YMenage          = household latitude
#   XChefLSite       = MNJ site longitude
#   YChefLSite       = MNJ site latitude

distanceSiteProche <- read.csv(
  "./table/distance_menagesToChefLieuSiteMnjComplet.csv"
)


# Sort households by household ID
distanceSiteProche <- distanceSiteProche[
  order(distanceSiteProche$ID_menage),
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

var.IDs20000mnj  <- reactiveVal()
var.IDs40000mnj  <- reactiveVal()
var.IDs60000mnj  <- reactiveVal()
var.IDs80000mnj  <- reactiveVal()
var.IDs100000mnj <- reactiveVal()


# ================================================================
# 7. FUNCTION TO CALCULATE OSRM ROUTES
# ================================================================

fun.parcours <- function(varData, varLabel, varPahtOut) {

  # --------------------------------------------------------------
  # 7.1 Initialize output table
  # --------------------------------------------------------------
  
  # Each row represents one coordinate of an OSRM route.
  #
  # Columns:
  #   idmenage = household ID
  #   site     = MNJ site ID
  #   long     = longitude
  #   lat      = latitude
  #   distance = OSRM route distance
  #   track    = unique route ID
  
  taj <- data.frame(
    idmenage = character(),
    site = character(),
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
  # 7.2 Calculate routes for each household
  # --------------------------------------------------------------
  
  for (i in varID) {

    # Update the corresponding reactive variable in Shiny.
    queue$producer$fireAssignReactive(
      paste0("var.ID", varLabel),
      i
    )
    
    
    # Display the household currently being processed.
    print(
      paste(
        "Retrieving route for household ID:",
        i,
        "to MNJ site ->",
        varData[varData$ID_menage == i, ]$ID_chef_lieu_fkt
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
      
      
      # Calculate the route from the household to the MNJ site.
      #
      # src:
      #   Household coordinates.
      #
      # dst:
      #   MNJ site coordinates.
      #
      # overview = "simplified":
      #   Return a simplified route geometry.
      current.data <- varData[
        varData$ID_menage == i,
      ]
      
      src.current <-
        data.frame(
          long=current.data$XMenage,
          lat=current.data$YMenage
      )
      
      dst.current <- data.frame(
        long=current.data$XChefLSite,
        lat=current.data$YChefLSite
      )
      
      routesite <- osrmRoute(
        src = src.current,
        dst = dst.current,
        overview = "simplified"
      )
      
      
      # ----------------------------------------------------------
      # Extract route coordinates
      # ----------------------------------------------------------
      
      # Extract the coordinates of the calculated route.
      parcours <- st_coordinates(routesite)
      
      
      # Number of coordinate points in the route
      my.length <- nrow(parcours)
      
      
      # Current position in the output table
      pos.actuelle <- pos.initial
      
      
      # ----------------------------------------------------------
      # Store route attributes
      # ----------------------------------------------------------
      
      # Household ID
      taj[
        pos.actuelle:(pos.actuelle + my.length - 1),
        1
      ] <- as.character(
        varData[varData$ID_menage == i, ]$ID_menage
      )
      
      
      # MNJ site ID
      taj[
        pos.actuelle:(pos.actuelle + my.length - 1),
        2
      ] <- as.character(
        varData[varData$ID_menage == i, ]$ID_chef_lieu_fkt
      )
      
      
      # Longitude
      taj[
        pos.actuelle:(pos.actuelle + my.length - 1),
        3
      ] <- parcours[, "X"]
      
      
      # Latitude
      taj[
        pos.actuelle:(pos.actuelle + my.length - 1),
        4
      ] <- parcours[, "Y"]
      
      
      # OSRM route distance
      taj[
        pos.actuelle:(pos.actuelle + my.length - 1),
        5
      ] <- routesite$distance
      
      
      # Create a unique route identifier
      taj[
        pos.actuelle:(pos.actuelle + my.length - 1),
        6
      ] <- paste0("track", i)
      
      
      # Small delay to reduce pressure on the OSRM backend.
      Sys.sleep(0.5)
      
      
      # Update the starting position for the next route.
      pos.initial <- pos.actuelle + my.length
      
      
    }, error = function(e) {
      
      # If the OSRM request fails, display the error and
      # continue with the next household.
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
      "pathSite_", varLabel, ".gpkg"
    ),
    driver = "GPKG",
    layer = paste0("pathSite_", varLabel),
    delete_layer = TRUE,
    quiet = TRUE
  )
  
}


# ================================================================
# 13. PARALLEL ROUTE PROCESSING
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
# ================================================================

# Launch all parallel jobs
future::plan(
  future::multisession,
  workers = parallel::detectCores(logical = TRUE)-2
)

varPahtOut <- "./parcours/pathSite/"

dir.create(
  varPahtOut, 
  recursive = TRUE, 
  showWarnings = FALSE
)

# ------------------------------------------------
# Group 1: households 1-20,000
# ------------------------------------------------

tempjob1s %<-% fun.parcours(
  distanceSiteProche[
    distanceSiteProche$ID_menage %in% c(1:20000),
  ],
  "s20000mnj", 
  varPahtOut
)


# ------------------------------------------------
# Group 2: households 20,001-40,000
# ------------------------------------------------

tempjob2s %<-% fun.parcours(
  distanceSiteProche[
    distanceSiteProche$ID_menage %in% c(20001:40000),
  ],
  "s40000mnj",
  varPahtOut
)


# ------------------------------------------------
# Group 3: households 40,001-60,000
# ------------------------------------------------

tempjob3s %<-% fun.parcours(
  distanceSiteProche[
    distanceSiteProche$ID_menage %in% c(40001:60000),
  ],
  "s60000mnj",
  varPahtOut
)


# ------------------------------------------------
# Group 4: households 60,001-80,000
# ------------------------------------------------

tempjob4s %<-% fun.parcours(
  distanceSiteProche[
    distanceSiteProche$ID_menage %in% c(60001:80000),
  ],
  "s80000mnj",
  varPahtOut
)


# ------------------------------------------------
# Group 5: households 80,001-end
# ------------------------------------------------

tempjob5s %<-% fun.parcours(
  distanceSiteProche[
    distanceSiteProche$ID_menage %in%
      c(80001:nrow(distanceSiteProche)),
  ],
  "s100000mnj",
  varPahtOut
)

# # Stop multisession workers
# future::plan(
#   future::sequential
# )
