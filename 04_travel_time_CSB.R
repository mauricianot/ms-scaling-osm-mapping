# ================================================================
# CSB ROUTE TIME PREDICTION - MANANJARY (MNJ)
#
# Purpose:
#   1. Import CSB route data.
#   2. Combine route data with land-cover information.
#   3. Calculate original savane_arboree where necessary.
#   4. Classify slope, distance and land-cover categories.
#   5. Load and fit the walking-speed GAM.
#   6. Predict walking speed under minimum rainfall.
#   7. Predict walking speed under maximum rainfall.
#   8. Calculate travel time and confidence intervals.
#   9. Aggregate travel time by track.
#  10. Merge travel time with household-to-CSB distance.
#  11. Export final results.
#
# Modernized for:
#   - data.table
#   - mgcv
#   - R >= 4.4
#
# ================================================================


# ================================================================
# 1. SET WORKING DIRECTORY
# ================================================================

setwd(file.path(getwd(), "data"))


# ================================================================
# 2. LOAD REQUIRED PACKAGES
# ================================================================

library(data.table)
library(mgcv)


# ================================================================
# 3. CONFIGURATION
# ================================================================

# Number of data.table threads
setDTthreads(14)


# ------------------------------------------------
# Input files
# ------------------------------------------------

route_file <- "./parcours/pathCsbComplet_MNJ_Complet_v2.csv"

landcover_file <-
  "./landcover/pathCsbMNJCompletLandcoverdTransposer.csv"

model_file <- "./model_pied.rds"

distance_household_file <-
  "./table/distance_menagesToCsbMnjComplet.csv"


# ------------------------------------------------
# Output files
# ------------------------------------------------

time_output_file <-
  "./time/pathCsbMNJCompletTimeWithRainAndWithout_With_SdeFit.csv"

final_output_file <-
  "./time/pathCsbMNJCompletTimeAll_With_SdeFit.csv"


# ================================================================
# 4. CHECK INPUT FILES
# ================================================================

input_files <- c(
  route_file,
  landcover_file,
  model_file,
  distance_household_file
)

missing_files <- input_files[!file.exists(input_files)]

if (length(missing_files) > 0) {
  
  stop(
    "\nThe following input file(s) do not exist:\n",
    paste(
      missing_files,
      collapse = "\n"
    )
  )
}


# ================================================================
# 5. IMPORT ROUTE DATA
# ================================================================

cat("\n")
cat("============================================================\n")
cat("IMPORT CSB ROUTE DATA\n")
cat("============================================================\n")

csbComplet <- fread(
  route_file
)

cat(
  "Number of route records:",
  format(nrow(csbComplet), big.mark = ","),
  "\n"
)


# ------------------------------------------------
# Check required columns
# ------------------------------------------------

required_route_columns <- c(
  "row",
  "track",
  "distance",
  "slope"
)

missing_route_columns <- setdiff(
  required_route_columns,
  names(csbComplet)
)

if (length(missing_route_columns) > 0) {
  
  stop(
    "Missing route column(s): ",
    paste(
      missing_route_columns,
      collapse = ", "
    )
  )
}


# ================================================================
# 6. STANDARDIZE SEGMENT DISTANCE
# ================================================================

# Original workflow:
# distances >= 99.9 m are considered 100 m.

csbComplet[
  is.finite(distance) & distance >= 99.9,
  distance := 100
]


# ================================================================
# 7. IMPORT LAND-COVER DATA
# ================================================================

cat("\n")
cat("============================================================\n")
cat("IMPORT LAND-COVER DATA\n")
cat("============================================================\n")

csbCompletLandcover <- fread(
  landcover_file
)

cat(
  "Number of land-cover records:",
  format(nrow(csbCompletLandcover), big.mark = ","),
  "\n"
)


# ------------------------------------------------
# Check row identifier
# ------------------------------------------------

if (!"row" %in% names(csbCompletLandcover)) {
  
  stop(
    "The land-cover table must contain a column named 'row'."
  )
}


# ================================================================
# 8. ADD SAVANE ARBOREE FOR MISSING ROUTES
# ================================================================

# Routes absent from the land-cover table.
#
# Their complete segment distance is initially assigned to
# savane_arboree. The original savane_arboree is subsequently
# recalculated below from the remaining land-cover classes.

missing_rows <- csbComplet[
  !row %in% csbCompletLandcover$row,
  .(
    row,
    distance
  )
]


# Rename distance -> savane_arboree
setnames(
  missing_rows,
  "distance",
  "savane_arboree"
)


# ================================================================
# 9. CONVERT LAND-COVER DATA TO LONG FORMAT
# ================================================================

landcover_columns <- setdiff(
  names(csbCompletLandcover),
  "row"
)

if (length(landcover_columns) == 0) {
  
  stop(
    "No land-cover columns were found."
  )
}


csbLandcoverLong <- melt(
  csbCompletLandcover,
  id.vars = "row",
  measure.vars = landcover_columns,
  variable.name = "variable",
  value.name = "value"
)


# ------------------------------------------------
# Convert missing rows to same structure
# ------------------------------------------------

if (nrow(missing_rows) > 0) {
  
  csbLandcoverSavaneeLong <- melt(
    missing_rows,
    id.vars = "row",
    variable.name = "variable",
    value.name = "value"
  )
  
} else {
  
  csbLandcoverSavaneeLong <- data.table(
    row = numeric(0),
    variable = character(0),
    value = numeric(0)
  )
}


# ================================================================
# 10. COMBINE LAND-COVER DATA
# ================================================================

csbLandcoverWithSavanee <- rbindlist(
  list(
    csbLandcoverLong,
    csbLandcoverSavaneeLong
  ),
  use.names = TRUE,
  fill = TRUE
)


# ================================================================
# 11. RECONSTRUCT LAND-COVER TABLE
# ================================================================

csbLandcoverWithSavanee <- dcast(
  csbLandcoverWithSavanee,
  row ~ variable,
  value.var = "value",
  fun.aggregate = sum,
  fill = 0
)


# ================================================================
# 12. MERGE ROUTE AND LAND-COVER DATA
# ================================================================

csbCompletWithLandcover <- merge(
  csbLandcoverWithSavanee,
  csbComplet,
  by = "row",
  all = FALSE
)


# ================================================================
# 13. CALCULATE ORIGINAL SAVANE ARBOREE
# ================================================================

# Identify the land-cover columns that were originally measured.

original_landcover_cols <- intersect(
  c(
    "savane_arboree",
    "savane_arbustive",
    "savane_herbeuse",
    "foret",
    "sol_nu",
    "riziere",
    "sable",
    "Tanne",
    "zone_humide",
    "eau"
  ),
  names(csbCompletWithLandcover)
)


# ------------------------------------------------
# Calculate total known land-cover distance
# ------------------------------------------------

if (length(original_landcover_cols) > 0) {
  
  known_landcover_distance <- rowSums(
    csbCompletWithLandcover[
      ,
      ..original_landcover_cols
    ],
    na.rm = TRUE
  )
  
} else {
  
  known_landcover_distance <- rep(
    0,
    nrow(csbCompletWithLandcover)
  )
}


# ------------------------------------------------
# Original savane arboree
# ------------------------------------------------

csbCompletWithLandcover[
  ,
  savane_arboree.original :=
    pmax(
      distance - known_landcover_distance,
      0
    )
]


# ================================================================
# 14. CREATE ANALYSIS DATASET
# ================================================================

csbData <- copy(
  csbCompletWithLandcover
)


# ================================================================
# 15. SLOPE CLASSIFICATION
# ================================================================

csbData[
  ,
  categoryslope := cut(
    slope,
    breaks = c(
      0,
      30,
      70,
      100,
      150
    ),
    include.lowest = TRUE
  )
]


csbData[
  ,
  typeslope := fcase(
    
    categoryslope == "[0,30]",
    "Horizontal",
    
    categoryslope == "(30,70]",
    "Moderate slopes",
    
    categoryslope == "(70,100]",
    "Strong slopes",
    
    categoryslope == "(100,150]",
    "Street slopes",
    
    default = NA_character_
  )
]


# ================================================================
# 16. TRAVEL TRACK INFORMATION
# ================================================================

# All CSB routes correspond to farmers.
csbData[
  ,
  individual := "paysan"
]


# Ensure track is character.
csbData[
  ,
  track := as.character(track)
]


# ================================================================
# 17. DISTANCE CALCULATIONS
# ================================================================

# Convert metres -> kilometres.
csbData[
  ,
  distance.km := distance / 1000
]


# Order before calculating cumulative distance.
setorder(
  csbData,
  track,
  row
)


# Cumulative distance along each track.
csbData[
  ,
  distance.origine := cumsum(
    distance.km
  ),
  by = track
]


# ================================================================
# 18. DISTANCE CATEGORY
# ================================================================

var.maxDistance <- round(
  max(
    csbData$distance.origine,
    na.rm = TRUE
  ),
  digits = 2
)


if (!is.finite(var.maxDistance)) {
  
  stop(
    "Unable to calculate maximum route distance."
  )
}


cat(
  "Maximum CSB route distance:",
  var.maxDistance,
  "km\n"
)


# ------------------------------------------------
# Create distance categories
# ------------------------------------------------

if (var.maxDistance <= 13) {
  
  csbData[
    ,
    categorydistance := cut(
      distance.origine,
      breaks = c(
        0,
        13
      ),
      include.lowest = TRUE
    )
  ]
  
} else {
  
  csbData[
    ,
    categorydistance := cut(
      distance.origine,
      breaks = c(
        0,
        13,
        var.maxDistance
      ),
      include.lowest = TRUE
    )
  ]
}


# ================================================================
# 19. LAND-COVER CLASSIFICATION
# ================================================================

landcover_cols <- intersect(
  c(
    "savane_arboree.original",
    "savane_arboree",
    "savane_arbustive",
    "savane_herbeuse",
    "foret",
    "sol_nu",
    "riziere",
    "sable",
    "Tanne",
    "zone_humide",
    "eau"
  ),
  names(csbData)
)


if (length(landcover_cols) == 0) {
  
  stop(
    "No land-cover variables were found in the CSB dataset."
  )
}


# ------------------------------------------------
# Select land-cover data
# ------------------------------------------------

csbDataLandcover <- csbData[
  ,
  c(
    "row",
    landcover_cols
  ),
  with = FALSE
]


# ------------------------------------------------
# Wide -> long
# ------------------------------------------------

melt.csbData <- melt(
  csbDataLandcover,
  id.vars = "row",
  variable.name = "variable",
  value.name = "value"
)


# Ensure character variable names.
melt.csbData[
  ,
  variable := as.character(variable)
]


# ================================================================
# 20. ASSIGN SIMPLIFIED LAND-COVER CATEGORIES
# ================================================================

melt.csbData[
  ,
  landcover := fcase(
    
    variable %in% c(
      "savane_arboree.original",
      "savane_arboree",
      "savane_arbustive",
      "savane_herbeuse"
    ),
    "Savane_Arboree",
    
    variable == "foret",
    "Foret_dense",
    
    variable == "sol_nu",
    "Zone_Habitation",
    
    variable %in% c(
      "riziere",
      "sable",
      "Tanne",
      "zone_humide"
    ),
    "Riziere",
    
    variable == "eau",
    "Eau_de_surface",
    
    default = NA_character_
  )
]


# Remove variables that were not classified.
melt.csbData <- melt.csbData[
  !is.na(landcover)
]


# ================================================================
# 21. AGGREGATE LAND-COVER CATEGORIES
# ================================================================

cast.csbData <- dcast(
  melt.csbData,
  row ~ landcover,
  value.var = "value",
  fun.aggregate = sum,
  fill = 0
)


# ================================================================
# 22. ENSURE ALL REQUIRED LAND-COVER CATEGORIES EXIST
# ================================================================

required_landcover_categories <- c(
  "Savane_Arboree",
  "Foret_dense",
  "Zone_Habitation",
  "Riziere",
  "Eau_de_surface"
)


for (variable in required_landcover_categories) {
  
  if (!variable %in% names(cast.csbData)) {
    
    cast.csbData[
      ,
      (variable) := 0
    ]
  }
}


# ================================================================
# 23. MERGE LAND-COVER CATEGORIES WITH ROUTES
# ================================================================

csbData.V2 <- merge(
  csbData,
  cast.csbData,
  by = "row",
  all.x = TRUE
)


# ================================================================
# 24. IDENTIFY MAIN LAND-COVER CATEGORY
# ================================================================

landcover_matrix <- as.matrix(
  csbData.V2[
    ,
    ..required_landcover_categories
  ]
)


# ------------------------------------------------
# Main category = category representing >50%
# ------------------------------------------------

main.cat <- apply(
  landcover_matrix,
  1,
  function(x) {
    
    selected <- which(
      is.finite(x) &
        x > 50
    )
    
    if (length(selected) == 0) {
      
      return(NA_integer_)
      
    }
    
    selected[1]
  }
)


# ------------------------------------------------
# Convert missing categories to 0 = Mixte
# ------------------------------------------------

csbData.V2[
  ,
  main.cat.temp := main.cat
]

csbData.V2[
  is.na(main.cat.temp),
  main.cat.temp := 0L
]


# ================================================================
# 25. CREATE OCCUPATION VARIABLE
# ================================================================

csbData.V2[
  ,
  occupation := fcase(
    
    main.cat.temp == 1,
    "Savane_Arboree",
    
    main.cat.temp == 2,
    "Foret_dense",
    
    main.cat.temp == 3,
    "Zone_Habitation",
    
    main.cat.temp == 4,
    "Riziere",
    
    main.cat.temp == 5,
    "Eau_de_surface",
    
    main.cat.temp == 0, 
    "Mixte",
    
    default = "Mixte"
  )
]


# ================================================================
# 26. PREPARE SLOPE FOR GAM MODEL
# ================================================================

# Original model:
# slope was divided by 15 before prediction.

csbData.V2[
  ,
  slope := as.numeric(slope) / 15
]


# ================================================================
# 27. LOAD GAM MODEL DATA
# ================================================================

cat("\n")
cat("============================================================\n")
cat("LOAD GAM MODEL\n")
cat("============================================================\n")


model.test <- readRDS(
  model_file
)


# Ensure data.table.
setDT(
  model.test
)


# ================================================================
# 28. CHECK MODEL VARIABLES
# ================================================================

required_model_columns <- c(
  "speed",
  "slope",
  "rain",
  "categorydistance",
  "occupation",
  "individual"
)


missing_model_columns <- setdiff(
  required_model_columns,
  names(model.test)
)


if (length(missing_model_columns) > 0) {
  
  stop(
    "Missing model column(s): ",
    paste(
      missing_model_columns,
      collapse = ", "
    )
  )
}


# ================================================================
# 29. PREPARE DISTANCE CATEGORY FOR MODEL
# ================================================================

if (round(var.maxDistance, 1) <= 13) {
  
  labelMaxDistance <- paste0(
    "[0,",
    round(var.maxDistance, 1),
    "]"
  )
  
  
  csbData.V2[
    ,
    categorydistance := as.character(
      categorydistance
    )
  ]
  
  
  csbData.V2[
    categorydistance == labelMaxDistance,
    categorydistance := "[0,13]"
  ]
  
} else {
  
  csbData.V2[
    ,
    categorydistance := as.character(
      cut(
        distance.origine,
        breaks = c(
          0,
          13,
          round(var.maxDistance, 1)
        ),
        include.lowest = TRUE
      )
    )
  ]
  
  
  labelMaxDistance <- paste0(
    "(13,",
    round(var.maxDistance, 1),
    "]"
  )
  
  
  # Preserve original model convention.
  csbData.V2[
    categorydistance == labelMaxDistance,
    categorydistance := "(13,22.9]"
  ]
}


# ================================================================
# 30. MATCH MODEL FACTOR LEVELS
# ================================================================

model_distance_levels <- levels(
  factor(
    model.test$categorydistance
  )
)


csbData.V2[
  ,
  categorydistance := factor(
    categorydistance,
    levels = model_distance_levels
  )
]


# ------------------------------------------------
# Check for unknown distance categories
# ------------------------------------------------

if (anyNA(csbData.V2$categorydistance)) {
  
  n_invalid_distance_category <- sum(
    is.na(csbData.V2$categorydistance)
  )
  
  warning(
    n_invalid_distance_category,
    " route segments have a categorydistance ",
    "not present in the GAM model and will not be predicted."
  )
}


# ================================================================
# 31. MATCH OCCUPATION FACTOR LEVELS
# ================================================================

model_occupation_levels <- levels(
  factor(
    model.test$occupation
  )
)


csbData.V2[
  ,
  occupation := factor(
    occupation,
    levels = model_occupation_levels
  )
]


# ================================================================
# 32. MATCH INDIVIDUAL FACTOR LEVELS
# ================================================================

model_individual_levels <- levels(
  factor(
    model.test$individual
  )
)


csbData.V2[
  ,
  individual := factor(
    individual,
    levels = model_individual_levels
  )
]


# ================================================================
# 33. CHECK FACTOR COMPATIBILITY
# ================================================================

cat("\nModel factor levels:\n")

cat(
  "categorydistance:",
  paste(
    model_distance_levels,
    collapse = ", "
  ),
  "\n"
)

cat(
  "occupation:",
  paste(
    model_occupation_levels,
    collapse = ", "
  ),
  "\n"
)

cat(
  "individual:",
  paste(
    model_individual_levels,
    collapse = ", "
  ),
  "\n"
)


# ================================================================
# 34. FIT GAM MODEL
# ================================================================

cat("\n")
cat("============================================================\n")
cat("FIT GAM MODEL\n")
cat("============================================================\n")


modelPredict <- gam(
  speed ~
    s(slope) +
    rain +
    categorydistance +
    occupation +
    individual,
  data = model.test
)


print(
  summary(modelPredict)
)


# ================================================================
# 35. PREPARE PREDICTION DATASET
# ================================================================

csbData.model.temp <- csbData.V2[
  ,
  .(
    track,
    distance,
    slope,
    rain = 0,
    categorydistance,
    occupation,
    individual
  )
]


# ================================================================
# 36. REMOVE INVALID MODEL CATEGORIES
# ================================================================

csbData.model.temp <- csbData.model.temp[
  !is.na(categorydistance) &
    !is.na(occupation) &
    !is.na(individual)
]


cat(
  "\nPrediction records after factor validation:",
  format(
    nrow(csbData.model.temp),
    big.mark = ","
  ),
  "\n"
)


# ================================================================
# 37. RAINFALL MINIMUM
# ================================================================

cat("\n")
cat("Predicting minimum rainfall scenario...\n")


rain_min <- min(
  model.test$rain,
  na.rm = TRUE
)


csbData.model.temp[
  ,
  rain := rain_min
]


csbData.model.temp[
  ,
  rain.min := rain
]


model.out.fit.min <- predict(
  modelPredict,
  newdata = csbData.model.temp,
  se.fit = TRUE
)


# ------------------------------------------------
# Speed prediction
# ------------------------------------------------

csbData.model.temp[
  ,
  speed.rain.min :=
    as.numeric(
      model.out.fit.min$fit
    )
]


# ------------------------------------------------
# Confidence interval
# ------------------------------------------------

csbData.model.temp[
  ,
  speed.rain.min.lower :=
    speed.rain.min -
    1.96 *
    as.numeric(
      model.out.fit.min$se.fit
    )
]


csbData.model.temp[
  ,
  speed.rain.min.upper :=
    speed.rain.min +
    1.96 *
    as.numeric(
      model.out.fit.min$se.fit
    )
]


# ================================================================
# 38. RAINFALL MAXIMUM
# ================================================================

cat(
  "Predicting maximum rainfall scenario...\n"
)


rain_max <- max(
  model.test$rain,
  na.rm = TRUE
)


csbData.model.temp[
  ,
  rain := rain_max
]


csbData.model.temp[
  ,
  rain.max := rain
]


model.out.fit.max <- predict(
  modelPredict,
  newdata = csbData.model.temp,
  se.fit = TRUE
)


# ------------------------------------------------
# Speed prediction
# ------------------------------------------------

csbData.model.temp[
  ,
  speed.rain.max :=
    as.numeric(
      model.out.fit.max$fit
    )
]


# ------------------------------------------------
# Confidence interval
# ------------------------------------------------

csbData.model.temp[
  ,
  speed.rain.max.lower :=
    speed.rain.max -
    1.96 *
    as.numeric(
      model.out.fit.max$se.fit
    )
]


csbData.model.temp[
  ,
  speed.rain.max.upper :=
    speed.rain.max +
    1.96 *
    as.numeric(
      model.out.fit.max$se.fit
    )
]


# ================================================================
# 39. DISPLAY RAINFALL VALUES
# ================================================================

cat("\n")
cat("Rainfall scenarios:\n")
cat(
  "Minimum rainfall:",
  rain_min,
  "\n"
)
cat(
  "Maximum rainfall:",
  rain_max,
  "\n"
)


# ================================================================
# 40. REMOVE INVALID SPEED ESTIMATES
# ================================================================

speed_cols <- c(
  "speed.rain.min",
  "speed.rain.min.lower",
  "speed.rain.min.upper",
  "speed.rain.max",
  "speed.rain.max.lower",
  "speed.rain.max.upper"
)


# ------------------------------------------------
# Check columns
# ------------------------------------------------

missing_speed_cols <- setdiff(
  speed_cols,
  names(csbData.model.temp)
)


if (length(missing_speed_cols) > 0) {
  
  stop(
    "Missing speed column(s): ",
    paste(
      missing_speed_cols,
      collapse = ", "
    )
  )
}


# ------------------------------------------------
# IMPORTANT:
#
# Keep a row ONLY when ALL speed estimates are:
#   1. finite
#   2. strictly > 0
#
# This replaces the old filter_at()/any_vars()
# workflow.
# ------------------------------------------------

valid_speed <- csbData.model.temp[
  ,
  Reduce(
    `&`,
    lapply(
      .SD,
      function(x) {
        is.finite(x) & x > 0
      }
    )
  ),
  .SDcols = speed_cols
]


n_before_speed_filter <- nrow(
  csbData.model.temp
)


csbData.model.temp <- csbData.model.temp[
  valid_speed
]


n_after_speed_filter <- nrow(
  csbData.model.temp
)


cat("\n")
cat("Speed validation:\n")
cat(
  "Records before filtering:",
  format(
    n_before_speed_filter,
    big.mark = ","
  ),
  "\n"
)

cat(
  "Records removed:",
  format(
    n_before_speed_filter -
      n_after_speed_filter,
    big.mark = ","
  ),
  "\n"
)

cat(
  "Records retained:",
  format(
    n_after_speed_filter,
    big.mark = ","
  ),
  "\n"
)


# ================================================================
# 41. CONVERT DISTANCE TO KILOMETRES
# ================================================================

csbData.model.temp[
  ,
  distance := distance / 1000
]


# ================================================================
# 42. CALCULATE TRAVEL TIME
# ================================================================

# Formula:
#
#   time (minutes)
#       =
#   distance (km)
#       /
#   speed (km/h)
#       *
#   60
#
# ------------------------------------------------


csbData.model.temp[
  ,
  time.rain.min :=
    distance /
    speed.rain.min *
    60
]


csbData.model.temp[
  ,
  time.rain.min.lower :=
    distance /
    speed.rain.min.lower *
    60
]


csbData.model.temp[
  ,
  time.rain.min.upper :=
    distance /
    speed.rain.min.upper *
    60
]


csbData.model.temp[
  ,
  time.rain.max :=
    distance /
    speed.rain.max *
    60
]


csbData.model.temp[
  ,
  time.rain.max.lower :=
    distance /
    speed.rain.max.lower *
    60
]


csbData.model.temp[
  ,
  time.rain.max.upper :=
    distance /
    speed.rain.max.upper *
    60
]


# ================================================================
# 43. CLEAN NON-FINITE TRAVEL TIMES
# ================================================================

time_cols <- c(
  "time.rain.min",
  "time.rain.min.lower",
  "time.rain.min.upper",
  "time.rain.max",
  "time.rain.max.lower",
  "time.rain.max.upper"
)


# Replace Inf / -Inf with NA.
csbData.model.temp[
  ,
  (time_cols) := lapply(
    .SD,
    function(x) {
      
      x[
        !is.finite(x)
      ] <- NA_real_
      
      x
    }
  ),
  .SDcols = time_cols
]


# ================================================================
# 44. AGGREGATE TRAVEL TIME BY TRACK
# ================================================================

output.csbData.model <- csbData.model.temp[
  ,
  c(
    list(
      distance = sum(
        distance,
        na.rm = TRUE
      )
    ),
    lapply(
      .SD,
      sum,
      na.rm = TRUE
    )
  ),
  by = track,
  .SDcols = time_cols
]


# ================================================================
# 45. EXPORT CSB ROUTE TRAVEL TIME
# ================================================================

fwrite(
  output.csbData.model,
  time_output_file
)


cat("\n")
cat(
  "CSB route travel-time output:\n"
)
cat(
  time_output_file,
  "\n"
)


# ================================================================
# 46. IMPORT HOUSEHOLD-TO-CSB DISTANCES
# ================================================================

cat("\n")
cat("============================================================\n")
cat("MERGE WITH HOUSEHOLD-TO-CSB DISTANCES\n")
cat("============================================================\n")


csbCompletDistanceMenage <- fread(
  distance_household_file
)


# ------------------------------------------------
# Check household ID
# ------------------------------------------------

if (!"ID_menage" %in% names(csbCompletDistanceMenage)) {
  
  stop(
    "The household-distance table must contain 'ID_menage'."
  )
}


# ================================================================
# 47. CREATE TRACK IDENTIFIER
# ================================================================

csbCompletDistanceMenage[
  ,
  track := paste0(
    "track",
    ID_menage
  )
]


# ================================================================
# 48. MERGE TRAVEL TIME AND HOUSEHOLD DATA
# ================================================================

csbCompletAll <- merge(
  output.csbData.model,
  csbCompletDistanceMenage,
  by = "track",
  all = FALSE
)


# ================================================================
# 49. RENAME TIME VARIABLES
# ================================================================

rename_time_old <- c(
  "time.rain.min",
  "time.rain.min.lower",
  "time.rain.min.upper",
  "time.rain.max",
  "time.rain.max.lower",
  "time.rain.max.upper"
)


rename_time_new <- c(
  "timeWithoutRain",
  "timeWithoutRainLower",
  "timeWithoutRainUpper",
  "timeWithRain",
  "timeWithRainLower",
  "timeWithRainUpper"
)


setnames(
  csbCompletAll,
  old = rename_time_old,
  new = rename_time_new,
  skip_absent = TRUE
)


# ================================================================
# 50. EXPORT FINAL DATASET
# ================================================================

fwrite(
  csbCompletAll,
  final_output_file
)


# ================================================================
# 51. FINAL SUMMARY
# ================================================================

cat("\n")
cat("============================================================\n")
cat("PROCESS COMPLETED\n")
cat("============================================================\n")


cat(
  "Route records:",
  format(
    nrow(csbComplet),
    big.mark = ","
  ),
  "\n"
)


cat(
  "Prediction records:",
  format(
    nrow(csbData.model.temp),
    big.mark = ","
  ),
  "\n"
)


cat(
  "Tracks with predicted travel time:",
  format(
    nrow(output.csbData.model),
    big.mark = ","
  ),
  "\n"
)


cat(
  "Tracks after household merge:",
  format(
    nrow(csbCompletAll),
    big.mark = ","
  ),
  "\n"
)


cat("\n")
cat("Rainfall scenarios:\n")

cat(
  "- Minimum:",
  rain_min,
  "\n"
)

cat(
  "- Maximum:",
  rain_max,
  "\n"
)


cat("\n")
cat("Output files:\n")

cat(
  "- ",
  time_output_file,
  "\n",
  sep = ""
)

cat(
  "- ",
  final_output_file,
  "\n",
  sep = ""
)

cat("\n")
cat("============================================================\n")
