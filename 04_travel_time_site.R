# ================================================================
# SITE ROUTE TIME PREDICTION - MANANJARY (MNJ)
#
# Purpose:
#   1. Combine route data with land-cover information.
#   2. Classify slope, distance and land-cover categories.
#   3. Predict walking speed using a GAM.
#   4. Estimate travel time under minimum and maximum rainfall.
#   5. Aggregate travel time by track.
#   6. Merge predicted travel time with household-to-site distances.
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

setDTthreads(14)

# Input files
route_file <- "./parcours/pathSiteComplet_MNJ_Complet.csv"

landcover_file <- "./landcover/pathSiteMNJCompletLandcoverdTransposer.csv"

model_file <- "./model_pied.rds"

distance_household_file <-
  "./table/distance_menagesToChefLieuSiteMnjComplet.csv"

# Output files
time_output_file <-
  "./time/pathSiteMNJCompletTimeWithRainAndWithout_With_SdeFit.csv"

final_output_file <-
  "./time/pathSiteMNJCompletTimeAll_With_SdeFit.csv"

# Create output directory
if (!dir.exists("./time")) {
  dir.create("./time", recursive = TRUE)
}


# ================================================================
# 4. IMPORT ROUTE DATA
# ================================================================

cat("\n")
cat("============================================================\n")
cat("IMPORT ROUTE DATA\n")
cat("============================================================\n")

siteComplet <- fread(route_file)

cat(
  "Number of route records:",
  format(nrow(siteComplet), big.mark = ","),
  "\n"
)


# ================================================================
# 5. STANDARDIZE SEGMENT DISTANCE
# ================================================================

# Original workflow rounds distances >= 99.8 m to 100 m.
siteComplet[
  distance >= 99.8,
  distance := 100
]


# ================================================================
# 6. IMPORT LAND-COVER DATA
# ================================================================

cat("\n")
cat("============================================================\n")
cat("IMPORT LAND-COVER DATA\n")
cat("============================================================\n")

siteCompletLandcover <- fread(landcover_file)

cat(
  "Number of land-cover records:",
  format(nrow(siteCompletLandcover), big.mark = ","),
  "\n"
)


# ================================================================
# 7. ADD SAVANE ARBOREE FOR ROUTES WITHOUT LAND-COVER DATA
# ================================================================

# Identify rows that are not present in the land-cover table.
missing_rows <- siteComplet[
  !row %in% siteCompletLandcover$row,
  .(row, distance)
]

# Rename distance to savane_arboree.
setnames(
  missing_rows,
  "distance",
  "savane_arboree"
)


# ================================================================
# 8. COMBINE LAND-COVER DATA
# ================================================================

# Convert land-cover table from wide to long format.
siteLandcoverLong <- melt(
  siteCompletLandcover,
  id.vars = "row",
  variable.name = "variable",
  value.name = "value"
)

# Convert missing rows to the same structure.
missingRowsLong <- melt(
  missing_rows,
  id.vars = "row",
  variable.name = "variable",
  value.name = "value"
)

# Combine both sources.
siteLandcoverWithSavanee <- rbindlist(
  list(
    siteLandcoverLong,
    missingRowsLong
  ),
  use.names = TRUE,
  fill = TRUE
)


# ================================================================
# 9. RECONSTRUCT LAND-COVER TABLE
# ================================================================

siteLandcoverWithSavanee <- dcast(
  siteLandcoverWithSavanee,
  row ~ variable,
  value.var = "value",
  fun.aggregate = sum,
  fill = 0
)


# ================================================================
# 10. MERGE ROUTE AND LAND-COVER DATA
# ================================================================

siteCompletWithLandcover <- merge(
  siteLandcoverWithSavanee,
  siteComplet,
  by = "row",
  all = FALSE
)


# ================================================================
# 11. CREATE ANALYSIS DATASET
# ================================================================

siteData <- copy(siteCompletWithLandcover)


# ================================================================
# 12. SLOPE CLASSIFICATION
# ================================================================

siteData[
  ,
  categoryslope := cut(
    slope,
    breaks = c(0, 30, 70, 100, 150),
    include.lowest = TRUE
  )
]

siteData[
  ,
  typeslope := fifelse(
    categoryslope == "[0,30]",
    "Horizontal",
    fifelse(
      categoryslope == "(30,70]",
      "Moderate slopes",
      fifelse(
        categoryslope == "(70,100]",
        "Strong slopes",
        fifelse(
          categoryslope == "(100,150]",
          "Street slopes",
          NA_character_
        )
      )
    )
  )
]


# ================================================================
# 13. TRAVEL TRACK INFORMATION
# ================================================================

# All tracks correspond to farmers.
siteData[
  ,
  individual := "paysan"
]

# Convert track to character.
siteData[
  ,
  track := as.character(track)
]


# ================================================================
# 14. DISTANCE CALCULATIONS
# ================================================================

# Convert segment distance from metres to kilometres.
siteData[
  ,
  distance.km := distance / 1000
]

# Calculate cumulative distance along each track.
setorder(
  siteData,
  track,
  row
)

siteData[
  ,
  distance.origine := cumsum(distance.km),
  by = track
]


# ================================================================
# 15. DISTANCE CATEGORY
# ================================================================

var.maxDistance <- round(
  max(siteData$distance.origine, na.rm = TRUE),
  digits = 2
)

cat(
  "Maximum route distance:",
  var.maxDistance,
  "km\n"
)

if (var.maxDistance <= 13) {
  
  siteData[
    ,
    categorydistance := cut(
      distance.origine,
      breaks = c(0, 13),
      include.lowest = TRUE
    )
  ]
  
} else {
  
  siteData[
    ,
    categorydistance := cut(
      distance.origine,
      breaks = c(0, 13, var.maxDistance),
      include.lowest = TRUE
    )
  ]
}


# ================================================================
# 16. LAND-COVER CLASSIFICATION
# ================================================================

landcover_cols <- c(
  "savane_arbustive",
  "savane_herbeuse",
  "savane_arboree",
  "foret",
  "sol_nu",
  "riziere",
  "sable",
  "Tanne",
  "zone_humide",
  "eau"
)

# Keep only land-cover columns that actually exist.
landcover_cols <- intersect(
  landcover_cols,
  names(siteData)
)

siteDataLandcover <- siteData[
  ,
  c("row", landcover_cols),
  with = FALSE
]

# Convert from wide to long.
melt.siteData <- melt(
  siteDataLandcover,
  id.vars = "row",
  variable.name = "variable",
  value.name = "value"
)


# ------------------------------------------------
# Assign simplified land-cover categories
# ------------------------------------------------

melt.siteData[
  ,
  landcover := fcase(
    
    variable %in% c(
      "savane_arbustive",
      "savane_herbeuse",
      "savane_arboree"
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


# Remove variables not assigned to a category.
melt.siteData <- melt.siteData[
  !is.na(landcover)
]


# ================================================================
# 17. AGGREGATE LAND-COVER CATEGORIES
# ================================================================

cast.siteData <- dcast(
  melt.siteData,
  row ~ landcover,
  value.var = "value",
  fun.aggregate = sum,
  fill = 0
)


# ================================================================
# 18. MERGE LAND-COVER CATEGORIES WITH ROUTES
# ================================================================

siteData.V2 <- merge(
  siteData,
  cast.siteData,
  by = "row",
  all.x = TRUE
)


# ================================================================
# 19. ENSURE ALL LAND-COVER VARIABLES EXIST
# ================================================================

required_landcover_categories <- c(
  "Savane_Arboree",
  "Foret_dense",
  "Zone_Habitation",
  "Riziere",
  "Eau_de_surface"
)

for (variable in required_landcover_categories) {
  
  if (!variable %in% names(siteData.V2)) {
    siteData.V2[
      ,
      (variable) := 0
    ]
  }
}


# ================================================================
# 20. IDENTIFY MAIN LAND-COVER CATEGORY
# ================================================================

landcover_matrix <- as.matrix(
  siteData.V2[
    ,
    ..required_landcover_categories
  ]
)

# Identify categories representing >50% of the segment.
main.cat <- apply(
  landcover_matrix,
  1,
  function(x) {
    
    selected <- which(x > 50)
    
    if (length(selected) == 0) {
      return(NA_integer_)
    }
    
    selected[1]
  }
)

# Replace undefined dominant categories with "Mixte".
siteData.V2[
  ,
  main.cat.temp := main.cat
]

siteData.V2[
  is.na(main.cat.temp),
  main.cat.temp := 0
]


# ================================================================
# 21. CREATE OCCUPATION VARIABLE
# ================================================================

siteData.V2[
  ,
  occupation := fcase(
    
    main.cat.temp == "1",
    "Savane_Arboree",
    
    main.cat.temp == "2",
    "Foret_dense",
    
    main.cat.temp == "3",
    "Zone_Habitation",
    
    main.cat.temp == "4",
    "Riziere",
    
    main.cat.temp == "5",
    "Eau_de_surface",
    
    main.cat.temp == 0, 
    "Mixte",
    
    default = "Mixte"
  )
]


# ================================================================
# 22. PREPARE SLOPE FOR THE MODEL
# ================================================================

siteData.V2[
  ,
  slope := as.numeric(slope) / 15
]


# ================================================================
# 23. LOAD GAM MODEL DATA
# ================================================================

cat("\n")
cat("============================================================\n")
cat("LOAD GAM MODEL\n")
cat("============================================================\n")

model.test <- readRDS(
  model_file
)


# ================================================================
# 24. PREPARE DISTANCE CATEGORY FOR PREDICTION
# ================================================================

if (round(var.maxDistance, 1) <= 13) {
  
  labelMaxDistance <- paste0(
    "[0,",
    round(var.maxDistance, 1),
    "]"
  )
  
  siteData.V2[
    ,
    categorydistance := as.character(categorydistance)
  ]
  
  siteData.V2[
    categorydistance == labelMaxDistance,
    categorydistance := "[0,13]"
  ]
  
} else {
  
  siteData.V2[
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
  
  siteData.V2[
    categorydistance == labelMaxDistance,
    categorydistance := "(13,22.9]"
  ]
}


# Convert distance category to factor.
siteData.V2[
  ,
  categorydistance := factor(categorydistance)
]


# ================================================================
# 25. PREPARE MODEL FACTORS
# ================================================================

# Convert to data.table
setDT(model.test)

model.test[
  ,
  categorydistance := factor(
    categorydistance
  )
]

# Keep the same factor levels between training and prediction.
common_levels <- levels(model.test$categorydistance)

siteData.V2[
  ,
  categorydistance := factor(
    categorydistance,
    levels = common_levels
  )
]


# ================================================================
# 26. FIT GAM MODEL
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

print(summary(modelPredict))


# ================================================================
# 27. PREPARE DATA FOR PREDICTION
# ================================================================

siteData.model.temp <- siteData.V2[
  ,
  .(
    track,
    distance,
    slope,
    rain = 0,
    categorydistance,
    occupation,
    individual = individual
  )
]


# ================================================================
# 28. CHECK PREDICTION VARIABLES
# ================================================================

required_prediction_vars <- c(
  "track",
  "distance",
  "slope",
  "rain",
  "categorydistance",
  "occupation",
  "individual"
)

missing_prediction_vars <- setdiff(
  required_prediction_vars,
  names(siteData.model.temp)
)

if (length(missing_prediction_vars) > 0) {
  
  stop(
    "Missing prediction variable(s): ",
    paste(
      missing_prediction_vars,
      collapse = ", "
    )
  )
}


# ================================================================
# 29. RAINFALL MINIMUM
# ================================================================

cat("\n")
cat("Predicting minimum rainfall scenario...\n")

siteData.model.temp[
  ,
  rain := min(
    model.test$rain,
    na.rm = TRUE
  )
]

siteData.model.temp[
  ,
  rain.min := rain
]

model.out.fit.min <- predict(
  modelPredict,
  newdata = siteData.model.temp,
  se.fit = TRUE
)

siteData.model.temp[
  ,
  speed.rain.min := as.numeric(
    model.out.fit.min$fit
  )
]

siteData.model.temp[
  ,
  speed.rain.min.lower :=
    speed.rain.min -
    1.96 * as.numeric(model.out.fit.min$se.fit)
]

siteData.model.temp[
  ,
  speed.rain.min.upper :=
    speed.rain.min +
    1.96 * as.numeric(model.out.fit.min$se.fit)
]


# ================================================================
# 30. RAINFALL MAXIMUM
# ================================================================

cat("Predicting maximum rainfall scenario...\n")

siteData.model.temp[
  ,
  rain := max(
    model.test$rain,
    na.rm = TRUE
  )
]

siteData.model.temp[
  ,
  rain.max := rain
]

model.out.fit.max <- predict(
  modelPredict,
  newdata = siteData.model.temp,
  se.fit = TRUE
)

siteData.model.temp[
  ,
  speed.rain.max := as.numeric(
    model.out.fit.max$fit
  )
]

siteData.model.temp[
  ,
  speed.rain.max.lower :=
    speed.rain.max -
    1.96 * as.numeric(model.out.fit.max$se.fit)
]

siteData.model.temp[
  ,
  speed.rain.max.upper :=
    speed.rain.max +
    1.96 * as.numeric(model.out.fit.max$se.fit)
]


# ================================================================
# 31. REMOVE INVALID SPEED VALUES
# ================================================================

speed_cols <- c(
  "speed.rain.min",
  "speed.rain.min.lower",
  "speed.rain.min.upper",
  "speed.rain.max",
  "speed.rain.max.lower",
  "speed.rain.max.upper"
)

# Keep only rows where ALL speed estimates are:
#   - finite
#   - strictly positive

valid_speed <- Reduce(
  `&`,
  lapply(
    siteData.model.temp[, ..speed_cols],
    function(x) {
      is.finite(x) & x > 0
    }
  )
)

# Keep rows for which all speed estimates are strictly positive.
siteData.model.temp <- siteData.model.temp[
  valid_speed
]

# ================================================================
# 32. CALCULATE TRAVEL TIME
# ================================================================

# Convert distance from metres to kilometres.
siteData.model.temp[
  ,
  distance := distance / 1000
]

# Travel time:
# time (minutes) = distance (km) / speed (km/h) * 60

siteData.model.temp[
  ,
  time.rain.min :=
    distance / speed.rain.min * 60
]

siteData.model.temp[
  ,
  time.rain.min.lower :=
    distance / speed.rain.min.lower * 60
]

siteData.model.temp[
  ,
  time.rain.min.upper :=
    distance / speed.rain.min.upper * 60
]

siteData.model.temp[
  ,
  time.rain.max :=
    distance / speed.rain.max * 60
]

siteData.model.temp[
  ,
  time.rain.max.lower :=
    distance / speed.rain.max.lower * 60
]

siteData.model.temp[
  ,
  time.rain.max.upper :=
    distance / speed.rain.max.upper * 60
]


# ================================================================
# 33. REMOVE NON-FINITE TRAVEL TIMES
# ================================================================

time_cols <- c(
  "time.rain.min",
  "time.rain.min.lower",
  "time.rain.min.upper",
  "time.rain.max",
  "time.rain.max.lower",
  "time.rain.max.upper"
)

siteData.model.temp[
  ,
  (time_cols) := lapply(
    .SD,
    function(x) {
      x[!is.finite(x)] <- NA_real_
      x
    }
  ),
  .SDcols = time_cols
]


# ================================================================
# 34. AGGREGATE TRAVEL TIME BY TRACK
# ================================================================

output.siteData.model <- siteData.model.temp[
  ,
  c(
    list(
      distance = sum(distance, na.rm = TRUE)
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
# 35. EXPORT ROUTE TRAVEL TIME
# ================================================================

fwrite(
  output.siteData.model,
  time_output_file
)

cat("\n")
cat("Travel-time output:\n")
cat(time_output_file, "\n")


# ================================================================
# 36. IMPORT HOUSEHOLD-TO-SITE DISTANCES
# ================================================================

cat("\n")
cat("============================================================\n")
cat("MERGE WITH HOUSEHOLD DISTANCES\n")
cat("============================================================\n")

siteCompletDistanceMenage <- fread(
  distance_household_file
)


# ================================================================
# 37. CREATE TRACK IDENTIFIER
# ================================================================

siteCompletDistanceMenage[
  ,
  track := paste0(
    "track",
    ID_menage
  )
]


# ================================================================
# 38. MERGE TRAVEL TIME AND HOUSEHOLD DATA
# ================================================================

siteCompletAll <- merge(
  output.siteData.model,
  siteCompletDistanceMenage,
  by = "track",
  all = FALSE
)


# ================================================================
# 39. RENAME TIME VARIABLES
# ================================================================

setnames(
  siteCompletAll,
  old = c(
    "time.rain.min",
    "time.rain.min.lower",
    "time.rain.min.upper",
    "time.rain.max",
    "time.rain.max.lower",
    "time.rain.max.upper"
  ),
  new = c(
    "timeWithoutRain",
    "timeWithoutRainLower",
    "timeWithoutRainUpper",
    "timeWithRain",
    "timeWithRainLower",
    "timeWithRainUpper"
  )
)


# ================================================================
# 40. EXPORT FINAL DATASET
# ================================================================

fwrite(
  siteCompletAll,
  final_output_file
)


# ================================================================
# 41. FINAL SUMMARY
# ================================================================

cat("\n")
cat("============================================================\n")
cat("PROCESS COMPLETED\n")
cat("============================================================\n")

cat(
  "Route records:",
  format(nrow(siteComplet), big.mark = ","),
  "\n"
)

cat(
  "Prediction records:",
  format(nrow(siteData.model.temp), big.mark = ","),
  "\n"
)

cat(
  "Tracks with predicted time:",
  format(nrow(output.siteData.model), big.mark = ","),
  "\n"
)

cat(
  "Tracks after household merge:",
  format(nrow(siteCompletAll), big.mark = ","),
  "\n"
)

cat("\nOutput files:\n")
cat("-", time_output_file, "\n")
cat("-", final_output_file, "\n")
