# =========================================================================== #
#                            LIBRARIES AND SET UP                             #
# =========================================================================== #
library(dplyr)
library(rjson)

# import lists function
source('./combat/fx_attackRolls.R')

# =========================================================================== #
#                                  FUNCTION                                   #
# =========================================================================== #
factionGame <- function (factionsDF, nRounds, seed = NULL) {
  # set seed if requested by argument
  if (!is.null(seed)) { set.seed(seed) }
  
  # start rounds counter
  rounds <- 0
  
  while (rounds < nRounds) {
    # calculate round gains in development and turf
    factionsDF <- factionsDF %>%
      mutate(roll = sample(1:20, n()),
             result = roll + 10 * tier + tenacity,
             devAdd = round(result / max(result), 1),
             turfAdd = ifelse(dev + devAdd + turf > 12, turf/sum(turf), 0),
             turfNew = ifelse(turfAdd + turf < 1, 0, turfAdd + turf),
             turfNrm = round(turfNew * 12 / sum(turfNew), 1),
             devNew = ifelse(turfAdd > 0, 0, round(dev + devAdd, 1)))
    
    # create updated data frame for factions
    factionsDF <- factionsDF %>%
      select(name,
             type,
             tier,
             hold,
             turf = turfNrm,
             dev = devNew,
             tenacity)
    
    # update rounds counter
    rounds <- rounds + 1
  }
  
  return (factionsDF)
}

# =========================================================================== #
#                             Loading Factions                                #
# =========================================================================== #
regionsNames <- c('glasrath', 'linhe')

regionsList <- lapply(regionsNames, function (x) {
  fileName <- paste0('./political/factions_', x, '.json')
  fx_convListDF(rjson::fromJSON(file = fileName))
})
names(regionsList) <- regionsNames
regionsList

# =========================================================================== #
#                  Factions Simulator History Matching                        #
# =========================================================================== #
# Simulator start date is when the Glasrath Alliance was formed
simDate <- 3877.2

# Seed to use on function
useSeed <- 1

# Part 2, Chapter 1
newDate <- 3877.4

regionsList <- lapply(regionsNames, function(x) {
  factionGame(factionsDF = regionsList[[x]],
              nRounds = (newDate - simDate) * 300,
              seed = useSeed)
})
names(regionsList) <- regionsNames
regionsList
simDate <- newDate


# Part 2, Chapter 3
newDate <- 3877.52

regionsList <- lapply(regionsNames, function(x) {
  factionGame(factionsDF = regionsList[[x]],
              nRounds = (newDate - simDate) * 300,
              seed = useSeed)
})
names(regionsList) <- regionsNames
simDate <- newDate
regionsList$simDate <- simDate
regionsList

regionsListJSON <- jsonlite::toJSON(regionsList, pretty = TRUE)
write(regionsListJSON, file = './political/regionsList.json')

# =========================================================================== #
#                  Factions Simulator Future Predictions                      #
# =========================================================================== #
# New simulation date
newDate <- 3878.32

# Copy current state of factions
regionsListPred <- regionsList

# Editing current state based on PCs actions
# TIER changes if the faction is now at a higher tier of play (unlikely)
# TURF changes if the PCs helped the faction gain territory/influence
# TENACITY changes if the PCs helped increase the manpower of a given faction
regionsListPred$linhe[regionsListPred$linhe$name == 'rurkinar', 'turf']

# New prediction is made
regionsListPred <- lapply(regionsNames, function(x) {
  factionGame(factionsDF = regionsListPred[[x]],
              nRounds = (newDate - simDate) * 300)
})
names(regionsListPred) <- regionsNames
regionsListPred$simDate <- newDate

# Comparison of current and future states
regionsList
regionsListPred


