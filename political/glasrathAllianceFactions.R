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
#                             Glasrath Alliance                               #
# =========================================================================== #
alliance <- fx_convListDF(rjson::fromJSON(
  file = './political/glasrathAllianceFactions.json'))

# resolving the first 2 months while characters are preparing to leave
# to the ammuinoth
alliance <- factionGame(alliance, nRounds = 60, seed = 1)
alliance %>%
  mutate(turf = round(turf),
         dev = round(dev))

