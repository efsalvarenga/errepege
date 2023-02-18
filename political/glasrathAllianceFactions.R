# =========================================================================== #
#                            LIBRARIES AND SET UP                             #
# =========================================================================== #
library(dplyr)
library(rjson)

# import lists function
source('./combat/fx_attackRolls.R')

# =========================================================================== #
#                                    ROUNDS                                   #
# =========================================================================== #

alliance <- fx_convListDF(rjson::fromJSON(
  file = './political/glasrathAllianceFactions.json'))
alliance

stopCond <- 270
rounds <- 0

while (rounds < stopCond) {
  alliance <- alliance %>%
    mutate(roll = sample(1:20, n()),
           result = roll + 10 * tier + expansion,
           devAdd = round(result / max(result), 1),
           turfAdd = ifelse(dev + devAdd + turf > 12, turf/sum(turf), 0),
           turfNew = ifelse(turfAdd + turf < 1, 0, turfAdd + turf),
           turfNrm = round(turfNew * 12 / sum(turfNew), 1),
           devNew = ifelse(turfAdd > 0, 0, round(dev + devAdd, 1)))
  
  alliance <- alliance %>%
    select(name,
           tier,
           type,
           turf = turfNrm,
           dev = devNew,
           expansion)
  
  rounds <- rounds + 1
}

alliance

