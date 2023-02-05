# =========================================================================== #
#                            LIBRARIES AND SET UP                             #
# =========================================================================== #
library(dplyr)
library(rjson)

source('./combat/fx_attackRolls.R')


# =========================================================================== #
#                               SCRATCH ROUNDS                                #
# =========================================================================== #

# load JSON data
# condition: E(d20D) = 7.2 (0.7x); E(d20) = 10.5 (1x); E(d20A) = 13.8 (1.3x)
npcsDf <- fx_convListDF(rjson::fromJSON(
  file = './combat/Glasrath_AssautTgt.json'))
pcsDf <- fx_convListDF(rjson::fromJSON(
  file = './combat/Glasrath_AssautPCs.json'))

fx_attackRolls(atck = npcsDf, dfnd = pcsDf, seed = NULL,
               maxAtckMult = 6, output = 'summary')
