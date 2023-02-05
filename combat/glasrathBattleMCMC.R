# =========================================================================== #
#                            LIBRARIES AND SET UP                             #
# =========================================================================== #
library(dplyr)
library(rjson)

source('./combat/fx_attackRolls.R')


# =========================================================================== #
#                                 MCMC RUNS                                   #
# =========================================================================== #

# End-to-end combat with multiple rounds

# load JSON data
# condition: E(d20D) = 7.2 (0.7x); E(d20) = 10.5 (1x); E(d20A) = 13.8 (1.3x)
npcsDf <- fx_convListDF(rjson::fromJSON(file = './combat/Glasrath_Uchbur.json'))
pcsDf <- fx_convListDF(rjson::fromJSON(file = './combat/Glasrath_Alliance.json'))

# set up simulation
runRound <- TRUE
expSims <- 1
stopCond <- 10

# first print
npcsDf
pcsDf

# run rounds back-to-back until stop condition is met
while (runRound) {
  
  # run round for pcs side
  atcks_pcs <- fxW_aR_expectationRuns(atck = pcsDf, dfnd = npcsDf,
                                      maxAtckMult = 3, rounds = expSims)
  
  # update DF only if there is any successful attack on the round
  if (nrow(atcks_pcs) > 0) {
    npcsDf <- npcsDf %>%
      left_join(atcks_pcs, by = c('name' = 'tgt')) %>%
      mutate(count = ifelse(is.na(count), 0, count),
             total = ifelse(is.na(total), 0, total),
             survivors = pmax(0, round((number * hp - total) / hp, digits = 1)),
             survivors = ifelse(is.na(survivors), 0, survivors)) %>%
      mutate(number = survivors) %>%
      select(-count, -total, -survivors)
  }
  
  # run round for npcs side
  atcks_npcs <- fxW_aR_expectationRuns(atck = npcsDf, dfnd = pcsDf,
                                       maxAtckMult = 3, rounds = expSims)
  
  # update DF only if there is any successful attack on the round
  if (nrow(atcks_npcs) > 0) {
    pcsDf <- pcsDf %>%
      left_join(atcks_npcs, by = c('name' = 'tgt')) %>%
      mutate(count = ifelse(is.na(count), 0, count),
             total = ifelse(is.na(total), 0, total),
             survivors = pmax(0, round((number * hp - total) / hp, digits = 1)),
             survivors = ifelse(is.na(survivors), 0, survivors)) %>%
      mutate(number = survivors) %>%
      select(-count, -total, -survivors)
  }
  
  # print results for both sides
  print(npcsDf)
  print(pcsDf)
  
  # check if while-loops continues
  runRound <- sum(npcsDf$number) > stopCond & sum(pcsDf$number) > stopCond
  print(runRound)
}
