# =========================================================================== #
#                            LIBRARIES AND SET UP                             #
# =========================================================================== #
library(dplyr)
library(rjson)

source('./combat/fx_attackRolls.R')


# =========================================================================== #
#                               SCRATCH ROUNDS                                #
# =========================================================================== #

# SMALL COMBAT
# load JSON data
# condition: E(d20D) = 7.2 (0.7x); E(d20) = 10.5 (1x); E(d20A) = 13.8 (1.3x)
npcsDf1 <- fx_convListDF(rjson::fromJSON(
  file = './combat/Glasrath_AssautTgt.json'))
pcsDf1 <- fx_convListDF(rjson::fromJSON(
  file = './combat/Glasrath_AssautPCs.json'))

fx_attackRolls(atck = npcsDf1, dfnd = pcsDf1, seed = NULL,
               maxAtckMult = 6, output = 'summary')


# LARGE COMBAT
# load JSON data
# condition: E(d20D) = 7.2 (0.7x); E(d20) = 10.5 (1x); E(d20A) = 13.8 (1.3x)
pcsList <- c(
  # 'wtht',
  # 'ggke',
  'gmnz',
  # 'tfdd',
  'ngn'
  # 'dmth'
)

npcsList <- c(
  # 't',
  'g'
  # 'c'
)

pcsDf2 <- lapply(pcsList, function(x) {
  fx_convListDF(rjson::fromJSON(
    file = paste0('./combat/Glasrath_Alliance_', x, '.json')))
}) %>% bind_rows() %>%
  filter(number > 0)


npcsDf2 <- lapply(npcsList, function(x) {
  fx_convListDF(rjson::fromJSON(
    file = paste0('./combat/Glasrath_Uchbur_', x, '.json')))
}) %>% bind_rows() %>%
  filter(number > 0)

# npcs attacking
npcsRound <- left_join(pcsDf2,
          fx_attackRolls(atck = npcsDf2, dfnd = pcsDf2, seed = NULL,
               maxAtckMult = 3, output = 'warsummary'),
          by = c('name' = 'tgt')) %>%
  mutate(deaths = round(total / hp, digits = 1),
         survivors = number - deaths,
         resistance = round(survivors / number, digits = 2))
npcsRound

# pcs attacking
pcsRound <- left_join(npcsDf2,
          fx_attackRolls(atck = pcsDf2, dfnd = npcsDf2, seed = NULL,
                         maxAtckMult = 3, output = 'warsummary'),
          by = c('name' = 'tgt')) %>%
  mutate(total = ifelse(is.na(total), 0, total),
         deaths = round(total / hp, digits = 1),
         survivors = number - deaths,
         resistance = round(survivors / number, digits = 2))
pcsRound


