library(dplyr)

# pcs side set-up
pcs <- list(
  c = list(ac = 16),
  g = list(ac = 14),
  b = list(ac = 10),
  t = list(ac = 13),
  m = list(ac = 12),
  d1 = list(ac = 14),
  d2 = list(ac = 15)
)


# npcs side set-up
npcs <- list(
  skeleton = list(toHit = 4,
                  dmg = c(3:8),
                  number = 5,
                  condition = 'n'),
  zombie = list(toHit = 3,
                dmg = c(2:7),
                number = 0,
                condition = 'n')
)

# attack
atckRolls <- lapply(names(npcs), function (name) {
  # attack rolls and choice of targets (all random)
  browser()
  case_when()
  
  roll = sample(1:20, npcs[[name]]$number, replace = T) + npcs[[name]]$toHit
  tgts = sample(names(pcs), npcs[[name]]$number, replace = T)
  
  # resolving rolls and damage
  tgts_ac <- sapply(tgts, function (tgt) { pcs[[tgt]]$ac })
  outcome = roll > tgts_ac
  dmgs <- outcome[outcome]
  dmgs[dmgs] <- sample(npcs[[name]]$dmg, sum(outcome), replace = T)
  
  # summarised outcome
  return( dmgs )
})
names(atckRolls) <- names(npcs)
atckRolls



tgtDC <- 15
nRoll <- 500
rollB <- mean(c(3, 4))
unitD <- sample(1:6, nRoll, replace = T) + 2

sccs <- round((20 - (tgtDC - rollB)) / 20 * nRoll, digits = 0)
totD <- sum(sample(unitD, sccs, replace = F))