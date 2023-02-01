
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
                  number = 50,
                  condition = 1),
  zombie = list(toHit = 3,
                dmg = c(2:7),
                number = 0,
                condition = 1)
)

# rolls: E(d20D) = 7.2; E(d20) = 10.5; E(d20A) = 13.8
# condition: a = 1.3; n = 1; d = 0.7

# attack
atckRolls <- lapply(names(npcs), function (name) {
  # attack rolls and choice of targets (all random)
  roll = sample(1:20, npcs[[name]]$number, replace = T)
  # roll[4] = 20
  criticals <- which(roll == 20)
  # browser()
  roll = roll * npcs[[name]]$condition + npcs[[name]]$toHit
  tgts = sample(names(pcs), npcs[[name]]$number, replace = T)
  
  # resolving rolls and damage
  tgts_ac <- sapply(tgts, function (tgt) { pcs[[tgt]]$ac })
  outcome = roll > tgts_ac
  outcome[criticals] <- TRUE
  
  # dmgs <- outcome[outcome]
  dmgs <- outcome * 1
  dmgs[criticals] <- 1.5
  dmgs <- sample(npcs[[name]]$dmg, npcs[[name]]$number, replace = T) * dmgs
  dmgs <- round(dmgs)
  
  
  
  # summarised outcome
  return( list(dmgs, criticals) )
})
names(atckRolls) <- names(npcs)
atckRolls


# To-do
# criticals
# multi attacks