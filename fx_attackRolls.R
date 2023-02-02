library(dplyr)

# defining function
fx_attackRolls <- function(pcs, npcs, seed = NULL,
                           output = c('detail', 'summary', 'war')) {
  
  if (!is.null(seed)) { set.seed(seed) }
  
  # attack roll
  atckRolls <- lapply(names(npcs), function (name) {
    # attack rolls and choice of targets (all random)
    roll = sample(1:20, npcs[[name]]$number, replace = T)
    criticals <- roll == 20
    roll = roll * npcs[[name]]$condition + npcs[[name]]$toHit
    tgts = sample(names(pcs), npcs[[name]]$number, replace = T)
    
    # resolving rolls
    tgts_ac <- sapply(tgts, function (tgt) { pcs[[tgt]]$ac })
    outcome = roll > tgts_ac
    outcome[criticals] <- TRUE
    
    # calculating damage
    dmgs <- outcome * 1
    dmgs[criticals] <- 1.5
    dmgs <- sample(npcs[[name]]$dmg, npcs[[name]]$number, replace = T) * dmgs
    dmgs <- round(dmgs)
    
    # preparing outcome data
    hit <- ifelse(dmgs == 0, 'miss', 'hit')
    hit[criticals] <- 'crit'
    
    return(data.frame(tgt = names(dmgs),
                      dmg = dmgs,
                      hit = hit))
  })
  # improve naming
  names(atckRolls) <- names(npcs)
  
  if (output == 'detail') {
    return (atckRolls)
  } else {
    # summarised results
    atckRollsS <- lapply(names(npcs), function (name) {
      # skiping empty tables
      if (nrow(atckRolls[[name]]) == 0) { return (NA) }
      
      # summarising
      atckSumm <- atckRolls[[name]] %>%
        group_by(tgt, hit) %>%
        summarise(count = n(),
                  total = sum(dmg))
      
      return (atckSumm)
    })
    # improve naming
    names(atckRollsS) <- names(npcs)
    
    if (output == 'summary') {
      return (atckRollsS)
    } else {
      # war summary
      atckRollsW <- lapply(names(npcs), function (name) {
        # skiping empty tables
        if (nrow(atckRollsS[[name]]) == 0) {
          return (NA)
        }
        
        # summarising
        atckSumm <- atckRollsS[[name]] %>%
          filter(hit != 'miss') %>%
          group_by(tgt) %>%
          summarise(count = sum(count),
                    total = sum(total))
        
        return (atckSumm)
      })
      # improve naming
      names(atckRollsW) <- names(npcs)
      
      return (atckRollsW)
    }
    
  }
  
}



# pcs side set-up
pcs_tgt <- list(
  ggkeGhuse = list(ac = 19),
  ggkeRest = list(ac = 18)
  # c = list(ac = 16),
  # g = list(ac = 14),
  # b = list(ac = 10),
  # t = list(ac = 13),
  # m = list(ac = 12),
  # d1 = list(ac = 14),
  # d2 = list(ac = 15)
)

pcs_atck <- list(
  ggkeGhuse = list(toHit = 5,
               dmg = c(4:13),
               number = 100,
               condition = 1),
  ggkeRest = list(toHit = 5,
                dmg = c(4:11),
                number = 152,
                condition = 1)
)

npcs_tgt <- list(
  skelzombie = list(ac = 11)
)

# npcs side set-up
# rolls: E(d20D) = 7.2; E(d20) = 10.5; E(d20A) = 13.8
# .: condition: a = 1.3; n = 1; d = 0.7
npcs_atck <- list(
  skelzombie = list(toHit = 3,
                    dmg = c(3:8),
                    number = 480,
                    condition = 1)
  # skeleton = list(toHit = 4,
  #                 dmg = c(3:8),
  #                 number = 0,
  #                 condition = 1),
  # zombie = list(toHit = 3,
  #               dmg = c(2:7),
  #               number = 0,
  #               condition = 1)
)

# pcs attack roll
fx_attackRolls(pcs = npcs_tgt, npcs = pcs_atck, seed = NULL, output = 'war')

# npcs attack roll
fx_attackRolls(pcs = pcs_tgt, npcs = npcs_atck, seed = 1, output = 'war')









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

npcs <- list(
  skeleton = list(toHit = 4,
                  dmg = c(3:8),
                  number = 8,
                  condition = 1),
  zombie = list(toHit = 3,
                dmg = c(2:7),
                number = 1,
                condition = 1)
)

fx_attackRolls(pcs = pcs, npcs = npcs, seed = NULL, output = 'summary')
