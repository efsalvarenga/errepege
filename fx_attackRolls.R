library(dplyr)

# defining function
fx_attackRolls <- function(atck, dfnd, seed = NULL,
                           output = c('detail', 'summary', 'war')) {
  
  if (!is.null(seed)) { set.seed(seed) }
  
  # attack roll
  atckRolls <- lapply(names(atck), function (name) {
    # attack rolls and choice of targets (all random)
    roll = sample(1:20, atck[[name]]$number, replace = T)
    criticals <- roll == 20
    roll = roll * atck[[name]]$condition + atck[[name]]$toHit
    tgts = sample(names(dfnd), atck[[name]]$number, replace = T)
    
    # resolving rolls
    tgts_ac <- sapply(tgts, function (tgt) { dfnd[[tgt]]$ac })
    outcome = roll > tgts_ac
    outcome[criticals] <- TRUE
    
    # calculating damage
    dmgs <- outcome * 1
    dmgs[criticals] <- 1.5
    dmgs <- sample(atck[[name]]$dmg, atck[[name]]$number, replace = T) * dmgs
    dmgs <- round(dmgs)
    
    # preparing outcome data
    hit <- ifelse(dmgs == 0, 'miss', 'hit')
    hit[criticals] <- 'crit'
    
    return(data.frame(tgt = names(dmgs),
                      dmg = dmgs,
                      hit = hit))
  })
  # improve naming
  names(atckRolls) <- names(atck)
  
  if (output == 'detail') {
    return (atckRolls)
  } else {
    # summarised results
    atckRollsS <- lapply(names(atck), function (name) {
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
    names(atckRollsS) <- names(atck)
    
    if (output == 'summary') {
      return (atckRollsS)
    } else {
      # war summary
      atckRollsW <- lapply(names(atck), function (name) {
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
      names(atckRollsW) <- names(atck)
      
      return (atckRollsW)
    }
    
  }
  
}



# pcs side set-up
pcs_tgt <- list(
  ggkeGhuse = list(ac = 19),
  ggkeRest = list(ac = 18)
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
  # skullComm = list(ac = 18),
  # lizardUndead = list(ac = 14)
)

# npcs side set-up
# rolls: E(d20D) = 7.2; E(d20) = 10.5; E(d20A) = 13.8
# .: condition: a = 1.3; n = 1; d = 0.7
npcs_atck <- list(
  skelzombie = list(toHit = 3,
                    dmg = c(5:10), # c(3:8) when commander dies
                    number = 550,
                    condition = 1.125), # 1 when commander dies
  skullComm = list(toHit = 7,
                   dmg = c(6:18),
                   number = 2,
                   condition = 1),
  lizardUndead = list(toHit = 7,
                      dmg = c(7:21), # c(7:21) when commander dies
                      number = 2,
                      condition = 1.125) # 1 when commander dies
)

# pcs attack roll
fx_attackRolls(atck = pcs_atck, dfnd = npcs_tgt, seed = NULL, output = 'war')

# npcs attack roll
fx_attackRolls(atck = npcs_atck, dfnd = pcs_tgt, seed = NULL, output = 'war')









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

fx_attackRolls(atck = npcs, dfnd = pcs, seed = NULL, output = 'summary')
