# =========================================================================== #
#                            LIBRARIES AND SET UP                             #
# =========================================================================== #
library(dplyr)
options(dplyr.summarise.inform = FALSE)


# =========================================================================== #
#                            FUNCTIONS BLOCK                                  #
# =========================================================================== #

# function to convert a character set (in a list/JSON list) to a data frame
fx_convListDF <- function (list) {
  lapply(names(list), function (name) {
    bind_cols(name = name,
              as.data.frame(list[[name]]))
  }) %>% bind_rows
}
# ----------------------------------------------------------------------------#


# main attack roll function
fx_attackRolls <- function(atck, dfnd, seed = NULL,
                            maxAtckMult = 6,
                            output = c('detail', 'summary',
                                       'wardetail', 'warsummary')) {
  
  # set seed if requested by argument
  if (!is.null(seed)) { set.seed(seed) }
  
  # attack roll
  atckRolls <- lapply(atck$name, function (name) {
    
    # filtering to appropriate line on DF
    atck1 <- atck[atck$name == name, ]
    
    # determining number of attacks to be made
    numberAttacks <- atck1$number * atck1$atkn
    
    # attack rolls (all random)
    roll = sample(1:20, numberAttacks, replace = T)
    criticals <- roll == 20
    roll = roll * atck1$condition + atck1$toHit
    
    # target probability is defined based on highest damage of creature and
    # number of creatures (root of multiplication of variables)
    tgtProb <- sapply(dfnd$name, function (x) {
      dfnd1 <- dfnd[dfnd$name == x, ]
      sqrt(dfnd1$number * dfnd1$maxDmg) # focus attacks on strong enemies
      #dfnd1$number
    })
    tgts = sample(dfnd$name, numberAttacks, replace = T, prob = tgtProb)
    
    # resolving rolls based on targets
    tgts_ac <- sapply(tgts, function (tgt) { dfnd[dfnd$name == tgt, 'ac'] })
    outcome = roll > tgts_ac
    outcome[criticals] <- TRUE
    
    # calculating damages
    dmgs <- outcome * 1
    dmgs[criticals] <- 1.5
    dmgs <- dmgs * sample(atck1$minDmg:atck1$maxDmg,
                          numberAttacks, replace = T)
    dmgs <- round(dmgs)
    
    # preparing outcome data
    hit <- ifelse(dmgs == 0, 'miss', 'hit')
    hit[criticals] <- 'crit'
    
    return(data.frame(tgt = names(dmgs),
                      dmg = dmgs,
                      hit = hit,
                      row.names = NULL))
  })
  # improve naming
  names(atckRolls) <- atck$name
  
  # applying confusion when multiple attacks with same targets
  atckRolls <- fxH_aR_confusion(atck, dfnd, atckRolls, maxAtckMult)
  
  # output depends on requested detail level
  # 'detail', 'summary', 'wardetail', 'warsummary'
  if (output == 'detail') {
    return (atckRolls)
  } else {
    atckRollsS <- fxH_aR_summary(atck, atckRolls)
    
    if (output == 'summary') {
      return (atckRollsS)
    } else {
      atckRollsW <- fxH_aR_war(atck, atckRollsS)
      
      if (output == 'war') {
        return (atckRollsW)  
      } else {
        atckRollsWS <- bind_rows(atckRollsW)
        
        # need to summarise only if 1 or more rows exists
        if (nrow(atckRollsWS) > 0) {
          atckRollsWS <- atckRollsWS %>%
            group_by(tgt) %>%
            summarise(count = sum(count),
                      total = sum(total))
        }
        
        # output == 'warsummary'
        return (atckRollsWS)
      }
    }
  }
}
# ----------------------------------------------------------------------------#


# helper function to apply confusion when many attackers aim at same target
fxH_aR_confusion <- function (atck, dfnd, atckRolls, maxAtckMult) {
  
  # single df with atckRolls
  atckRollsDf <- lapply(names(atckRolls), function (y) {
    bind_cols(atckr = y, atckRolls[[y]])
  }) %>% bind_rows()
  
  # define max attacks each target group can have based on maxAtckMult
  maxAtcks <- sapply(dfnd$name, function (x) {
    c(tgt = x, maxAtcks = maxAtckMult * dfnd[dfnd$name == x, 'number'])
  }) %>% t() %>% as.data.frame()
  
  # generate summary confusion table with actual attacks
  confunsion <- atckRollsDf %>%
    group_by(tgt) %>%
    summarise(count = n()) %>%
    left_join(maxAtcks, by = 'tgt') %>%
    mutate(maxAtcks = as.numeric(maxAtcks),
           effect = pmin(0, maxAtcks - count),
           actualAtcks = pmin(count, maxAtcks))
  
  # resolving confusion and sampling attacks based on max per target group
  atckRollsDf_resolved <- lapply(unique(atckRollsDf$tgt), function (z) {
    df <- atckRollsDf[atckRollsDf$tgt == z, ]
    subsetRows <- sample(nrow(atckRollsDf[atckRollsDf$tgt == z, ]),
                         size = confunsion$actualAtcks[confunsion$tgt == z])
    
    return (df[subsetRows, ])
  }) %>% bind_rows()
  
  # re-listing for output standard
  atckRollsDf_resolvedList <- lapply(atck$name, function (x) {
    atckRollsDf_resolved %>%
      filter(atckr == x) %>%
      select(-atckr)
  })
  names(atckRollsDf_resolvedList) <- names(atckRolls)
  
  return (atckRollsDf_resolvedList)
}
# ----------------------------------------------------------------------------#


# helper function to summarise results to verbosity 3
fxH_aR_summary <- function (atck, atckRolls) {
  
  atckRollsS <- lapply(atck$name, function (name) {
    
    # skiping empty tables
    if (is.null(atckRolls[[name]])) { return (NULL) }
    if (nrow(atckRolls[[name]]) == 0) { return (NULL) }
    
    # summarising
    atckSumm <- atckRolls[[name]] %>%
      group_by(tgt, hit) %>%
      summarise(count = n(),
                total = sum(dmg))
    return (atckSumm)
  })
  
  # improve naming
  names(atckRollsS) <- atck$name
  
  return (atckRollsS)
}
# ----------------------------------------------------------------------------#


# helper function to summarise results to verbosity 2
fxH_aR_war <- function (atck, atckRollsS) {
  
  atckRollsW <- lapply(atck$name, function (name) {
    
    # skiping empty tables
    if (is.null(atckRollsS[[name]])) { return (NULL) }
    
    # summarising
    atckSumm <- atckRollsS[[name]] %>%
      filter(hit != 'miss') %>%
      group_by(tgt) %>%
      summarise(count = sum(count),
                total = sum(total))
    
    # making NULL list items without any rows
    if(nrow(atckSumm) == 0) { return (NULL) }
    
    return (atckSumm)
  })
  
  # improve naming
  names(atckRollsW) <- atck$name
  
  return (atckRollsW)
}
# ----------------------------------------------------------------------------#

# wrapper function for making expectation runs over multiple single rounds
fxW_aR_expectationRuns <- function(atck, dfnd, maxAtckMult = 6,
                                   rounds = 100) {
  
  # run multiple single round
  atcks <- lapply(1:rounds, function(x) {
    fx_attackRolls(atck = atck, dfnd = dfnd, maxAtckMult = maxAtckMult,
                    seed = NULL, output = 'warsummary')
  }) %>% bind_rows()
  
  # consolidate multiple runs in a singular average round
  if (nrow(atcks) > 0) {
    atcks <- atcks %>%
      group_by(tgt) %>%
      summarise(count = round(sum(count) / rounds),
                total = round(sum(total) / rounds))
  } 
  
  return (atcks)
}
