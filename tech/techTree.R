library(rjson)
library(dplyr)
library(stringr)
library(jsonlite)

# import data
techTree <- rjson::fromJSON(file = "tech/civTechTree.json")

# clean data
techTree <- lapply(techTree, function(tech) {
  if (str_detect(tech$tech_name, '\\(')) {
    tech$tech_name <- sub(" \\(Civ5)", "", tech$tech_name)
  }
  return (tech)
})

# create simple table for reference
techTable <- lapply(techTree, function(tech) {
  data.frame(tech_id = tech$tech_id,
             tech_name = str_to_lower(tech$tech_name),
             era = tech$era,
             cost = tech$cost,
             units = toString(tech$units_enabled))
})
techTable <- do.call(rbind, techTable)
names(techTree) <- techTable$tech_name

# summarise cost per era, and calculate yearly costs per era
costPerEra <- techTable %>%
  group_by(era) %>%
  summarise(totCost = sum(cost)) %>%
  arrange(totCost)
costPerEra$yearsInEra <- c(3000, 1500, 850, 375, 165, 55, 50, 25)
costPerEra$progrPerYear <- costPerEra$totCost / costPerEra$yearsInEra

# create list with options for further development
techForward <- lapply(techTree, function(tech) {
  str_to_lower(tech$leads_to)
})
names(techForward) <- techTable$tech_name

# create list with requirements per technology
techBackward <- lapply(techTree, function(tech) {
  str_to_lower(tech$required_techs)
})
names(techBackward) <- techTable$tech_name

# simulation function
createTechDevPath <- function (totResources = 219945, dvpdTechs,
                               currentYear = 3877, currentDevDevRate = 4.5,
                               seed = 1) {
  # start with initial resources
  iniCosts <- sum(techTable[techTable$tech_name %in% dvpdTechs, ]$cost)
  resources <- totResources - iniCosts
  
  # store initial dvpdTechs
  dvpdTechsIni <- dvpdTechs
  
  # cycle until resources are not negative to develop and not all techs are
  # developed - it might be that in the end it will be negative resources
  while(resources > 0 && length(dvpdTechs) < nrow(techTable)) {

    # check what are the options for development, without filter
    altDevs <- techForward[techTable$tech_name %in% dvpdTechs] %>%
      unlist() %>%
      as.character()
    
    # filter options for development that are already developed
    altDevs <- altDevs[!(altDevs %in% dvpdTechs)]
    
    # identify which options for development are blocked due to required tech
    blocking <- lapply(altDevs, function(tech) {
      if (any(!techBackward[[tech]] %in% dvpdTechs)) {
        return ('blocked')
      } else {
        return ('unblocked')
      }
    }) %>% unlist()
    
    # keep only unblocked options
    altDevs <- altDevs[blocking == 'unblocked']
    
    # choose randomly a new tech
    set.seed(seed)
    choiceDev <- sample(altDevs, 1)
    
    # update resorces
    resources <- resources - techTree[[choiceDev]]$cost
    
    # add new technology to developed list
    dvpdTechs <- c(dvpdTechs, choiceDev)
  }
  
  # create results for function
  unitsAvailable <- techTable[techTable$tech_name %in% dvpdTechs, ]$units
  unitsAvailable <- toString(unitsAvailable[unitsAvailable != ''])
  result <- list(dvpdTechs, unitsAvailable, resources,
                 totResources, dvpdTechsIni, currentYear,
                 currentDevDevRate, seed)
  names(result) <- c('dvpdOrder', 'unitsAvailable', 'resourcesLeft',
                     'totResources', 'dvpdTechsIni', 'currentYear',
                     'currentDevDevRate', 'seed')
  
  return (result)
}

showTechTable <- function (civName) {
  techs <- simList[[civName]]$dvpdOrder
  table <- techTable[techTable$tech_name %in% techs, ] %>%
    arrange(cost)
  return (table)
}

relatedDev <- function (relCiv, relRate = 0.7) {
  forkPoint <- floor(length(simList[[relCiv]]$dvpdOrder) * relRate)
  return (simList[[relCiv]]$dvpdOrder[1:forkPoint])
}

# technology development simulation
startYear = 3877
currentYear = 3877.5


# import existing technology development simulation
simList <- rjson::fromJSON(file = "tech/simList.json")

names(simList)

# re-run
simList$kuzar <- createTechDevPath(420,
                                   dvpdTechs = simList$kuzar$dvpdOrder,
                                   currentDevDevRate = 4, seed = 1)
simList$acrisae <- createTechDevPath(13000,
                                     dvpdTechs = 'agriculture',
                                     currentDevDevRate = 40, seed = 9)
simList$nurderad <- createTechDevPath(650,
                                      dvpdTechs = relatedDev('kuzar', 0.7),
                                      currentDevDevRate = 6, seed = 3)
simList$tolfoddund <- createTechDevPath(420,
                                        dvpdTechs = relatedDev('kuzar', 0.5),
                                        currentDevDevRate = 4, seed = 1)
simList$kostoch <- createTechDevPath(470,
                                     dvpdTechs = relatedDev('kuzar', 0.5),
                                     currentDevDevRate = 4.2, seed = 1)


simTable <- lapply(names(simList), function (civ) {
  data.frame(civ = civ,
             showTechTable(civ))
}) %>% bind_rows()


if (FALSE) {
  simListJSON <- jsonlite::toJSON(simList, pretty = TRUE)
  write(simListJSON, file = 'simList.json')
  
  write.csv(simTable, 'simTable.csv')
}
