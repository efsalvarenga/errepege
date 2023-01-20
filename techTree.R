library(rjson)
library(dplyr)
library(stringr)

# import data
techTree <- fromJSON(file = "civTechTree.json")

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
createTechDevPath <- function (totResources = 219945, dvpdTechs) {
  # start with initial resources
  resources <- totResources
  
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
    choiceDev <- sample(altDevs, 1)
    
    # update resorces
    resources <- resources - techTree[[choiceDev]]$cost
    
    # add new technology to developed list
    dvpdTechs <- c(dvpdTechs, choiceDev)
  }
  
  # create results for function
  dvpdTable <- techTable[techTable$tech_name %in% dvpdTechs, ]
  result <- list(dvpdTechs, resources, dvpdTable)
  names(result) <- c('dvpdTechs', 'resourcesLeft', 'dvpdTable')
  
  return (result)
}


set.seed(1)
createTechDevPath(400, 'agriculture') # compass should not be created
