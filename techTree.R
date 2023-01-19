library(rjson)
library(dplyr)
library(stringr)

techTree <- fromJSON(file = "civTechTree.json")

# data cleaning
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

# give the tree nice names
names(techTree) <- techTable$tech_name

# create list with options for further development
techForward <- lapply(techTree, function(tech) {
  str_to_lower(tech$leads_to)
  # df <- data.frame(tech_name = str_to_lower(tech$leads_to))
  # tech_id <- left_join(df, techTable, by = 'tech_name')[['tech_id']]
  # return (tech_id)
})
names(techForward) <- techTable$tech_name

# simulation starting settings

totResources <- 10000    # 219945 enables total development
resources <- totResources
depdTechs <- 'agriculture'

# simulation run
while(resources > 0 && length(depdTechs) < nrow(techTable)) {
  altDevs <- techForward[techTable$tech_name %in% depdTechs] %>%
    unlist() %>%
    as.character()
  altDevs <- altDevs[!(altDevs %in% depdTechs)]
  choiceDev <- sample(altDevs, 1)
  resources <- resources - techTree[[choiceDev]]$cost
  depdTechs <- c(depdTechs, choiceDev)
}

dvpdTable <- techTable[techTable$tech_name %in% depdTechs, ]

