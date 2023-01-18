# Generate city military and number of leaders

# Set up city table
library(EnvStats)
set.seed(1) # CTRL+A run to guarantee seed

# Master data
cityName <- c('Watheton', 'Gorthugke', 'Gamunz', 'Tolfoddund', 'Nargun', 'Delmuth')
cityPop <- c(512, 11682, 10180, 6472, 9000, 5000)

# Configuration
numCities <- length(cityName)
leadPropRate <- rtri(numCities, min = 6, max = 15, mode = 12.5) * 1000
milPropRate <- rtri(numCities, min = 1, max = 3, mode = 2) / 100

# Generate city table
cityTable <- data.frame(cityName = cityName,
                        cityPop = cityPop,
                        cityLeads = 2 + round(cityPop/leadPropRate),
                        cityMil = round(cityPop * milPropRate))
cityTable
sum(cityTable$cityLeads)

# Set up alliances table
set.seed(1) # CTRL+A run to guarantee seed

# Proportion of possible alliances
propPossAli <- 0.03

# Generate alliances
possAliances <- expand.grid(c(LETTERS[1:cityTable$cityLeads[1]]), c(LETTERS[1:cityTable$cityLeads[2]]), c(LETTERS[1:cityTable$cityLeads[3]]),
                            c(LETTERS[1:cityTable$cityLeads[4]]), c(LETTERS[1:cityTable$cityLeads[5]]), c(LETTERS[1:cityTable$cityLeads[6]]))
names(possAliances) <- cityTable$cityName
possAliances$alianceCode <- sample(nrow(possAliances), nrow(possAliances), replace = FALSE)
possAliances$alianceReached <- ifelse(possAliances$alianceCode/nrow(possAliances) < propPossAli, TRUE, FALSE)

# Summarise alliances that can be reached
alianceReached <- possAliances[possAliances$alianceReached, 1:6]
summary(alianceReached)

# Create look up for DM
alianceReachedPerFact <- lapply(1:length(cityTable$cityName), function(i) {
  collect <- lapply(unique(alianceReached[, i]), function (x) {
    summary(alianceReached[alianceReached[, i] == x, -i])
  })
  names(collect) <- paste(cityTable$cityName[i], unique(alianceReached[, i]), sep = ' ')
  collect
})
names(alianceReachedPerFact) <- cityTable$cityName
alianceReachedPerFact
