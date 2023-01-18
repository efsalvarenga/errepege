# Generate city military and number of leaders
library(EnvStats)
set.seed(1)
cityName <- c('Watheton', 'Gorthugke', 'Gamunz', 'Tolfoddund', 'Dwarfs', 'Drows')
cityPop <- c(512, 11682, 10180, 6472, 9000, 5000)

numCities <- length(cityName)
leadPropRate <- rtri(numCities, min = 2, max = 7, mode = 5.5) * 1000
milPropRate <- rtri(numCities, min = 1, max = 3, mode = 2) / 100

cityTable <- data.frame(cityName = cityName,
                        cityPop = cityPop,
                        cityLeads = 2 + round(cityPop/leadPropRate),
                        cityMil = round(cityPop * milPropRate))
cityTable

#######
propPossAli <- 0.006
set.seed(1)

possAliances <- expand.grid(c(LETTERS[1:cityTable$cityLeads[1]]), c(LETTERS[1:cityTable$cityLeads[2]]), c(LETTERS[1:cityTable$cityLeads[3]]),
                            c(LETTERS[1:cityTable$cityLeads[4]]), c(LETTERS[1:cityTable$cityLeads[5]]), c(LETTERS[1:cityTable$cityLeads[6]]))
names(possAliances) <- cityTable$cityName
possAliances$alianceCode <- sample(nrow(possAliances), nrow(possAliances), replace = FALSE)
possAliances$alianceReached <- ifelse(possAliances$alianceCode/nrow(possAliances) < propPossAli, TRUE, FALSE)

# how can we make cases where it is yes, if....

# sum(possAliances$alianceReached)

alianceReached <- possAliances[possAliances$alianceReached, 1:6]

summary(alianceReached)

alianceReachedPerFact <- lapply(1:length(cityTable$cityName), function(i) {
  collect <- lapply(unique(alianceReached[, i]), function (x) {
    summary(alianceReached[alianceReached[, i] == x, -i])
  })
  names(collect) <- paste(cityTable$cityName[i], unique(alianceReached[, i]), sep = ' ')
  collect
})
names(alianceReachedPerFact) <- cityTable$cityName


# c('religious', 'political', 'military', 'arcane', 'racial')


# another alliance style (more principled)




leadLetter <- lapply(1:nrow(cityTable), function(i) {
  LETTERS[1:cityTable$cityLeads[i]]
})
names(leadLetter) <- cityTable$cityName


lapply(leadLetter[[i]], function(j) {
  
})


j=1
leadLetter[j]


alianceReached



