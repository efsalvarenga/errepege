# Generate seasons

library(EnvStats)
# library(dplyr)

set.seed(259)

# Setup
noOfCycles <- 200
cycleLength <- round(rtri(noOfCycles, min = 15, max = 25, mode = 20))
winterLength <- round(rtri(noOfCycles, min = 3, max = 10, mode = 4))
summerLength <- round(rtri(noOfCycles, min = 0, max = 5, mode = 3))
shoulderLength <- (cycleLength - winterLength - summerLength) / 2


yearStart <- 1
cyclesEnd <- yearStart + cumsum(cycleLength)
cyclesStart <- c(yearStart, (lag(cyclesEnd)[-1] + 1))

primaveras <- cyclesStart
veroes <- primaveras + shoulderLength
outonos <- veroes + summerLength
invernos <- outonos + shoulderLength

iniSeasons <- data.frame(primaveras = primaveras,
                         veroes = veroes,
                         outonos = outonos,
                         invernos = invernos)
iniSeasons[191:195,]