a1 <- sample(1:6, 10000, replace = T)
a2 <- sample(1:6, 10000, replace = T)
b <- sample(1:12, 10000, replace = T)

rerolla1 <- a1 <= 2
rerolla2 <- a2 <= 2
rerollb <- b <= 2

mean(a1 + a2)
mean(b)

a1[rerolla1] <- sample(1:6, sum(rerolla1), replace = T)
a2[rerolla2] <- sample(1:6, sum(rerolla2), replace = T)
b[rerollb] <- sample(1:12, sum(rerollb), replace = T)

mean(a1 + a2)
mean(b)

mean(a1 + a2)/mean(b)