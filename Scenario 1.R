# SET APPROPRIATE WORKING DIRECTORY AT FIRST!

data2p7 <- read.csv("synthese 2P7.csv")
library(texreg)

str(data2p7)
View(data2p7)


# PER GROUP
# 2019
tapply(data2p7$sp_2019, data2p7$groupe, mean, na.rm=T)
tapply(data2p7$ih_2019, data2p7$groupe, mean, na.rm=T)
tapply(data2p7$ic_2019, data2p7$groupe, mean, na.rm=T)
tapply(data2p7$pc_2019, data2p7$groupe, mean, na.rm=T)
tapply(data2p7$qe_2019, data2p7$groupe, mean, na.rm=T)
tapply(data2p7$ap_2019, data2p7$groupe, mean, na.rm=T)


# 2020
tapply(data2p7$sp_2020, data2p7$groupe, mean, na.rm=T)
tapply(data2p7$ih_2020, data2p7$groupe, mean, na.rm=T)
tapply(data2p7$ic_2020, data2p7$groupe, mean, na.rm=T)
tapply(data2p7$pc_2020, data2p7$groupe, mean, na.rm=T)
tapply(data2p7$qe_2020, data2p7$groupe, mean, na.rm=T)
tapply(data2p7$ap_2020, data2p7$groupe, mean, na.rm=T)



# DIFFERENCE IN MEANS (ORDRE IMPORTANT)
test <- data2p7
diffsp <- tapply((test$sp_2020 - test$sp_2019), test$id, mean)
diffpc <- tapply((test$pc_2020 - test$pc_2019), test$id, mean)
diffih <- tapply((test$ih_2020 - test$ih_2019), test$id, mean)
diffic <- tapply((test$ic_2020 - test$ic_2019), test$id, mean)
diffqe <- tapply((test$qe_2020 - test$qe_2019), test$id, mean)
diffap <- tapply((test$ap_2020 - test$ap_2019), test$id, mean)

test$diff_sp <- diffsp
test$diff_ih <- diffih
test$diff_ic <- diffic
test$diff_pc <- diffpc
test$diff_qe <- diffqe
test$diff_ap <- diffap

test <- test[,-c(8:19)]

diffact <- (test$diff_ap+ test$diff_ic+ test$diff_ih+ test$diff_pc+ test$diff_qe+ test$diff_sp)/6
test$diffact <- diffact

View(test)

tapply(test$diffact, test$groupe, mean)
tapply(test$diffact, test$genre, mean)
tapply(test$tor, test$groupe, mean)


# Different impacts of Covid on the type of PA
cor(test$diff_ap, test$tor)
cor(test$diff_ic, test$tor)
cor(test$diff_ih, test$tor)
cor(test$diff_pc, test$tor)
cor(test$diff_qe, test$tor)
cor(test$diff_sp, test$tor)
cor(test$diffact, test$tor)

# The impacts of dependent variables on PA
cor(test$diffact, test$distance)
cor(test$diffact, test$population)
cor(test$diffact, test$naissance)

test$genre[test$genre == "F"] <- 1
test$genre[test$genre == "H"] <- 0
test$genre <- as.numeric(test$genre)
cor(test$diffact, test$genre)

# Gender
tapply(test$diff_ap, test$genre, mean)
tapply(test$diff_sp, test$genre, mean)
tapply(test$diff_ih, test$genre, mean)
tapply(test$diff_ic, test$genre, mean)
tapply(test$diff_pc, test$genre, mean)
tapply(test$diff_qe, test$genre, mean)
tapply(test$diffact, test$genre, mean)

# Distance
test$distance[test$distance < 101] <- "0-100"

test$distance[test$dept > 95] <- "1000+"

test$distance[test$dept == 1] <- "301-500"
test$distance[test$dept == 7] <- "301-500"
test$distance[test$dept == 15] <- "301-500"
test$distance[test$dept == 16] <- "301-500"
test$distance[test$dept == 17] <- "301-500"
test$distance[test$dept == 19] <- "301-500"
test$distance[test$dept == 22] <- "301-500"
test$distance[test$dept == 23] <- "301-500"
test$distance[test$dept == 24] <- "301-500"
test$distance[test$dept == 25] <- "301-500"
test$distance[test$dept == 26] <- "301-500"
test$distance[test$dept == 29] <- "301-500"
test$distance[test$dept == 33] <- "301-500"
test$distance[test$dept == 35] <- "301-500"
test$distance[test$dept == 38] <- "301-500"
test$distance[test$dept == 39] <- "301-500"
test$distance[test$dept == 42] <- "301-500"
test$distance[test$dept == 43] <- "301-500"
test$distance[test$dept == 44] <- "301-500"
test$distance[test$dept == 46] <- "301-500"
test$distance[test$dept == 48] <- "301-500"
test$distance[test$dept == 56] <- "301-500"
test$distance[test$dept == 63] <- "301-500"
test$distance[test$dept == 67] <- "301-500"
test$distance[test$dept == 68] <- "301-500"
test$distance[test$dept == 69] <- "301-500"
test$distance[test$dept == 70] <- "301-500"
test$distance[test$dept == 71] <- "301-500"
test$distance[test$dept == 73] <- "301-500"
test$distance[test$dept == 74] <- "301-500"
test$distance[test$dept == 79] <- "301-500"
test$distance[test$dept == 85] <- "301-500"
test$distance[test$dept == 87] <- "301-500"
test$distance[test$dept == 88] <- "301-500"
test$distance[test$dept == 90] <- "301-500"

test$distance[test$dept == 2] <- "101-300"
test$distance[test$dept == 3] <- "101-300"
test$distance[test$dept == 8] <- "101-300"
test$distance[test$dept == 10] <- "101-300"
test$distance[test$dept == 14] <- "101-300"
test$distance[test$dept == 18] <- "101-300"
test$distance[test$dept == 21] <- "101-300"
test$distance[test$dept == 36] <- "101-300"
test$distance[test$dept == 37] <- "101-300"
test$distance[test$dept == 41] <- "101-300"
test$distance[test$dept == 45] <- "101-300"
test$distance[test$dept == 49] <- "101-300"
test$distance[test$dept == 50] <- "101-300"
test$distance[test$dept == 51] <- "101-300"
test$distance[test$dept == 52] <- "101-300"
test$distance[test$dept == 53] <- "101-300"
test$distance[test$dept == 54] <- "101-300"
test$distance[test$dept == 55] <- "101-300"
test$distance[test$dept == 57] <- "101-300"
test$distance[test$dept == 58] <- "101-300"
test$distance[test$dept == 59] <- "101-300"
test$distance[test$dept == 61] <- "101-300"
test$distance[test$dept == 62] <- "101-300"
test$distance[test$dept == 72] <- "101-300"
test$distance[test$dept == 76] <- "101-300"
test$distance[test$dept == 80] <- "101-300"
test$distance[test$dept == 86] <- "101-300"
test$distance[test$dept == 89] <- "101-300"

test$distance[test$dept == 4] <- "501-1000"
test$distance[test$dept == 5] <- "501-1000"
test$distance[test$dept == 6] <- "501-1000"
test$distance[test$dept == 9] <- "501-1000"
test$distance[test$dept == 11] <- "501-1000"
test$distance[test$dept == 12] <- "501-1000"
test$distance[test$dept == 13] <- "501-1000"
test$distance[test$dept == 30] <- "501-1000"
test$distance[test$dept == 31] <- "501-1000"
test$distance[test$dept == 32] <- "501-1000"
test$distance[test$dept == 34] <- "501-1000"
test$distance[test$dept == 40] <- "501-1000"
test$distance[test$dept == 47] <- "501-1000"
test$distance[test$dept == 64] <- "501-1000"
test$distance[test$dept == 65] <- "501-1000"
test$distance[test$dept == 66] <- "501-1000"
test$distance[test$dept == 81] <- "501-1000"
test$distance[test$dept == 82] <- "501-1000"
test$distance[test$dept == 83] <- "501-1000"
test$distance[test$dept == 84] <- "501-1000"
test$distance[test$dept == "2A"] <- "501-1000"
test$distance[test$dept == "2B"] <- "501-1000"

tapply(test$diff_sp, test$distance, mean)
tapply(test$diff_pc, test$distance, mean)
tapply(test$diff_ih, test$distance, mean)
tapply(test$diff_ic, test$distance, mean)
tapply(test$diff_ap, test$distance, mean)
tapply(test$diff_qe, test$distance, mean)
tapply(test$diffact, test$distance, mean)


# Population
mean(test$diff_sp[test$population < 400000])
mean(test$diff_sp[test$population > 400000 & test$population < 690000])
mean(test$diff_sp[test$population > 690000 & test$population < 1100000])
mean(test$diff_sp[test$population > 1100000 & test$population < 1550000])
mean(test$diff_sp[test$population > 1550000])

mean(test$diff_pc[test$population < 400000])
mean(test$diff_pc[test$population > 400000 & test$population < 690000])
mean(test$diff_pc[test$population > 690000 & test$population < 1100000])
mean(test$diff_pc[test$population > 1100000 & test$population < 1550000])
mean(test$diff_pc[test$population > 1550000])

mean(test$diff_ih[test$population < 400000])
mean(test$diff_ih[test$population > 400000 & test$population < 690000])
mean(test$diff_ih[test$population > 690000 & test$population < 1100000])
mean(test$diff_ih[test$population > 1100000 & test$population < 1550000])
mean(test$diff_ih[test$population > 1550000])

mean(test$diff_ic[test$population < 400000])
mean(test$diff_ic[test$population > 400000 & test$population < 690000])
mean(test$diff_ic[test$population > 690000 & test$population < 1100000])
mean(test$diff_ic[test$population > 1100000 & test$population < 1550000])
mean(test$diff_ic[test$population > 1550000])

mean(test$diff_ap[test$population < 400000])
mean(test$diff_ap[test$population > 400000 & test$population < 690000])
mean(test$diff_ap[test$population > 690000 & test$population < 1100000])
mean(test$diff_ap[test$population > 1100000 & test$population < 1550000])
mean(test$diff_ap[test$population > 1550000])

mean(test$diff_qe[test$population < 400000])
mean(test$diff_qe[test$population > 400000 & test$population < 690000])
mean(test$diff_qe[test$population > 690000 & test$population < 1100000])
mean(test$diff_qe[test$population > 1100000 & test$population < 1550000])
mean(test$diff_qe[test$population > 1550000])

mean(test$diffact[test$population < 400000])
mean(test$diffact[test$population > 400000 & test$population < 690000])
mean(test$diffact[test$population > 690000 & test$population < 1100000])
mean(test$diffact[test$population > 1100000 & test$population < 1550000])
mean(test$diffact[test$population > 1550000])

length(test$population[test$population < 400000])
length(test$population[test$population > 400000 & test$population < 690000])
length(test$population[test$population > 690000 & test$population < 1100000])
length(test$population[test$population > 1100000 & test$population < 1550000])
length(test$population[test$population > 1550000])
