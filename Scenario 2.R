# SET APPROPRIATE WORKING DIRECTORY AT FIRST!

covid <- read.csv("covid 3P4.csv")
data <- read.csv("covid 3P4.csv")
library(texreg)

str(covid)
View(covid)

# FORMATTAGE TABLEAU
covid[,1] <- as.numeric(covid[,1])
covid[,3] <- as.numeric(covid[,3])
covid[,5] <- as.numeric(covid[,5])
covid[,6] <- as.numeric(covid[,6])
covid[,8] <- as.numeric(covid[,8])
covid[,9] <- as.numeric(covid[,9])
covid[,10] <- as.numeric(covid[,10])
covid[,11] <- as.numeric(covid[,11])
covid[,12] <- as.numeric(covid[,12])
covid[,13] <- as.numeric(covid[,13])
covid[,14] <- as.numeric(covid[,14])
covid[,15] <- as.numeric(covid[,15])
covid[,16] <- as.numeric(covid[,16])
covid[,17] <- as.numeric(covid[,17])
covid[,18] <- as.numeric(covid[,18])
covid[,19] <- as.numeric(covid[,19])
covid[,20] <- as.numeric(covid[,20])
covid[,20] <- as.numeric(covid[,21])
covid[,22] <- as.numeric(covid[,22])
covid[,23] <- as.numeric(covid[,23])
covid[,24] <- as.numeric(covid[,24])
covid[,25] <- as.numeric(covid[,25])


# VARIABLES
tor_20 <- mean(covid$tor_2020, na.rm=T)
tor_21 <- mean(covid$tor_2021, na.rm=T)
sp_19 <- mean(covid$sp_2019, na.rm=T)
sp_20 <- mean(covid$sp_2020, na.rm=T)
sp_21 <- mean(covid$sp_2021, na.rm=T)
R0_21 <- mean(covid$R0_2021, na.rm=T)
incid_21 <- mean(covid$incid_2021, na.rm=T)
diff_sp_c2 <- sp_21 - sp_19
diff_sp_c1 <- sp_21 - sp_20
diffact_1921 <- mean(covid$diffact_sansap_c2, na.rm=T)
diffact_2021 <- mean(covid$diffact_sansap_c1, na.rm=T)
diffact_ap_1921 <- mean(covid$diffact_ap_c2, na.rm=T)
diffact_ap_2021 <- mean(covid$diffact_ap_c1, na.rm=T)
dist <- mean(covid$distance, na.rm=T)


# MODIFYING THE TABLE TO INCLUDE DIM
diffsp20 <- tapply((covid$sp_2021 - covid$sp_2020), covid$id, mean)
diffpc20 <- tapply((covid$pc_2021 - covid$pc_2020), covid$id, mean)
diffih20 <- tapply((covid$ih_2021 - covid$ih_2020), covid$id, mean)
diffic20 <- tapply((covid$ic_2021 - covid$ic_2020), covid$id, mean)
diffqe20 <- tapply((covid$qe_2021 - covid$qe_2020), covid$id, mean)
diffap20 <- tapply((covid$ap_2021 - covid$ap_2020), covid$id, mean)

diffsp21 <- tapply((covid$sp_2021 - covid$sp_2019), covid$id, mean)
diffpc21 <- tapply((covid$pc_2021 - covid$pc_2019), covid$id, mean)
diffih21 <- tapply((covid$ih_2021 - covid$ih_2019), covid$id, mean)
diffic21 <- tapply((covid$ic_2021 - covid$ic_2019), covid$id, mean)
diffqe21 <- tapply((covid$qe_2021 - covid$qe_2019), covid$id, mean)
diffap21 <- tapply((covid$ap_2021 - covid$ap_2019), covid$id, mean)

covid$diff_sp20 <- diffsp20
covid$diff_ih20 <- diffih20
covid$diff_ic20 <- diffic20
covid$diff_pc20 <- diffpc20
covid$diff_qe20 <- diffqe20
covid$diff_ap20 <- diffap20

covid$diff_sp21 <- diffsp21
covid$diff_ih21 <- diffih21
covid$diff_ic21 <- diffic21
covid$diff_pc21 <- diffpc21
covid$diff_qe21 <- diffqe21
covid$diff_ap21 <- diffap21

diffact_20 <- (covid$diff_ap20+ covid$diff_qe20+ covid$diff_ic20+ covid$diff_pc20+ covid$diff_ih20+ covid$diff_sp20)/6
diffact_21 <- (covid$diff_ap21+ covid$diff_qe21+ covid$diff_ic21+ covid$diff_pc21+ covid$diff_ih21+ covid$diff_sp21)/6

diffact_horsap_20 <-(covid$diff_qe20+ covid$diff_ic20+ covid$diff_pc20+ covid$diff_ih20+ covid$diff_sp20)/5
diffact_horsap_21 <-(covid$diff_qe21+ covid$diff_ic21+ covid$diff_pc21+ covid$diff_ih21+ covid$diff_sp21)/5

covid$diffact20 <- diffact_20
covid$diffact21 <- diffact_21
covid$diffact_horsap_20 <- diffact_horsap_20
covid$diffact_horsap_21 <- diffact_horsap_21

covid <- covid[,-c(8:25)]
covid <- covid[,-c(12:15)]

tapply(covid$diffact21, covid$groupe, mean)
tapply(covid$diffact20, covid$groupe, mean)

tapply(covid$diffact21, covid$genre, mean)
tapply(covid$diffact20, covid$genre, mean)



# Different impacts of Covid on the type of PA
cor(covid$diff_ap21, covid$tor_2021)
cor(covid$diff_ic21, covid$tor_2021)
cor(covid$diff_ih21, covid$tor_2021)
cor(covid$diff_pc21, covid$tor_2021)
cor(covid$diff_qe21, covid$tor_2021)
cor(covid$diff_sp21, covid$tor_2021)
cor(covid$diffact21, covid$tor_2021)

cor(covid$diff_ap21, covid$R0_2021)
cor(covid$diff_ic21, covid$R0_2021)
cor(covid$diff_ih21, covid$R0_2021)
cor(covid$diff_pc21, covid$R0_2021)
cor(covid$diff_qe21, covid$R0_2021)
cor(covid$diff_sp21, covid$R0_2021)
cor(covid$diffact21, covid$R0_2021)

cor(covid$diff_ap21, covid$incid_2021)
cor(covid$diff_ic21, covid$incid_2021)
cor(covid$diff_ih21, covid$incid_2021)
cor(covid$diff_pc21, covid$incid_2021)
cor(covid$diff_qe21, covid$incid_2021)
cor(covid$diff_sp21, covid$incid_2021)
cor(covid$diffact21, covid$incid_2021)


# Regression on Weeks of Presence
screenreg(lm(covid$diff_sp21 ~ covid$tor_2021 + covid$R0_2021 + covid$incid_2021))
screenreg(lm(covid$diff_sp20 ~ covid$tor_2021))

# Controlling for distance
screenreg(lm(covid$diff_sp21 ~ covid$tor_2021 * covid$distance + covid$R0_2021 * covid$distance))


# Gender
tapply(covid$diff_ap21, covid$genre, mean)
tapply(covid$diff_sp21, covid$genre, mean)
tapply(covid$diff_ih21, covid$genre, mean)
tapply(covid$diff_ic21, covid$genre, mean)
tapply(covid$diff_pc21, covid$genre, mean)
tapply(covid$diff_qe21, covid$genre, mean)
tapply(covid$diffact21, covid$genre, mean)


# AMENDMENTS PER OPPOSITION OR MAJORITY GROUPS
test <- covid
test <- covid[,-c(8:27)]
View(test)
test$opposition[test$groupe == "LREM"] <- 0
test$opposition[test$groupe == "UDI"] <- 1
test$opposition[test$groupe == "LFI"] <- 1
test$opposition[test$groupe == "UDI"] <- 1
test$opposition[test$groupe == "LR"] <- 1
test$opposition[test$groupe == "GDR"] <- 1
test$opposition[test$groupe == "AE"] <- 0
test$opposition[test$groupe == "MODEM"] <- 0
test$opposition[test$groupe == "LT"] <- 1
test$opposition[test$groupe == "SOC"] <- 1
test$ap <- data$ap_2021
test <- test[,-9]
test$ap21 <- data$ap_2021
test$ap20 <- data$ap_2020
test$ap19 <- data$ap_2019
test$diffap2120 <- test$ap21
test$diffap2120 <- test$ap21 - test$ap20

opposition_diffap <- lm(test$diffap2120 ~ test$opposition)
opposition_ap <- lm(test$ap21 ~ test$opposition)


# Distance
covid$distance[covid$distance < 101] <- "0-100"

covid$distance[covid$dept > 95] <- "1000+"

covid$distance[covid$dept == 1] <- "301-500"
covid$distance[covid$dept == 7] <- "301-500"
covid$distance[covid$dept == 15] <- "301-500"
covid$distance[covid$dept == 16] <- "301-500"
covid$distance[covid$dept == 17] <- "301-500"
covid$distance[covid$dept == 19] <- "301-500"
covid$distance[covid$dept == 22] <- "301-500"
covid$distance[covid$dept == 23] <- "301-500"
covid$distance[covid$dept == 24] <- "301-500"
covid$distance[covid$dept == 25] <- "301-500"
covid$distance[covid$dept == 26] <- "301-500"
covid$distance[covid$dept == 29] <- "301-500"
covid$distance[covid$dept == 33] <- "301-500"
covid$distance[covid$dept == 35] <- "301-500"
covid$distance[covid$dept == 38] <- "301-500"
covid$distance[covid$dept == 39] <- "301-500"
covid$distance[covid$dept == 42] <- "301-500"
covid$distance[covid$dept == 43] <- "301-500"
covid$distance[covid$dept == 44] <- "301-500"
covid$distance[covid$dept == 46] <- "301-500"
covid$distance[covid$dept == 48] <- "301-500"
covid$distance[covid$dept == 56] <- "301-500"
covid$distance[covid$dept == 63] <- "301-500"
covid$distance[covid$dept == 67] <- "301-500"
covid$distance[covid$dept == 68] <- "301-500"
covid$distance[covid$dept == 69] <- "301-500"
covid$distance[covid$dept == 70] <- "301-500"
covid$distance[covid$dept == 71] <- "301-500"
covid$distance[covid$dept == 73] <- "301-500"
covid$distance[covid$dept == 74] <- "301-500"
covid$distance[covid$dept == 79] <- "301-500"
covid$distance[covid$dept == 85] <- "301-500"
covid$distance[covid$dept == 87] <- "301-500"
covid$distance[covid$dept == 88] <- "301-500"
covid$distance[covid$dept == 90] <- "301-500"

covid$distance[covid$dept == 2] <- "101-300"
covid$distance[covid$dept == 3] <- "101-300"
covid$distance[covid$dept == 8] <- "101-300"
covid$distance[covid$dept == 10] <- "101-300"
covid$distance[covid$dept == 14] <- "101-300"
covid$distance[covid$dept == 18] <- "101-300"
covid$distance[covid$dept == 21] <- "101-300"
covid$distance[covid$dept == 36] <- "101-300"
covid$distance[covid$dept == 37] <- "101-300"
covid$distance[covid$dept == 41] <- "101-300"
covid$distance[covid$dept == 45] <- "101-300"
covid$distance[covid$dept == 49] <- "101-300"
covid$distance[covid$dept == 50] <- "101-300"
covid$distance[covid$dept == 51] <- "101-300"
covid$distance[covid$dept == 52] <- "101-300"
covid$distance[covid$dept == 53] <- "101-300"
covid$distance[covid$dept == 54] <- "101-300"
covid$distance[covid$dept == 55] <- "101-300"
covid$distance[covid$dept == 57] <- "101-300"
covid$distance[covid$dept == 58] <- "101-300"
covid$distance[covid$dept == 59] <- "101-300"
covid$distance[covid$dept == 61] <- "101-300"
covid$distance[covid$dept == 62] <- "101-300"
covid$distance[covid$dept == 72] <- "101-300"
covid$distance[covid$dept == 76] <- "101-300"
covid$distance[covid$dept == 80] <- "101-300"
covid$distance[covid$dept == 86] <- "101-300"
covid$distance[covid$dept == 89] <- "101-300"

covid$distance[covid$dept == 4] <- "501-1000"
covid$distance[covid$dept == 5] <- "501-1000"
covid$distance[covid$dept == 6] <- "501-1000"
covid$distance[covid$dept == 9] <- "501-1000"
covid$distance[covid$dept == 11] <- "501-1000"
covid$distance[covid$dept == 12] <- "501-1000"
covid$distance[covid$dept == 13] <- "501-1000"
covid$distance[covid$dept == 30] <- "501-1000"
covid$distance[covid$dept == 31] <- "501-1000"
covid$distance[covid$dept == 32] <- "501-1000"
covid$distance[covid$dept == 34] <- "501-1000"
covid$distance[covid$dept == 40] <- "501-1000"
covid$distance[covid$dept == 47] <- "501-1000"
covid$distance[covid$dept == 64] <- "501-1000"
covid$distance[covid$dept == 65] <- "501-1000"
covid$distance[covid$dept == 66] <- "501-1000"
covid$distance[covid$dept == 81] <- "501-1000"
covid$distance[covid$dept == 82] <- "501-1000"
covid$distance[covid$dept == 83] <- "501-1000"
covid$distance[covid$dept == 84] <- "501-1000"
covid$distance[covid$dept == "2A"] <- "501-1000"
covid$distance[covid$dept == "2B"] <- "501-1000"

tapply(covid$diff_sp20, covid$distance, mean)
tapply(covid$diff_pc20, covid$distance, mean)
tapply(covid$diff_ih20, covid$distance, mean)
tapply(covid$diff_ic20, covid$distance, mean)
tapply(covid$diff_ap20, covid$distance, mean)
tapply(covid$diff_qe20, covid$distance, mean)
tapply(covid$diffact20, covid$distance, mean)

screenreg(lm(covid$diff_sp20 ~ test$opposition * covid$distance))
screenreg(lm(covid$diffact20 ~ test$opposition * covid$distance))
screenreg(lm(covid$diff_ap20 ~ test$opposition * covid$distance))


# Population
mean(covid$diff_sp[covid$population < 400000])
mean(covid$diff_sp[covid$population > 400000 & covid$population < 690000])
mean(covid$diff_sp[covid$population > 690000 & covid$population < 1100000])
mean(covid$diff_sp[covid$population > 1100000 & covid$population < 1550000])
mean(covid$diff_sp[covid$population > 1550000])

mean(covid$diff_pc[covid$population < 400000])
mean(covid$diff_pc[covid$population > 400000 & covid$population < 690000])
mean(covid$diff_pc[covid$population > 690000 & covid$population < 1100000])
mean(covid$diff_pc[covid$population > 1100000 & covid$population < 1550000])
mean(covid$diff_pc[covid$population > 1550000])

mean(covid$diff_ih[covid$population < 400000])
mean(covid$diff_ih[covid$population > 400000 & covid$population < 690000])
mean(covid$diff_ih[covid$population > 690000 & covid$population < 1100000])
mean(covid$diff_ih[covid$population > 1100000 & covid$population < 1550000])
mean(covid$diff_ih[covid$population > 1550000])

mean(covid$diff_ic[covid$population < 400000])
mean(covid$diff_ic[covid$population > 400000 & covid$population < 690000])
mean(covid$diff_ic[covid$population > 690000 & covid$population < 1100000])
mean(covid$diff_ic[covid$population > 1100000 & covid$population < 1550000])
mean(covid$diff_ic[covid$population > 1550000])

mean(covid$diff_ap[covid$population < 400000])
mean(covid$diff_ap[covid$population > 400000 & covid$population < 690000])
mean(covid$diff_ap[covid$population > 690000 & covid$population < 1100000])
mean(covid$diff_ap[covid$population > 1100000 & covid$population < 1550000])
mean(covid$diff_ap[covid$population > 1550000])

mean(covid$diff_qe[covid$population < 400000])
mean(covid$diff_qe[covid$population > 400000 & covid$population < 690000])
mean(covid$diff_qe[covid$population > 690000 & covid$population < 1100000])
mean(covid$diff_qe[covid$population > 1100000 & covid$population < 1550000])
mean(covid$diff_qe[covid$population > 1550000])

mean(covid$diffact[covid$population < 400000])
mean(covid$diffact[covid$population > 400000 & covid$population < 690000])
mean(covid$diffact[covid$population > 690000 & covid$population < 1100000])
mean(covid$diffact[covid$population > 1100000 & covid$population < 1550000])
mean(covid$diffact[covid$population > 1550000])

length(covid$population[covid$population < 400000])
length(covid$population[covid$population > 400000 & covid$population < 690000])
length(covid$population[covid$population > 690000 & covid$population < 1100000])
length(covid$population[covid$population > 1100000 & covid$population < 1550000])
length(covid$population[covid$population > 1550000])