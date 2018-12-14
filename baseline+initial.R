library(readxl)
library(dplyr)
library(tidyr)
library(reshape)
library(readxl)
library(writexl)

setwd("/Users/anupambanerjee/Documents/Fall-2018-School/DSA6000/term-project")
main_df <- read_excel("Data+campus_challenge_FINAL.xlsx")
footprint_df <- read_excel("carbon-footprint.xlsx")

View(main_df)
qol <- as.data.frame(aggregate(Quality_of_Life_Importance__1_10~Activity, main_df, mean, na.rm=TRUE))
cr <- data.frame("use of cooking range", 1)
names(cr) <- c("Activity","Quality_of_Life_Importance__1_10")
sk <- data.frame("Small kitchen appliance in the home", 1)
names(sk) <- c("Activity","Quality_of_Life_Importance__1_10")
he <- data.frame("hazardous or electric items disposed", 1)
names(he) <- c("Activity","Quality_of_Life_Importance__1_10")
ct <- data.frame("car trips - 2+ people with multiple end points", 1)
names(ct) <- c("Activity","Quality_of_Life_Importance__1_10")
qol <- rbind(qol, cr, sk, he, ct)
main_df$Quality_of_Life_Importance__1_10 <- as.integer(main_df$Quality_of_Life_Importance__1_10)
qol$Quality_of_Life_Importance__1_10 <- as.integer(qol$Quality_of_Life_Importance__1_10)
View(qol)
main_df$avgqol <- qol$Quality_of_Life_Importance__1_10[match(main_df$Activity, qol$Activity)]
main_df$Quality_of_Life_Importance__1_10[is.na(main_df$Quality_of_Life_Importance__1_10)] <- main_df$avgqol[is.na(main_df$Quality_of_Life_Importance__1_10)] 
main_df[is.na(main_df)] <- 0
main_df$avgqol <- NULL
View(main_df)

utilityData <- dplyr::select(main_df, -Group, -Activity, -Units, -Consumption, -Quality_of_Life_Importance__1_10)
utilityData[is.na(utilityData)] <- 0
utility <- aggregate(.~Indnum, utilityData, max) %>% rowwise() %>% mutate(waste = 1) %>% 
  mutate(jetfuel=replace(jetfuel,jetfuel==0,1)) %>%
  mutate(electric___peak_hours=replace(electric___peak_hours,electric___peak_hours==0,1)) %>%
  mutate(electric___off_peak_hours=replace(electric___off_peak_hours,electric___off_peak_hours==0,1))
View(utility)

View(footprint_df)
footprint_df$X__2 <- NULL
footprint_df$Notes <- NULL
footprint_df[is.na(footprint_df)] <- 0
footprint_df$Activity[15] = "Small kitchen appliance in the home" #correct spelling error in carbon footprint table
footprint_df$Unit[17:18] = "mile" #change per description from 100 miles to mile
footprint_df$`Jet Fuel`[17] = 0.0179/100 #normalize carbon footprint for air travel
footprint_df$`Jet Fuel`[18] = 0.0408/100 #normalize carbon footprint for air travel
footprint_df$hybrid[19] = 0.000757
footprint_df$hybrid[20] = 0.000781
footprint_df$hybrid[21] = 0.00021
footprint_df$`electric - peak hours`[19:21] = 0
footprint_df$`electric - off peak hours`[19:21] = 0
footprint_df2 <- footprint_df[,c("X__1", "Activity", "Unit", "solar powered  water heater", "gas water heater", "electric water heater - peak hours", "electric water heater - off peak hours",	"gas",	"natural gas",	"hybrid", "electric - peak hours",	"electric - off peak hours", "Jet Fuel",	"waste management")]
footprint_df2$Activity[15]
View(footprint_df2)

main_df2 <- merge(main_df[c(1,2,3,4,5,6)], utility, by="Indnum", all.x = T)
str(main_df2[c(1:5)])

View(main_df)
View(main_df2)

trivial_soln <- main_df2

trivial_soln$Consumption <- trivial_soln$Consumption * (trivial_soln$Quality_of_Life_Importance__1_10/100)
View(trivial_soln)

baseline <- cbind(main_df2[c(1:6)], main_df2$Consumption * main_df2[-c(1:6)] * footprint_df2[match(main_df2$Activity, footprint_df$Activity), -c(1:3)])
baseline$tcf <- rowSums(baseline[,7:17])
avgbaseline <- baseline[,7:17]
avgbaseline[avgbaseline == 0] <- NA
View(avgbaseline)
baseline$acf <- rowMeans(avgbaseline, na.rm = TRUE)
baseline[is.na(baseline)] <- 0
View(baseline)

reduced.cfp <- cbind(trivial_soln[c(1:6)], trivial_soln$Consumption * trivial_soln[-c(1:6)] * footprint_df2[match(trivial_soln$Activity, footprint_df$Activity), -c(1:3)])
View(reduced.cfp)
reduced.cfp$tcf <- rowSums(reduced.cfp[,7:17])
avgbaseline <- reduced.cfp[,7:17]
avgbaseline[avgbaseline == 0] <- NA
reduced.cfp$acf <- rowMeans(avgbaseline, na.rm = TRUE)
reduced.cfp[is.na(reduced.cfp)] <- 0

pivoted_table <- aggregate(acf ~Indnum,baseline, sum)
reduced_table <- aggregate(acf ~Indnum,reduced.cfp, sum)

base_cf <- aggregate(acf~Indnum, baseline, sum)

View(reduced_table)



write_xlsx(footprint_df2,'footprint_df2.xlsx')
write_xlsx(baseline,"baseline.xlsx")
write_xlsx(pivoted_table,"current_acf.xlsx")
write_xlsx(reduced_table,"reduced_acf.xlsx")
