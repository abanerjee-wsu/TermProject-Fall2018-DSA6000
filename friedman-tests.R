setwd("/Users/anupambanerjee/Documents/Fall-2018-School/DSA6000/term-project")
library("readxl")
baseline <- as.data.frame(read_excel("baseline.xlsx"))
View(baseline)

data <- baseline[c("Indnum","Activity", "Consumption", "Quality_of_Life_Importance__1_10")]

data$Weighted_Consumption <- with(data,(Consumption*Quality_of_Life_Importance__1_10)/100)

View(baseline)
View(data)

library(tidyr)
library(dplyr)

data<-data %>% mutate(Activity_Name = 
                                  case_when(
                                    Activity == 'Household heating => 70F' ~ 'Heat_70_More'
                                    , Activity == 'Household heating < 70F' ~ 'Heat_70_Less'
                                    , Activity == 'Use of heat pump' ~ 'Heat_Pump'
                                    , Activity == 'Use of air conditioner' ~ 'Air_Condition'
                                    , Activity == 'shower - short' ~ 'Shower_Short'
                                    , Activity == 'shower - long (> 3 min)' ~ 'Shower_Long'
                                    , Activity == 'bath' ~ 'Bath'
                                    , Activity == 'wash-up' ~ 'Wash_Up'
                                    , Activity == 'use of dishwasher' ~ 'Dishwasher'
                                    , Activity == 'use of clothes washer' ~ 'Clothes_Washer'
                                    , Activity == 'use of clothes dryer' ~ 'Clothes_Dryer'
                                    , Activity == 'use of cooking range' ~ 'Cooking_Range'
                                    , Activity == 'use of self-clean feature of electric oven' ~ 'Self_Clean'
                                    , grepl('oven',Activity) ~ 'Oven' #use of oven
                                    , Activity == 'Small kitchen appliance in the home' ~'Small_Kitchen'
                                    , Activity == 'TV/computer use' ~ 'Electronics'
                                    , Activity == 'air travel - large plane' ~ 'Air_Travel_Large'
                                    , grepl('50',Activity) ~ 'Air_Travel_Small' #air travel - small plane (<50 seats) 
                                    , Activity == 'car trips- self only' ~ 'Car_Self_Only'
                                    , Activity == 'car trips - driver and self' ~ 'Car_Driver_Plus_Self'
                                    , Activity == 'car trips - 2+ people with multiple end points' ~ 'Car_Multiple'
                                    , Activity == 'trips using public ground transportation' ~ 'Public_Transport'
                                    , Activity == 'bags of garbage disposed' ~ 'Garbage'
                                    , Activity == 'bags of recycling deposited (negative CF)' ~ 'Recycle_Disposed'
                                    , Activity == 'bags of compost deposited (negative CF)' ~ 'Compost_Disposed'
                                    , Activity == 'hazardous or electric items disposed' ~ 'Hazardous_Disposed'
                                    , Activity == 'large items disposed' ~ 'Large_Disposed'
                                    , TRUE ~ 'Other' 
                                  ))
View(data)

#Shape data based on QOfLI
data.filter <- data[c("Indnum","Activity_Name", "Quality_of_Life_Importance__1_10")]
View(data.filter)
data_wide <- spread(data.filter, Activity_Name, Quality_of_Life_Importance__1_10)
View(data_wide)

#Shape data based on Weighted_Consumption
data.weight.filter <- data[c("Indnum","Activity_Name", "Weighted_Consumption")]
View(data.weight.filter)
data_weight.wide <- spread(data.weight.filter, Activity_Name, Weighted_Consumption)
View(data_weight.wide)

#Water Importance Analysis
boxplot(data_wide$Shower_Short,data_wide$Shower_Long,data_wide$Bath,data_wide$Wash_Up)
shower <- data_wide[c("Shower_Short", "Shower_Long","Bath","Wash_Up")]
sample_shower <- sample_n(shower,100)
sample_shower <- as.matrix(sample_shower)
friedman.test(sample_shower)

#Water Weighted-Importance Analysis
boxplot(data_weight.wide$Shower_Short,data_weight.wide$Shower_Long,data_weight.wide$Bath,data_weight.wide$Wash_Up)
shower <- data_weight.wide[c("Shower_Short", "Shower_Long","Bath","Wash_Up")]
sample_shower <- sample_n(shower,100)
sample_shower <- as.matrix(sample_shower)
friedman.test(sample_shower)


#Heat Importance Analysis
boxplot(data_wide$Heat_70_Less,data_wide$Heat_70_More,data_wide$Heat_Pump,data_wide$Air_Condition,names = c("Heat_70_Less","Heat_70_More","Heat_Pump","Air_Condition"))
heat <- data_wide[c("Heat_70_Less", "Heat_70_More","Heat_Pump","Air_Condition")]
sample_heat <- sample_n(heat,100)
sample_heat <- as.matrix(sample_heat)
friedman.test(sample_heat)

#Heat Weighted-Importance Analysis
boxplot(data_weight.wide$Heat_70_Less,data_weight.wide$Heat_70_More,data_weight.wide$Heat_Pump,data_weight.wide$Air_Condition,names = c("Heat_70_Less","Heat_70_More","Heat_Pump","Air_Condition"))
heat <- data_weight.wide[c("Heat_70_Less", "Heat_70_More","Heat_Pump","Air_Condition")]
sample_heat <- sample_n(heat,100)
sample_heat <- as.matrix(sample_heat)
friedman.test(sample_heat)


#Kitchen Importance Analysis
boxplot(data_wide$Dishwasher,data_wide$Clothes_Washer,data_wide$Clothes_Dryer,data_wide$Cooking_Range,data_wide$Self_Clean,data_wide$Small_Kitchen,data_wide$Oven)
kitchen <- data_wide[c("Dishwasher", "Clothes_Washer","Clothes_Dryer","Cooking_Range","Self_Clean","Small_Kitchen","Oven")]
sample_kitchen <- sample_n(kitchen,100)
sample_kitchen <- as.matrix(sample_kitchen)
friedman.test(sample_kitchen)

#Heat Weighted-Kitchen Importance Analysis
boxplot(data_weight.wide$Dishwasher,data_weight.wide$Clothes_Washer,data_weight.wide$Clothes_Dryer,data_weight.wide$Cooking_Range,data_weight.wide$Self_Clean,data_weight.wide$Small_Kitchen,data_weight.wide$Oven)
kitchen <- data_weight.wide[c("Dishwasher", "Clothes_Washer","Clothes_Dryer","Cooking_Range","Self_Clean","Small_Kitchen","Oven")]
sample_kitchen <- sample_n(kitchen,100)
sample_kitchen <- as.matrix(sample_kitchen)
friedman.test(sample_kitchen)

# Optmization:

library(optimr)

f2=function(x)
{
  x1 = x[1]
  x2 = x[2]
  return(100 * (x1 - 15)^2 + 20 * (28 - x1)^2 + 100 * (x2 -
                                                         x1)^2 + 20 * (38 - x1 - x2)^2)
}
g2=function(x){
  x1 = x[1]
  x2 = x[2]
  return(c(200 * (x1 - 15) - 40 * (28 - x1) - 200 * (x2 -
                                                       x1)-40 * (38 - x1 - x2), 200*(x2-x1)-40 * (38 - x1 - x2)))
} 

optim(c(10.0,14.0),f2,g2,method="BFGS",control=list(trace=1)) 

