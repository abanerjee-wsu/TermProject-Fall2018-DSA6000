

install.packages("dplyr")

library("readxl")
library("dplyr")
library("tidyr")``

setwd("C:/Users/Robert/Documents/DSA6000/FinalProject/")


my_data <- read_excel("thedata.xlsx")

conversion <- read_excel("ConversionMatrix.xlsx")

my_data<-my_data %>% mutate(Consumption = replace_na(Consumption, 0))
my_data<-my_data %>% mutate(solar_powered__water_heater = replace_na(solar_powered__water_heater, 0))
my_data<-my_data %>% mutate(gas_water_heater = replace_na(gas_water_heater, 0))
my_data<-my_data %>% mutate(electric_water_heater___peak_hou = replace_na(electric_water_heater___peak_hou, 0))
my_data<-my_data %>% mutate(electric_water_heater___off_peak = replace_na(electric_water_heater___off_peak, 0))
my_data<-my_data %>% mutate(gas = replace_na(gas, 0))
my_data<-my_data %>% mutate(natural_gas = replace_na(natural_gas, 0))
my_data<-my_data %>% mutate(hybrid = replace_na(hybrid, 0))
my_data<-my_data %>% mutate(electric___peak_hours = replace_na(electric___peak_hours, 0))
my_data<-my_data %>% mutate(electric___off_peak_hours = replace_na(electric___off_peak_hours, 0))
my_data<-my_data %>% mutate(jetfuel = replace_na(jetfuel, 0))

my_data<-my_data %>% mutate(Activity_Num = 
                     case_when(
                       Activity == 'Household heating => 70F' ~ 1
                       , Activity == 'Household heating < 70F' ~ 2
                       , Activity == 'Use of heat pump' ~ 3
                       , Activity == 'Use of air conditioner' ~ 4
                       , Activity == 'shower - short' ~ 5
                       , Activity == 'shower - long (> 3 min)' ~ 6
                       , Activity == 'bath' ~ 7
                       , Activity == 'wash-up' ~ 8
                       , Activity == 'use of dishwasher' ~ 9
                       , Activity == 'use of clothes washer' ~ 10
                       , Activity == 'use of clothes dryer' ~ 11
                       , Activity == 'use of cooking range' ~ 12
                       , Activity == 'use of self-clean feature of electric oven' ~ 14
                       , grepl('oven',Activity) ~ 13 #use of oven
                       , Activity == 'Small kitchen appliance in the home' ~ 15
                       , Activity == 'TV/computer use' ~ 16
                       , Activity == 'air travel - large plane' ~ 17
                       , grepl('50',Activity) ~ 18 #air travel - small plane (<50 seats)  
                       , Activity == 'car trips- self only' ~ 19
                       , Activity == 'car trips - driver and self' ~ 20
                       , Activity == 'car trips - 2+ people with multiple end points' ~ 21
                       , Activity == 'trips using public ground transportation' ~ 22
                       , Activity == 'bags of garbage disposed' ~ 23
                       , Activity == 'bags of recycling deposited (negative CF)' ~ 24
                       , Activity == 'bags of compost deposited (negative CF)' ~ 25
                       , Activity == 'hazardous or electric items disposed' ~ 26
                       , Activity == 'large items disposed' ~ 27
                       , TRUE ~ 0 
                     ))                     

good_data <- my_data %>% select(Indnum, Group, Activity, Activity_Num, Consumption, Quality_of_Life_Importance__1_10) 
#View(good_data)

# Here we are going to replace NA quality of life importance with the mean for each activity
QL <- good_data %>% group_by(Activity_Num)
QL_1 <- good_data %>% group_by(Indnum)
QL_mean_df <- summarize(QL, QL_mean = mean(Quality_of_Life_Importance__1_10, na.rm=TRUE))
QL_mean_df_1 <- summarize(QL_1, QL_mean_1 = mean(Quality_of_Life_Importance__1_10, na.rm=TRUE))
good_data_1 <- left_join(good_data,QL_mean_df, by = c("Activity_Num" = "Activity_Num"))
good_data_1 <- left_join(good_data_1,QL_mean_df_1, by = c("Indnum" = "Indnum"))
good_data_2 <- good_data_1 %>% mutate(
  QL_cleaned = case_when(Activity_Num ==12 ~ QL_mean_1
                         , Activity_Num ==15 ~ QL_mean_1
                         , Activity_Num ==21 ~ QL_mean_1
                         , Activity_Num ==26 ~ QL_mean_1
                         , is.na(Quality_of_Life_Importance__1_10) ~  QL_mean
                         , TRUE ~ Quality_of_Life_Importance__1_10 )
)



#View(good_data_2)

# This is our final cleaned dataset without the carbon footprint
good_data_3 <- good_data_2 %>% select(Indnum, Group, Activity, Activity_Num, Consumption, QL_cleaned) 














# Now Computing the carbon footprint rate

additionalcolumns <- group_by(my_data, Indnum) %>% summarise(solar_powered__water_heater = max(solar_powered__water_heater)
                                                             , gas_water_heater = max(gas_water_heater)
                                                             , electric_water_heater___peak_hou = max(electric_water_heater___peak_hou)
                                                             , electric_water_heater___off_peak = max(electric_water_heater___off_peak)
                                                             , jetfuel = max(jetfuel)
                                                             , hybrid = max(hybrid)
)

#View(additionalcolumns)


electric___peak_hours_grp1= group_by(my_data, Indnum, Group)%>% summarise(electric___peak_hours_grp1 = max(electric___peak_hours))%>%filter(Group==1)%>%select(-Group)
electric___peak_hours_grp3= group_by(my_data, Indnum, Group)%>% summarise(electric___peak_hours_grp3 = max(electric___peak_hours))%>%filter(Group==3)%>%select(-Group)
electric___peak_hours_grp4= group_by(my_data, Indnum, Group)%>% summarise(electric___peak_hours_grp4 = max(electric___peak_hours))%>%filter(Group==4)%>%select(-Group)
electric___peak_hours_grp5= group_by(my_data, Indnum, Group)%>% summarise(electric___peak_hours_grp5 = max(electric___peak_hours))%>%filter(Group==5)%>%select(-Group)

electric___off_peak_hours_grp1= group_by(my_data, Indnum, Group)%>% summarise(electric___off_peak_hours_grp1 = max(electric___off_peak_hours))%>%filter(Group==1)%>%select(-Group)
electric___off_peak_hours_grp3= group_by(my_data, Indnum, Group)%>% summarise(electric___off_peak_hours_grp3 = max(electric___off_peak_hours))%>%filter(Group==3)%>%select(-Group)
electric___off_peak_hours_grp4= group_by(my_data, Indnum, Group)%>% summarise(electric___off_peak_hours_grp4 = max(electric___off_peak_hours))%>%filter(Group==4)%>%select(-Group)
electric___off_peak_hours_grp5= group_by(my_data, Indnum, Group)%>% summarise(electric___off_peak_hours_grp5 = max(electric___off_peak_hours))%>%filter(Group==5)%>%select(-Group)

natural_gas_grp1= group_by(my_data, Indnum, Group)%>% summarise(natural_gas_grp1 = max(natural_gas))%>%filter(Group==1)%>%select(-Group)
natural_gas_grp3= group_by(my_data, Indnum, Group)%>% summarise(natural_gas_grp3 = max(natural_gas))%>%filter(Group==3)%>%select(-Group)

gas_grp5= group_by(my_data, Indnum, Group)%>% summarise(gas_grp5 = max(gas))%>%filter(Group==5)%>%select(-Group)

additionalcolumns <-  left_join(additionalcolumns,electric___peak_hours_grp1, by = c("Indnum" = "Indnum"))
additionalcolumns <-  left_join(additionalcolumns,electric___peak_hours_grp3, by = c("Indnum" = "Indnum"))
additionalcolumns <-  left_join(additionalcolumns,electric___peak_hours_grp4, by = c("Indnum" = "Indnum"))
additionalcolumns <-  left_join(additionalcolumns,electric___peak_hours_grp5, by = c("Indnum" = "Indnum"))
additionalcolumns <-  left_join(additionalcolumns,electric___off_peak_hours_grp1, by = c("Indnum" = "Indnum"))
additionalcolumns <-  left_join(additionalcolumns,electric___off_peak_hours_grp3, by = c("Indnum" = "Indnum"))
additionalcolumns <-  left_join(additionalcolumns,electric___off_peak_hours_grp4, by = c("Indnum" = "Indnum"))
additionalcolumns <-  left_join(additionalcolumns,electric___off_peak_hours_grp5, by = c("Indnum" = "Indnum"))
additionalcolumns <-  left_join(additionalcolumns,natural_gas_grp1, by = c("Indnum" = "Indnum"))
additionalcolumns <-  left_join(additionalcolumns,natural_gas_grp3, by = c("Indnum" = "Indnum"))
additionalcolumns <-  left_join(additionalcolumns,gas_grp5, by = c("Indnum" = "Indnum"))

additionalcolumns <- additionalcolumns %>% 
  mutate(gas_water_heater_1 = case_when(solar_powered__water_heater == 0
                                  & gas_water_heater == 0
                                  & electric_water_heater___peak_hou == 0
                                  & electric_water_heater___off_peak == 0
                                  ~ 1
                                  ,TRUE ~ gas_water_heater 
                                  ))

additionalcolumns <- additionalcolumns %>% 
  mutate(natural_gas_grp1_cleaned = case_when(natural_gas_grp1 == 0  
                                      & electric___peak_hours_grp1 == 0
                                      & electric___off_peak_hours_grp1 == 0
                                      ~ 1
                                      ,TRUE ~ natural_gas_grp1 
                            ))

additionalcolumns <- additionalcolumns %>% 
  mutate(electric___off_peak_hours_grp3_cleaned = case_when(natural_gas_grp3 == 0  
                                              & electric___peak_hours_grp3 == 0
                                              & electric___off_peak_hours_grp3 == 0
                                              ~ 1
                                              ,TRUE ~ electric___off_peak_hours_grp3 
  ))

additionalcolumns <- additionalcolumns %>% 
  mutate(electric___peak_hours_grp4_cleaned = 1
  )

additionalcolumns <- additionalcolumns %>% 
  mutate(electric___off_peak_hours_grp4_cleaned = 1
  )

additionalcolumns <- additionalcolumns %>% 
  mutate(gas_grp5_cleaned = case_when(gas_grp5 == 0  
                                        & electric___peak_hours_grp5 == 0
                                        & electric___off_peak_hours_grp5 == 0
                                        & hybrid == 0
                                        ~ 1
                                      ,TRUE ~ gas_grp5 
  ))

additionalcolumns <- additionalcolumns %>% 
  mutate(waste_management = 1
  )

additionalcolumns <- additionalcolumns %>% 
  mutate(jetfuel1 = 1
  )

additionalcolumns <- additionalcolumns %>% 
  mutate(Water_Heater_count = solar_powered__water_heater+gas_water_heater_1+electric_water_heater___peak_hou+electric_water_heater___off_peak)

additionalcolumns <- additionalcolumns %>% 
  mutate(Heater_count = natural_gas_grp1_cleaned+electric___peak_hours_grp1+electric___off_peak_hours_grp1)

additionalcolumns <- additionalcolumns %>% 
  mutate(Energy_count = natural_gas_grp3+electric___peak_hours_grp3+electric___off_peak_hours_grp3_cleaned)

additionalcolumns <- additionalcolumns %>% 
  mutate(Grp4_Utility_count = electric___peak_hours_grp4_cleaned+electric___off_peak_hours_grp4_cleaned)

additionalcolumns <- additionalcolumns %>% 
  mutate(Car_count = gas_grp5_cleaned+electric___peak_hours_grp5+electric___off_peak_hours_grp5+hybrid)


cleaned_data <- additionalcolumns %>% select( Indnum
                             , solar_powered__water_heater
                             , gas_water_heater_1
                             , electric_water_heater___peak_hou
                             , electric_water_heater___off_peak
                             , gas_grp5_cleaned
                             , natural_gas_grp1_cleaned
                             , natural_gas_grp3
                             , jetfuel1
                             , waste_management
                             , hybrid
                             , electric___peak_hours_grp1
                             , electric___peak_hours_grp3
                             , electric___peak_hours_grp4_cleaned
                             , electric___peak_hours_grp5
                             , electric___off_peak_hours_grp1
                             , electric___off_peak_hours_grp3_cleaned
                             , electric___off_peak_hours_grp4_cleaned
                             , electric___off_peak_hours_grp5
                             , Water_Heater_count
                             , Heater_count
                             , Energy_count
                             , Grp4_Utility_count
                             , Car_count
                             )

#View(cleaned_data)
#View(conversion)

#View(crossing(cleaned_data,conversion))

cleaned_data_1 <- crossing(cleaned_data,conversion)

#colnames(cleaned_data_1)

cleaned_data_2 <- cleaned_data_1 %>% mutate(
  cf_rate = solar_powered__water_heater*solar_powered_water_heater/Water_Heater_count
    +gas_water_heater_1*gas_water_heater/Water_Heater_count
    +electric_water_heater___peak_hou*electric_water_heater_peak_hours/Water_Heater_count
    +electric_water_heater___off_peak*electric_water_heater_off_peak_hours/Water_Heater_count
    +gas_grp5_cleaned*gas_grp5/Car_count
    +natural_gas_grp1_cleaned*natural_gas_grp1/Heater_count
    +natural_gas_grp3*natural_gas_grp31/Energy_count
    +jetfuel1*Jet_Fuel/100
    +waste_management*waste_management1
    +hybrid*hybrid1/Car_count
    +electric___peak_hours_grp1*electric_peak_hours_grp1/Heater_count
    +electric___peak_hours_grp3*electric_peak_hours_grp3/Energy_count
    +electric___peak_hours_grp4_cleaned*electric_peak_hours_grp4/Grp4_Utility_count
    +electric___peak_hours_grp5*electric_peak_hours_grp5/Car_count
    +electric___off_peak_hours_grp1*electric_off_peak_hours_grp1/Heater_count
    +electric___off_peak_hours_grp3_cleaned*electric_off_peak_hours_grp3/Energy_count
    +electric___off_peak_hours_grp4_cleaned*electric_off_peak_hours_grp4/Grp4_Utility_count
    +electric___off_peak_hours_grp5*electric_off_peak_hours_grp5/Car_count
)

### This is our final dataset with the carbon footprint rates
cleaned_data_3 <- cleaned_data_2 %>% select(Indnum, Activity, Group, Activity_Num, cf_rate)
#View(cleaned_data_3)



# Joining the cleaned consumption/QL data with the cleaned cf_rates

final_data <- left_join(good_data_3,cleaned_data_3, by = c("Indnum" = "Indnum", "Activity_Num"="Activity_Num" )) 
final_data_1 <- final_data %>% select(Indnum,Group.x, Activity.x, Activity_Num, QL_cleaned, Consumption, cf_rate)
final_data_2 <- final_data_1 %>% mutate( carbon_footprint = Consumption*cf_rate)
colnames(final_data_2) <- c("Indnum","Group", "Activity","Activity_Num","QL_cleaned","Consumption","cf_rate","carbon_footprint")
View(final_data_2)

write.csv(final_data_2,'cleaned_data.csv')


