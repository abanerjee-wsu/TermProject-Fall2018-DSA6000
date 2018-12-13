install.packages("dplyr")
install.packages("lpSolve")
install.packages("lpSolveAPI")


require(lpSolve)

library("readxl")
library("dplyr")
library("tidyr")
library("lpSolveAPI")

setwd("C:/Users/Robert/Documents/DSA6000/FinalProject/")


df <- read.csv("cleaned_data.csv")

conversion <- read_excel("ConversionMatrix.xlsx")
#View(conversion)


#View(df)

df1 <- df %>% filter(Activity_Num == 1) %>% select(Indnum, QL_cleaned_1=QL_cleaned) 
df2 <- df %>% filter(Activity_Num == 2) %>% select(Indnum, QL_cleaned_2=QL_cleaned) 
df3 <- df %>% filter(Activity_Num == 3) %>% select(Indnum, QL_cleaned_3=QL_cleaned) 
df4 <- df %>% filter(Activity_Num == 4) %>% select(Indnum, QL_cleaned_4=QL_cleaned) 
df5 <- df %>% filter(Activity_Num == 5) %>% select(Indnum, QL_cleaned_5=QL_cleaned) 
df6 <- df %>% filter(Activity_Num == 6) %>% select(Indnum, QL_cleaned_6=QL_cleaned) 
df7 <- df %>% filter(Activity_Num == 7) %>% select(Indnum, QL_cleaned_7=QL_cleaned) 
df8 <- df %>% filter(Activity_Num == 8) %>% select(Indnum, QL_cleaned_8=QL_cleaned) 
df9 <- df %>% filter(Activity_Num == 9) %>% select(Indnum, QL_cleaned_9=QL_cleaned) 
df10 <- df %>% filter(Activity_Num == 10) %>% select(Indnum, QL_cleaned_10=QL_cleaned) 
df11 <- df %>% filter(Activity_Num == 11) %>% select(Indnum, QL_cleaned_11=QL_cleaned) 
df12 <- df %>% filter(Activity_Num == 12) %>% select(Indnum, QL_cleaned_12=QL_cleaned) 
df13 <- df %>% filter(Activity_Num == 13) %>% select(Indnum, QL_cleaned_13=QL_cleaned) 
df14 <- df %>% filter(Activity_Num == 14) %>% select(Indnum, QL_cleaned_14=QL_cleaned) 
df15 <- df %>% filter(Activity_Num == 15) %>% select(Indnum, QL_cleaned_15=QL_cleaned) 
df16 <- df %>% filter(Activity_Num == 16) %>% select(Indnum, QL_cleaned_16=QL_cleaned) 
df17 <- df %>% filter(Activity_Num == 17) %>% select(Indnum, QL_cleaned_17=QL_cleaned) 
df18 <- df %>% filter(Activity_Num == 18) %>% select(Indnum, QL_cleaned_18=QL_cleaned) 
df19 <- df %>% filter(Activity_Num == 19) %>% select(Indnum, QL_cleaned_19=QL_cleaned) 
df20 <- df %>% filter(Activity_Num == 20) %>% select(Indnum, QL_cleaned_20=QL_cleaned) 
df21 <- df %>% filter(Activity_Num == 21) %>% select(Indnum, QL_cleaned_21=QL_cleaned) 
df22 <- df %>% filter(Activity_Num == 22) %>% select(Indnum, QL_cleaned_22=QL_cleaned)
df23 <- df %>% filter(Activity_Num == 23) %>% select(Indnum, QL_cleaned_23=QL_cleaned) 
df24 <- df %>% filter(Activity_Num == 24) %>% select(Indnum, QL_cleaned_24=QL_cleaned) 
df25 <- df %>% filter(Activity_Num == 25) %>% select(Indnum, QL_cleaned_25=QL_cleaned) 
df26 <- df %>% filter(Activity_Num == 26) %>% select(Indnum, QL_cleaned_26=QL_cleaned) 
df27 <- df %>% filter(Activity_Num == 27) %>% select(Indnum, QL_cleaned_27=QL_cleaned) 

QL <- left_join(df1,df2, by = c("Indnum" = "Indnum")) 
QL <- left_join(QL,df3, by = c("Indnum" = "Indnum")) 
QL <- left_join(QL,df4, by = c("Indnum" = "Indnum")) 
QL <- left_join(QL,df5, by = c("Indnum" = "Indnum")) 
QL <- left_join(QL,df6, by = c("Indnum" = "Indnum")) 
QL <- left_join(QL,df7, by = c("Indnum" = "Indnum")) 
QL <- left_join(QL,df8, by = c("Indnum" = "Indnum")) 
QL <- left_join(QL,df9, by = c("Indnum" = "Indnum")) 
QL <- left_join(QL,df10, by = c("Indnum" = "Indnum")) 
QL <- left_join(QL,df11, by = c("Indnum" = "Indnum")) 
QL <- left_join(QL,df12, by = c("Indnum" = "Indnum")) 
QL <- left_join(QL,df13, by = c("Indnum" = "Indnum")) 
QL <- left_join(QL,df14, by = c("Indnum" = "Indnum")) 
QL <- left_join(QL,df15, by = c("Indnum" = "Indnum")) 
QL <- left_join(QL,df16, by = c("Indnum" = "Indnum")) 
QL <- left_join(QL,df17, by = c("Indnum" = "Indnum")) 
QL <- left_join(QL,df18, by = c("Indnum" = "Indnum")) 
QL <- left_join(QL,df19, by = c("Indnum" = "Indnum")) 
QL <- left_join(QL,df20, by = c("Indnum" = "Indnum")) 
QL <- left_join(QL,df21, by = c("Indnum" = "Indnum")) 
QL <- left_join(QL,df22, by = c("Indnum" = "Indnum")) 
QL <- left_join(QL,df23, by = c("Indnum" = "Indnum")) 
QL <- left_join(QL,df24, by = c("Indnum" = "Indnum")) 
QL <- left_join(QL,df25, by = c("Indnum" = "Indnum")) 
QL <- left_join(QL,df26, by = c("Indnum" = "Indnum")) 
QL <- left_join(QL,df27, by = c("Indnum" = "Indnum")) 
QL <- QL %>% mutate(QL_Sum =QL_cleaned_1+QL_cleaned_2+QL_cleaned_3+QL_cleaned_4) %>% 
  mutate(QL_Sum_2 =QL_cleaned_5+QL_cleaned_6+QL_cleaned_7+QL_cleaned_8) %>%
  mutate(QL_Sum_3 =QL_cleaned_9+QL_cleaned_10+QL_cleaned_11+QL_cleaned_12+QL_cleaned_13+QL_cleaned_14+QL_cleaned_15) %>%
  mutate(QL_Sum_5 =QL_cleaned_17+QL_cleaned_18+QL_cleaned_19+QL_cleaned_20+QL_cleaned_21+QL_cleaned_22)
#+QL_cleaned_23+QL_cleaned_24+QL_cleaned_25+QL_cleaned_26+QL_cleaned_27)

#View(QL)

QL_Normalized <-QL %>%mutate(QL_cleaned_1=QL_cleaned_1/QL_Sum)
QL_Normalized <-QL_Normalized %>%mutate(QL_cleaned_2=QL_cleaned_2/QL_Sum)
QL_Normalized <-QL_Normalized %>%mutate(QL_cleaned_3=QL_cleaned_3/QL_Sum)
QL_Normalized <-QL_Normalized %>%mutate(QL_cleaned_4=QL_cleaned_4/QL_Sum)
QL_Normalized <-QL_Normalized %>%mutate(QL_cleaned_5=QL_cleaned_5/QL_Sum_2)
QL_Normalized <-QL_Normalized %>%mutate(QL_cleaned_6=QL_cleaned_6/QL_Sum_2)
QL_Normalized <-QL_Normalized %>%mutate(QL_cleaned_7=QL_cleaned_7/QL_Sum_2)
QL_Normalized <-QL_Normalized %>%mutate(QL_cleaned_8=QL_cleaned_8/QL_Sum_2)
QL_Normalized <-QL_Normalized %>%mutate(QL_cleaned_9=QL_cleaned_9/QL_Sum_3)
QL_Normalized <-QL_Normalized %>%mutate(QL_cleaned_10=QL_cleaned_10/QL_Sum_3)
QL_Normalized <-QL_Normalized %>%mutate(QL_cleaned_11=QL_cleaned_11/QL_Sum_3)
QL_Normalized <-QL_Normalized %>%mutate(QL_cleaned_12=QL_cleaned_12/QL_Sum_3)
QL_Normalized <-QL_Normalized %>%mutate(QL_cleaned_13=QL_cleaned_13/QL_Sum_3)
QL_Normalized <-QL_Normalized %>%mutate(QL_cleaned_14=QL_cleaned_14/QL_Sum_3)
QL_Normalized <-QL_Normalized %>%mutate(QL_cleaned_15=QL_cleaned_15/QL_Sum_3)
QL_Normalized <-QL_Normalized %>%mutate(QL_cleaned_16=QL_cleaned_16)
QL_Normalized <-QL_Normalized %>%mutate(QL_cleaned_17=QL_cleaned_17/QL_Sum_5)
QL_Normalized <-QL_Normalized %>%mutate(QL_cleaned_18=QL_cleaned_18/QL_Sum_5)
QL_Normalized <-QL_Normalized %>%mutate(QL_cleaned_19=QL_cleaned_19/QL_Sum_5)
QL_Normalized <-QL_Normalized %>%mutate(QL_cleaned_20=QL_cleaned_20/QL_Sum_5)
QL_Normalized <-QL_Normalized %>%mutate(QL_cleaned_21=QL_cleaned_21/QL_Sum_5)
QL_Normalized <-QL_Normalized %>%mutate(QL_cleaned_22=QL_cleaned_22/QL_Sum_5)
QL_Normalized <-QL_Normalized %>%mutate(QL_cleaned_23=QL_cleaned_23/QL_Sum)
QL_Normalized <-QL_Normalized %>%mutate(QL_cleaned_24=QL_cleaned_24/QL_Sum)
QL_Normalized <-QL_Normalized %>%mutate(QL_cleaned_25=QL_cleaned_25/QL_Sum)
QL_Normalized <-QL_Normalized %>%mutate(QL_cleaned_26=QL_cleaned_26/QL_Sum)
QL_Normalized <-QL_Normalized %>%mutate(QL_cleaned_27=QL_cleaned_27/QL_Sum)
#View(QL_Normalized)



df1 <- df %>% filter(Activity_Num == 1) %>% select(Indnum, Consumption_1=Consumption) 
df2 <- df %>% filter(Activity_Num == 2) %>% select(Indnum, Consumption_2=Consumption) 
df3 <- df %>% filter(Activity_Num == 3) %>% select(Indnum, Consumption_3=Consumption) 
df4 <- df %>% filter(Activity_Num == 4) %>% select(Indnum, Consumption_4=Consumption) 
df5 <- df %>% filter(Activity_Num == 5) %>% select(Indnum, Consumption_5=Consumption) 
df6 <- df %>% filter(Activity_Num == 6) %>% select(Indnum, Consumption_6=Consumption) 
df7 <- df %>% filter(Activity_Num == 7) %>% select(Indnum, Consumption_7=Consumption) 
df8 <- df %>% filter(Activity_Num == 8) %>% select(Indnum, Consumption_8=Consumption) 
df9 <- df %>% filter(Activity_Num == 9) %>% select(Indnum, Consumption_9=Consumption) 
df10 <- df %>% filter(Activity_Num == 10) %>% select(Indnum, Consumption_10=Consumption) 
df11 <- df %>% filter(Activity_Num == 11) %>% select(Indnum, Consumption_11=Consumption) 
df12 <- df %>% filter(Activity_Num == 12) %>% select(Indnum, Consumption_12=Consumption) 
df13 <- df %>% filter(Activity_Num == 13) %>% select(Indnum, Consumption_13=Consumption) 
df14 <- df %>% filter(Activity_Num == 14) %>% select(Indnum, Consumption_14=Consumption) 
df15 <- df %>% filter(Activity_Num == 15) %>% select(Indnum, Consumption_15=Consumption) 
df16 <- df %>% filter(Activity_Num == 16) %>% select(Indnum, Consumption_16=Consumption) 
df17 <- df %>% filter(Activity_Num == 17) %>% select(Indnum, Consumption_17=Consumption) 
df18 <- df %>% filter(Activity_Num == 18) %>% select(Indnum, Consumption_18=Consumption) 
df19 <- df %>% filter(Activity_Num == 19) %>% select(Indnum, Consumption_19=Consumption) 
df20 <- df %>% filter(Activity_Num == 20) %>% select(Indnum, Consumption_20=Consumption) 
df21 <- df %>% filter(Activity_Num == 21) %>% select(Indnum, Consumption_21=Consumption) 
df22 <- df %>% filter(Activity_Num == 22) %>% select(Indnum, Consumption_22=Consumption)
df23 <- df %>% filter(Activity_Num == 23) %>% select(Indnum, Consumption_23=Consumption) 
df24 <- df %>% filter(Activity_Num == 24) %>% select(Indnum, Consumption_24=Consumption) 
df25 <- df %>% filter(Activity_Num == 25) %>% select(Indnum, Consumption_25=Consumption) 
df26 <- df %>% filter(Activity_Num == 26) %>% select(Indnum, Consumption_26=Consumption) 
df27 <- df %>% filter(Activity_Num == 27) %>% select(Indnum, Consumption_27=Consumption) 

Consume <- left_join(df1,df2, by = c("Indnum" = "Indnum")) 
Consume <- left_join(Consume,df3, by = c("Indnum" = "Indnum")) 
Consume <- left_join(Consume,df4, by = c("Indnum" = "Indnum")) 
Consume <- left_join(Consume,df5, by = c("Indnum" = "Indnum")) 
Consume <- left_join(Consume,df6, by = c("Indnum" = "Indnum")) 
Consume <- left_join(Consume,df7, by = c("Indnum" = "Indnum")) 
Consume <- left_join(Consume,df8, by = c("Indnum" = "Indnum")) 
Consume <- left_join(Consume,df9, by = c("Indnum" = "Indnum")) 
Consume <- left_join(Consume,df10, by = c("Indnum" = "Indnum")) 
Consume <- left_join(Consume,df11, by = c("Indnum" = "Indnum")) 
Consume <- left_join(Consume,df12, by = c("Indnum" = "Indnum")) 
Consume <- left_join(Consume,df13, by = c("Indnum" = "Indnum")) 
Consume <- left_join(Consume,df14, by = c("Indnum" = "Indnum")) 
Consume <- left_join(Consume,df15, by = c("Indnum" = "Indnum")) 
Consume <- left_join(Consume,df16, by = c("Indnum" = "Indnum")) 
Consume <- left_join(Consume,df17, by = c("Indnum" = "Indnum")) 
Consume <- left_join(Consume,df18, by = c("Indnum" = "Indnum")) 
Consume <- left_join(Consume,df19, by = c("Indnum" = "Indnum")) 
Consume <- left_join(Consume,df20, by = c("Indnum" = "Indnum")) 
Consume <- left_join(Consume,df21, by = c("Indnum" = "Indnum")) 
Consume <- left_join(Consume,df22, by = c("Indnum" = "Indnum")) 
Consume <- left_join(Consume,df23, by = c("Indnum" = "Indnum")) 
Consume <- left_join(Consume,df24, by = c("Indnum" = "Indnum")) 
Consume <- left_join(Consume,df25, by = c("Indnum" = "Indnum")) 
Consume <- left_join(Consume,df26, by = c("Indnum" = "Indnum")) 
Consume <- left_join(Consume,df27, by = c("Indnum" = "Indnum")) 

Consume <- Consume %>% mutate(Consumption_3=-Consumption_3)

#View(Consume)

#QL_understanding <- left_join(Consume,QL_Normalized, by = c("Indnum" = "Indnum")) 
#View(QL_understanding)
summary(QL_understanding)
View(cor(QL_understanding))
#plot(QL_understanding$Consumption_1, QL_understanding$QL_cleaned_1)

#plot(QL_understanding$Consumption_1, QL_understanding$Consumption_2)

#plot(QL_understanding$Consumption_2, QL_understanding$Consumption_4)

#plot(lm(formula = QL_understanding$Consumption_1 ~ QL_understanding$Consumption_2))


df1 <- df %>% filter(Activity_Num == 1) %>% select(Indnum, cf_rate_1=cf_rate) 
df2 <- df %>% filter(Activity_Num == 2) %>% select(Indnum, cf_rate_2=cf_rate) 
df3 <- df %>% filter(Activity_Num == 3) %>% select(Indnum, cf_rate_3=cf_rate) 
df4 <- df %>% filter(Activity_Num == 4) %>% select(Indnum, cf_rate_4=cf_rate) 
df5 <- df %>% filter(Activity_Num == 5) %>% select(Indnum, cf_rate_5=cf_rate) 
df6 <- df %>% filter(Activity_Num == 6) %>% select(Indnum, cf_rate_6=cf_rate) 
df7 <- df %>% filter(Activity_Num == 7) %>% select(Indnum, cf_rate_7=cf_rate) 
df8 <- df %>% filter(Activity_Num == 8) %>% select(Indnum, cf_rate_8=cf_rate) 
df9 <- df %>% filter(Activity_Num == 9) %>% select(Indnum, cf_rate_9=cf_rate) 
df10 <- df %>% filter(Activity_Num == 10) %>% select(Indnum, cf_rate_10=cf_rate) 
df11 <- df %>% filter(Activity_Num == 11) %>% select(Indnum, cf_rate_11=cf_rate) 
df12 <- df %>% filter(Activity_Num == 12) %>% select(Indnum, cf_rate_12=cf_rate) 
df13 <- df %>% filter(Activity_Num == 13) %>% select(Indnum, cf_rate_13=cf_rate) 
df14 <- df %>% filter(Activity_Num == 14) %>% select(Indnum, cf_rate_14=cf_rate) 
df15 <- df %>% filter(Activity_Num == 15) %>% select(Indnum, cf_rate_15=cf_rate) 
df16 <- df %>% filter(Activity_Num == 16) %>% select(Indnum, cf_rate_16=cf_rate) 
df17 <- df %>% filter(Activity_Num == 17) %>% select(Indnum, cf_rate_17=cf_rate) 
df18 <- df %>% filter(Activity_Num == 18) %>% select(Indnum, cf_rate_18=cf_rate) 
df19 <- df %>% filter(Activity_Num == 19) %>% select(Indnum, cf_rate_19=cf_rate) 
df20 <- df %>% filter(Activity_Num == 20) %>% select(Indnum, cf_rate_20=cf_rate) 
df21 <- df %>% filter(Activity_Num == 21) %>% select(Indnum, cf_rate_21=cf_rate) 
df22 <- df %>% filter(Activity_Num == 22) %>% select(Indnum, cf_rate_22=cf_rate)
df23 <- df %>% filter(Activity_Num == 23) %>% select(Indnum, cf_rate_23=cf_rate) 
df24 <- df %>% filter(Activity_Num == 24) %>% select(Indnum, cf_rate_24=cf_rate) 
df25 <- df %>% filter(Activity_Num == 25) %>% select(Indnum, cf_rate_25=cf_rate) 
df26 <- df %>% filter(Activity_Num == 26) %>% select(Indnum, cf_rate_26=cf_rate) 
df27 <- df %>% filter(Activity_Num == 27) %>% select(Indnum, cf_rate_27=cf_rate) 

CF <- left_join(df1,df2, by = c("Indnum" = "Indnum")) 
CF <- left_join(CF,df3, by = c("Indnum" = "Indnum")) 
CF <- left_join(CF,df4, by = c("Indnum" = "Indnum")) 
CF <- left_join(CF,df5, by = c("Indnum" = "Indnum")) 
CF <- left_join(CF,df6, by = c("Indnum" = "Indnum")) 
CF <- left_join(CF,df7, by = c("Indnum" = "Indnum")) 
CF <- left_join(CF,df8, by = c("Indnum" = "Indnum")) 
CF <- left_join(CF,df9, by = c("Indnum" = "Indnum")) 
CF <- left_join(CF,df10, by = c("Indnum" = "Indnum")) 
CF <- left_join(CF,df11, by = c("Indnum" = "Indnum")) 
CF <- left_join(CF,df12, by = c("Indnum" = "Indnum")) 
CF <- left_join(CF,df13, by = c("Indnum" = "Indnum")) 
CF <- left_join(CF,df14, by = c("Indnum" = "Indnum")) 
CF <- left_join(CF,df15, by = c("Indnum" = "Indnum")) 
CF <- left_join(CF,df16, by = c("Indnum" = "Indnum")) 
CF <- left_join(CF,df17, by = c("Indnum" = "Indnum")) 
CF <- left_join(CF,df18, by = c("Indnum" = "Indnum")) 
CF <- left_join(CF,df19, by = c("Indnum" = "Indnum")) 
CF <- left_join(CF,df20, by = c("Indnum" = "Indnum")) 
CF <- left_join(CF,df21, by = c("Indnum" = "Indnum")) 
CF <- left_join(CF,df22, by = c("Indnum" = "Indnum")) 
CF <- left_join(CF,df23, by = c("Indnum" = "Indnum")) 
CF <- left_join(CF,df24, by = c("Indnum" = "Indnum")) 
CF <- left_join(CF,df25, by = c("Indnum" = "Indnum")) 
CF <- left_join(CF,df26, by = c("Indnum" = "Indnum")) 
CF <- left_join(CF,df27, by = c("Indnum" = "Indnum")) 

CF <- CF %>% mutate(cf_rate_3=-cf_rate_3)

CF <- CF %>% mutate(CF_Sum =cf_rate_1+cf_rate_2+abs(cf_rate_3)+cf_rate_4) %>% 
  mutate(CF_Sum_2 =cf_rate_5+cf_rate_6+cf_rate_7+cf_rate_8) %>% 
  mutate(CF_Sum_3 =cf_rate_9+cf_rate_10+cf_rate_11+cf_rate_12+cf_rate_13+cf_rate_14+cf_rate_15) %>%
  mutate(CF_Sum_5 =cf_rate_17+cf_rate_18+cf_rate_19+cf_rate_20+cf_rate_21+cf_rate_22)


#+cf_rate_23+cf_rate_24+cf_rate_25+cf_rate_26+cf_rate_27)



#View(CF)

CF_Normalized <-CF %>%mutate(cf_rate_1=cf_rate_1/CF_Sum)
CF_Normalized <-CF_Normalized %>%mutate(cf_rate_2=cf_rate_2/CF_Sum)
CF_Normalized <-CF_Normalized %>%mutate(cf_rate_3=cf_rate_3/CF_Sum)
CF_Normalized <-CF_Normalized %>%mutate(cf_rate_4=cf_rate_4/CF_Sum)
CF_Normalized <-CF_Normalized %>%mutate(cf_rate_5=cf_rate_5/CF_Sum_2)
CF_Normalized <-CF_Normalized %>%mutate(cf_rate_6=cf_rate_6/CF_Sum_2)
CF_Normalized <-CF_Normalized %>%mutate(cf_rate_7=cf_rate_7/CF_Sum_2)
CF_Normalized <-CF_Normalized %>%mutate(cf_rate_8=cf_rate_8/CF_Sum_2)
CF_Normalized <-CF_Normalized %>%mutate(cf_rate_9=cf_rate_9/CF_Sum_3)
CF_Normalized <-CF_Normalized %>%mutate(cf_rate_10=cf_rate_10/CF_Sum_3)
CF_Normalized <-CF_Normalized %>%mutate(cf_rate_11=cf_rate_11/CF_Sum_3)
CF_Normalized <-CF_Normalized %>%mutate(cf_rate_12=cf_rate_12/CF_Sum_3)
CF_Normalized <-CF_Normalized %>%mutate(cf_rate_13=cf_rate_13/CF_Sum_3)
CF_Normalized <-CF_Normalized %>%mutate(cf_rate_14=cf_rate_14/CF_Sum_3)
CF_Normalized <-CF_Normalized %>%mutate(cf_rate_15=cf_rate_15/CF_Sum_3)
CF_Normalized <-CF_Normalized %>%mutate(cf_rate_16=cf_rate_16/CF_Sum)
CF_Normalized <-CF_Normalized %>%mutate(cf_rate_17=cf_rate_17/CF_Sum_5)
CF_Normalized <-CF_Normalized %>%mutate(cf_rate_18=cf_rate_18/CF_Sum_5)
CF_Normalized <-CF_Normalized %>%mutate(cf_rate_19=cf_rate_19/CF_Sum_5)
CF_Normalized <-CF_Normalized %>%mutate(cf_rate_20=cf_rate_20/CF_Sum_5)
CF_Normalized <-CF_Normalized %>%mutate(cf_rate_21=cf_rate_21/CF_Sum_5)
CF_Normalized <-CF_Normalized %>%mutate(cf_rate_22=cf_rate_22/CF_Sum_5)
CF_Normalized <-CF_Normalized %>%mutate(cf_rate_23=cf_rate_23/CF_Sum)
CF_Normalized <-CF_Normalized %>%mutate(cf_rate_24=cf_rate_24/CF_Sum)
CF_Normalized <-CF_Normalized %>%mutate(cf_rate_25=cf_rate_25/CF_Sum)
CF_Normalized <-CF_Normalized %>%mutate(cf_rate_26=cf_rate_26/CF_Sum)
CF_Normalized <-CF_Normalized %>%mutate(cf_rate_27=cf_rate_27/CF_Sum)
#View(CF_Normalized)

QL_Normalized_1 <- QL_Normalized %>%mutate(Indnum=Indnum*2)

#View(QL_Normalized_1)
#View(CF_Normalized)

Decision_DF <- QL_Normalized_1-CF_Normalized
#View(Decision_DF)


##################################################################################################################33
##################################################################################################################33
##################################################################################################################33


# Create constraint martix B
# x1 + x2 >= x1_i + x2_i
# x5 + x6 + x7 + x8 >= x5_i + x6_i + x7_i + x8_i
# x10 - x11 >= 0
# x12 + x13 >= x12_i + x13_i
# x17 + x18 >= x17_i + x18_i
# x19 + x20 + x21 + x22 >= x19_i + x20_i + x21_i + x22_i


##################################################################################################################33
##################################################################################################################33
##################################################################################################################33
############################# Group 1 #############################################################################3

indv_optimize_1 <- function(person) {
  
  #person=1
  C <- Decision_DF %>% filter(Indnum == person) %>% select(QL_cleaned_1,QL_cleaned_2,QL_cleaned_3,QL_cleaned_4)
  
  D<- QL_Normalized  %>% filter(Indnum == person)
  
  Consume_1 <- Consume %>% filter(Indnum == person) %>% select(Consumption_1,Consumption_2,Consumption_3,Consumption_4)
  
  lb_1 <- Consume_1 %>% 
    mutate(LB1=ceiling(D$QL_cleaned_1*Consumption_1)) %>% 
    mutate(LB2=ceiling(D$QL_cleaned_2*Consumption_2)) %>% 
    mutate(LB3=ceiling(D$QL_cleaned_3*Consumption_3)) %>% 
    mutate(LB4=ceiling(D$QL_cleaned_4*Consumption_4))
  lb <- lb_1  %>% select(LB1,LB2,LB3,LB4)
  
  ub_1 <- Consume_1 %>% 
    mutate(UB1=ceiling(D$QL_cleaned_1^-1*Consumption_1)) %>% 
    mutate(UB2=ceiling(D$QL_cleaned_2^-1*Consumption_2)) %>% 
    mutate(UB3=ceiling(D$QL_cleaned_3^-1*Consumption_3)) %>% 
    mutate(UB4=ceiling(D$QL_cleaned_4^-1*Consumption_4))
  ub <- ub_1  %>% select(UB1,UB2,UB3,UB4)
  
  # Creating_B <- Consume_1 %>% 
  #      mutate(B1=Consumption_1+Consumption_2) %>% 
  #      mutate(B2=0) %>% 
  #      mutate(B3=0) %>% 
  #      mutate(B4=3*Consumption_4)
  
  # mutate(B1=Consumption_1+Consumption_2) %>% 
  #mutate(B3=3*Consumption_3) %>% 
  
  Creating_B <- Consume_1 %>% 
    mutate(B1=Consumption_1+Consumption_2) #%>% 
    #mutate(B2=D$QL_cleaned_2^-1*Consumption_2) %>% 
    #mutate(B3=D$QL_cleaned_3^-1*Consumption_3) %>% 
    #mutate(B4=D$QL_cleaned_4^-1*Consumption_4)
  
  # mutate(B1=Consumption_1+Consumption_2) %>% 
  #mutate(B3=3*Consumption_3) %>% 
  
  plot(Consume$Consumption_3~Consume$Consumption_1 )  
  
  B <- Creating_B  %>% select(B1)#,B2,B3,B4)
  
  #    A <- matrix(c(1,1,0,0 ,
  #                  0,1,0,-1 ,
  #                  1,0,-2,0 ,
  #                  0,0,0,1
  #                  
  #    ), nrow=4, byrow=TRUE)
  
  A <- matrix(c(1,1,0,0 ,
                0,1,0,0 ,
                0,0,1,0 ,
                0,0,0,1
                
  ), nrow=4, byrow=TRUE)
  
  lprec <- make.lp(4,4)
  lp.control(lprec,sense='max')
  #?make.lp
  set.objfn(lprec, as.numeric(C[1,]) )
  ColNames <- c("Com_1", "Com_2", "Com_3", "Com_4")
  add.constraint(lprec, A[1,], "=", as.numeric(B[1,1]))
  #add.constraint(lprec, A[2,], "<=", as.numeric(B[1,2]))
  #add.constraint(lprec, A[3,], "<=", as.numeric(B[1,3])) #add.constraint(lprec, A[3,], ">=", as.numeric(B[1,3]))
  #add.constraint(lprec, A[4,], "<=", as.numeric(B[1,4]))
  set.bounds(lprec, lower = lb, columns = c(1,2,3,4))
  set.bounds(lprec, upper = ub, columns = c(1,2,3,4))
  #set.bounds(lprec, lower = .5*c(Consume_1$Consumption_1,Consume_1$Consumption_2,Consume_1$Consumption_3,Consume_1$Consumption_4), columns = c("Com_1", "Com_2", "Com_3", "Com_4"))
  #set.bounds(lprec, upper = 48.98, columns = 4)
  #RowNames <- c("THISROW", "THATROW", "LASTROW")
  #ColNames <- c("Com_1", "Com_2", "Com_3", "Com_4")
  
  #print(get.objective(lprec))
  #print(get.variables(lprec))
  # print(get.constraints(lprec))
  
  print(solve(lprec))
  
  sol<-get.variables(lprec)
  
  
  
  return(sol)
  
  
  # solve(lprec)
  # get.objective(lprec)
  # get.variables(lprec)
  # get.constraints(lprec)
}

indv_optimize_1(1)



##################################################################################################################33
##################################################################################################################33
##################################################################################################################33

#indv_optimize(1)

#create our output dataframe with no data

optimum_df <- data.frame(matrix(ncol = 5, nrow = 0))


#This inserts into our dataframe
for (i in 1:1002){
  df_i <- data.frame(t(c(i,indv_optimize_1(i)))) # this transposes the data before inserting into the dataframe
  optimum_df<-rbind(optimum_df, df_i)
}

colnames(optimum_df) <- c("Indnum", "Com_1", "Com_2", "Com_3", "Com_4")
print(optimum_df)
#print(c(i,indv_optimize(i)))

#This is to see how many tests are failing
#optimum_df %>% filter(Com_1==0 & Com_2==0 & Com_3==0 & Com_4==0)



##################################################################################################################33
##################################################################################################################33
##################################################################################################################33
############################# Group 2 #############################################################################3


indv_optimize_2 <- function(person) {
  
  #person=34
  C <- Decision_DF %>% filter(Indnum == person) %>% select(
    QL_cleaned_5,QL_cleaned_6,QL_cleaned_7,QL_cleaned_8)
  
  D<- QL_Normalized  %>% filter(Indnum == person)
  
  #v1 <- D$QL_cleaned_5/D$QL_cleaned_6
  
  Consume_1 <- Consume %>% filter(Indnum == person) %>% select(Consumption_5,Consumption_6,Consumption_7,Consumption_8)
  
  lb_1 <- Consume_1 %>% 
    mutate(LB1=ceiling(D$QL_cleaned_5*Consumption_5)) %>% 
    mutate(LB2=ceiling(D$QL_cleaned_6*Consumption_6)) %>% 
    mutate(LB3=ceiling(D$QL_cleaned_7*Consumption_7)) %>% 
    mutate(LB4=ceiling(D$QL_cleaned_8*Consumption_8))
  lb <- lb_1  %>% select(LB1,LB2,LB3,LB4)
  
  ub_1 <- Consume_1 %>% 
    mutate(UB1=ceiling(D$QL_cleaned_5^-1*Consumption_5)) %>% 
    mutate(UB2=ceiling(D$QL_cleaned_6^-1*Consumption_6)) %>% 
    mutate(UB3=ceiling(D$QL_cleaned_7^-1*Consumption_7)) %>% 
    mutate(UB4=ceiling(D$QL_cleaned_8^-1*Consumption_8))
  ub <- ub_1  %>% select(UB1,UB2,UB3,UB4)
  
  
  Creating_B <- Consume_1 %>% 
    mutate(B1=Consumption_5+Consumption_6+Consumption_7+Consumption_8) #%>% 
   # mutate(B2=ceiling(D$QL_cleaned_6^-1*Consumption_6)) %>% 
   # mutate(B3=ceiling(D$QL_cleaned_7^-1*Consumption_7)) %>% 
   # mutate(B4=ceiling(D$QL_cleaned_8^-1*Consumption_8))
  
  #Creating_B <- Consume_1 %>% 
  # mutate(B1=Consumption_5+Consumption_6+Consumption_7+Consumption_8) %>% 
  # mutate(B2=ceiling(0)) %>% 
  #  mutate(B3=ceiling(1.5*Consumption_7)) %>% 
  # mutate(B4=ceiling(1.5*Consumption_8))  
  
  # mutate(B1=Consumption_1+Consumption_2) %>% 
  
  
  
  
  B <- Creating_B  %>% select(B1)#,B2,B3,B4)
  
  A <- matrix(c(1,1,1,1 ,
                0,1,0,0 ,
                0,0,1,0 ,
                0,0,0,1
                
  ), nrow=4, byrow=TRUE)
  
  #A <- matrix(c(1,1,1,1 ,
  #              0,1,0,0 ,
  #              0,0,1,0 ,
  #              0,0,0,1
  #), nrow=4, byrow=TRUE)
  
  lprec <- make.lp(4,4)
  lp.control(lprec,sense='max')
  #?make.lp
  set.objfn(lprec, as.numeric(C[1,]) )
  ColNames <- c("Com_1", "Com_2", "Com_3", "Com_4")
  add.constraint(lprec, A[1,], "=", as.numeric(B[1,1]))
  #add.constraint(lprec, A[2,], "<=", as.numeric(B[1,2]))
  #add.constraint(lprec, A[3,], "<=", as.numeric(B[1,3]))
  #add.constraint(lprec, A[4,], "<=", as.numeric(B[1,4]))
  set.bounds(lprec, lower = lb, columns = c(1,2,3,4))
  set.bounds(lprec, upper = ub, columns = c(1,2,3,4))
  #set.bounds(lprec, lower = .5*c(Consume_1$Consumption_1,Consume_1$Consumption_2,Consume_1$Consumption_3,Consume_1$Consumption_4), columns = c("Com_1", "Com_2", "Com_3", "Com_4"))
  #set.bounds(lprec, upper = 48.98, columns = 4)
  #RowNames <- c("THISROW", "THATROW", "LASTROW")
  #ColNames <- c("Com_1", "Com_2", "Com_3", "Com_4")
  
  #print(get.objective(lprec))
  #print(get.variables(lprec))
  # print(get.constraints(lprec))
  
  print(solve(lprec))
  
  sol<-get.variables(lprec)
  
  
  
  return(sol)
  
  
  # solve(lprec)
  # get.objective(lprec)
  # get.variables(lprec)
  # get.constraints(lprec)
}

indv_optimize_2(1)



##################################################################################################################33
##################################################################################################################33
##################################################################################################################33

#indv_optimize(1)

#create our output dataframe with no data

optimum_df_2 <- data.frame(matrix(ncol = 5, nrow = 0))


#This inserts into our dataframe
for (i in 1:1002){
  df_i <- data.frame(t(c(i,indv_optimize_2(i)))) # this transposes the data before inserting into the dataframe
  optimum_df_2<-rbind(optimum_df_2, df_i)
}

colnames(optimum_df_2) <- c("Indnum", "Com_5", "Com_6", "Com_7", "Com_8")
print(optimum_df_2)
#print(c(i,indv_optimize(i)))

#This is to see how many tests are failing
#optimum_df %>% filter(Com_1==0 & Com_2==0 & Com_3==0 & Com_4==0)


##################################################################################################################33
##################################################################################################################33
##################################################################################################################33
############################# Group 3 #############################################################################3


indv_optimize_3 <- function(person) {
  
  #person=1
  C <- Decision_DF %>% filter(Indnum == person) %>% select(
    QL_cleaned_9,QL_cleaned_10,QL_cleaned_11,QL_cleaned_12,QL_cleaned_13,QL_cleaned_14,QL_cleaned_15)
  
  D<- QL_Normalized  %>% filter(Indnum == person)
  
  Consume_1 <- Consume %>% filter(Indnum == person) %>% select(Consumption_9,Consumption_10,Consumption_11,Consumption_12,Consumption_13,Consumption_14,Consumption_15)
  
  lb_1 <- Consume_1 %>% 
    mutate(LB1=ceiling(D$QL_cleaned_9*Consumption_9)) %>% 
    mutate(LB2=ceiling(D$QL_cleaned_10*Consumption_10)) %>% 
    mutate(LB3=ceiling(D$QL_cleaned_11*Consumption_11)) %>%
    mutate(LB4=ceiling(D$QL_cleaned_12*Consumption_12)) %>% 
    mutate(LB5=ceiling(D$QL_cleaned_13*Consumption_13)) %>% 
    mutate(LB6=ceiling(D$QL_cleaned_14*Consumption_14)) %>%  
    mutate(LB7=ceiling(D$QL_cleaned_15*Consumption_15))
  
  lb <- lb_1  %>% select(LB1,LB2,LB3,LB4,LB5,LB6,LB7)
  
  ub_1 <- Consume_1 %>% 
    mutate(UB1=ceiling(D$QL_cleaned_9^-1*Consumption_9)) %>% 
    mutate(UB2=ceiling(D$QL_cleaned_10^-1*Consumption_10)) %>% 
    mutate(UB3=ceiling(D$QL_cleaned_11^-1*Consumption_11)) %>%
    mutate(UB4=ceiling(D$QL_cleaned_12^-1*Consumption_12)) %>% 
    mutate(UB5=ceiling(D$QL_cleaned_13^-1*Consumption_13)) %>% 
    mutate(UB6=ceiling(D$QL_cleaned_14^-1*Consumption_14)) %>%  
    mutate(UB7=ceiling(D$QL_cleaned_15^-1*Consumption_15))
  
  ub <- ub_1  %>% select(UB1,UB2,UB3,UB4,UB5,UB6,UB7)
 
  
  Creating_B <- Consume_1 %>% 
    mutate(B1=ceiling(D$QL_cleaned_9^-1*Consumption_9)) %>% 
    mutate(B2=ceiling(D$QL_cleaned_10^-1*Consumption_10)) %>% 
    mutate(B3=ceiling(0)) %>% 
    mutate(B4=ceiling(Consumption_12+Consumption_13)) %>% 
    mutate(B5=ceiling(D$QL_cleaned_13^-1*Consumption_13)) %>% 
    mutate(B6=ceiling(0)) %>% 
    mutate(B7=ceiling(D$QL_cleaned_15^-1*Consumption_15))

  
  # mutate(B1=Consumption_1+Consumption_2) %>% 
  
  
  B <- Creating_B  %>% select(B1,B2,B3,B4,B5,B6,B7)
  
  A <- matrix(c(1,0,0,0 ,0,0,0,
                0,1,0,0 ,0,0,0,
                0,-1,1,0 ,0,0,0,
                0,0,0,1 ,1,0,0,
                0,0,0,0 ,1,0,0,
                0,0,0,0 ,-1,1,0,
                0,0,0,0 ,0,0,1
                
  ), nrow=7, byrow=TRUE)
  
  lprec <- make.lp(7,7)
  lp.control(lprec,sense='max')
  #?make.lp
  set.objfn(lprec, as.numeric(C[1,]) )
  ColNames <- c("Com_1", "Com_2", "Com_3", "Com_4", "Com_5", "Com_6", "Com_7")
  #add.constraint(lprec, A[1,], "<=", as.numeric(B[1,1]))
  #add.constraint(lprec, A[2,], "<=", as.numeric(B[1,2]))
  add.constraint(lprec, A[3,], "<=", as.numeric(B[1,3]))
  add.constraint(lprec, A[4,], "=", as.numeric(B[1,4]))
  #add.constraint(lprec, A[5,], "<=", as.numeric(B[1,5]))
  #add.constraint(lprec, A[6,], "<=", as.numeric(B[1,6]))
  #add.constraint(lprec, A[7,], "<=", as.numeric(B[1,7]))
  set.bounds(lprec, lower = lb, columns = c(1,2,3,4,5,6,7))
  set.bounds(lprec, upper = ub, columns = c(1,2,3,4,5,6,7))
  #set.bounds(lprec, lower = .5*c(Consume_1$Consumption_1,Consume_1$Consumption_2,Consume_1$Consumption_3,Consume_1$Consumption_4), columns = c("Com_1", "Com_2", "Com_3", "Com_4"))
  #set.bounds(lprec, upper = 48.98, columns = 4)
  #RowNames <- c("THISROW", "THATROW", "LASTROW")
  #ColNames <- c("Com_1", "Com_2", "Com_3", "Com_4")
  
  #print(get.objective(lprec))
  #print(get.variables(lprec))
  # print(get.constraints(lprec))
  
  print(solve(lprec))
  
  sol<-get.variables(lprec)
  
  
  
  return(sol)
  
  
  # solve(lprec)
  # get.objective(lprec)
  # get.variables(lprec)
  # get.constraints(lprec)
}

indv_optimize_3(1)



##################################################################################################################33
##################################################################################################################33
##################################################################################################################33

#indv_optimize(1)

#create our output dataframe with no data

optimum_df_3 <- data.frame(matrix(ncol = 8, nrow = 0))


#This inserts into our dataframe
for (i in 1:1002){
  df_i <- data.frame(t(c(i,indv_optimize_3(i)))) # this transposes the data before inserting into the dataframe
  optimum_df_3<-rbind(optimum_df_3, df_i)
}

colnames(optimum_df_3) <- c("Indnum", "Com_9", "Com_10", "Com_11", "Com_12", "Com_13", "Com_14", "Com_15")
print(optimum_df_3)
#print(c(i,indv_optimize(i)))

#This is to see how many tests are failing
#optimum_df %>% filter(Com_1==0 & Com_2==0 & Com_3==0 & Com_4==0)

##################################################################################################################33
##################################################################################################################33
##################################################################################################################33
############################# Group 5 #############################################################################3


indv_optimize_5 <- function(person) {
  
  #person=1
  C <- Decision_DF %>% filter(Indnum == person) %>% select(
    QL_cleaned_17,QL_cleaned_18,QL_cleaned_19,QL_cleaned_20,QL_cleaned_21,QL_cleaned_22)
  
  D<- QL_Normalized  %>% filter(Indnum == person)
  
  Consume_1 <- Consume %>% filter(Indnum == person) %>% select(Consumption_17,Consumption_18,Consumption_19,Consumption_20,Consumption_21,Consumption_22)
  
  lb_1 <- Consume_1 %>% 
    mutate(LB1=ceiling(D$QL_cleaned_17*Consumption_17)) %>% 
    mutate(LB2=ceiling(D$QL_cleaned_18*Consumption_18)) %>% 
    mutate(LB3=ceiling(D$QL_cleaned_19*Consumption_19)) %>% 
    mutate(LB4=ceiling(D$QL_cleaned_20*Consumption_20)) %>% 
    mutate(LB5=ceiling(D$QL_cleaned_21*Consumption_21)) %>% 
    mutate(LB6=ceiling(D$QL_cleaned_22*Consumption_22)) 
  lb <- lb_1  %>% select(LB1,LB2,LB3,LB4,LB5,LB6)
  
  ub_1 <- Consume_1 %>% 
    mutate(UB1=ceiling(D$QL_cleaned_17^-1*Consumption_17)) %>% 
    mutate(UB2=ceiling(D$QL_cleaned_18^-1*Consumption_18)) %>% 
    mutate(UB3=ceiling(D$QL_cleaned_19^-1*Consumption_19)) %>% 
    mutate(UB4=ceiling(D$QL_cleaned_20^-1*Consumption_20)) %>% 
    mutate(UB5=ceiling(D$QL_cleaned_21^-1*Consumption_21)) %>% 
    mutate(UB6=ceiling(D$QL_cleaned_22^-1*Consumption_22)) 
  ub <- ub_1  %>% select(UB1,UB2,UB3,UB4,UB5,UB6)
  
  Creating_B <- Consume_1 %>% 
    mutate(B1=ceiling(Consumption_17+Consumption_18)) %>% 
    mutate(B2=ceiling(D$QL_cleaned_18^-1*Consumption_18)) %>% 
    mutate(B3=ceiling(Consumption_19+Consumption_20+Consumption_21+Consumption_22)) %>% 
    mutate(B4=ceiling(D$QL_cleaned_20^-1*Consumption_20)) %>% 
    mutate(B5=ceiling(D$QL_cleaned_21^-1*Consumption_21)) %>% 
    mutate(B6=ceiling(D$QL_cleaned_22^-1*Consumption_22)) 
  
  # mutate(B1=Consumption_1+Consumption_2) %>% 
  
  
  B <- Creating_B  %>% select(B1,B2,B3,B4,B5,B6)
  
  A <- matrix(c(1,1,0,0 ,0,0,
                0,1,0,0 ,0,0,
                0,0,1,1 ,1,1,
                0,0,0,1 ,0,0,
                0,0,0,0 ,1,0,
                0,0,0,0 ,0,1
  ), nrow=6, byrow=TRUE)
  
  lprec <- make.lp(6,6)
  lp.control(lprec,sense='max')
  #?make.lp
  set.objfn(lprec, as.numeric(C[1,]) )
  ColNames <- c("Com_1", "Com_2", "Com_3", "Com_4", "Com_5", "Com_6", "Com_7")
  #add.constraint(lprec, A[1,], "=", as.numeric(B[1,1]))
  #add.constraint(lprec, A[2,], "<=", as.numeric(B[1,2]))
  add.constraint(lprec, A[3,], "=", as.numeric(B[1,3]))
  #add.constraint(lprec, A[4,], "<=", as.numeric(B[1,4]))
  #add.constraint(lprec, A[5,], "<=", as.numeric(B[1,5]))
  #add.constraint(lprec, A[6,], "<=", as.numeric(B[1,6]))
  set.bounds(lprec, lower = lb, columns = c(1,2,3,4,5,6))
  set.bounds(lprec, upper = ub, columns = c(1,2,3,4,5,6))
  #set.bounds(lprec, lower = .5*c(Consume_1$Consumption_1,Consume_1$Consumption_2,Consume_1$Consumption_3,Consume_1$Consumption_4), columns = c("Com_1", "Com_2", "Com_3", "Com_4"))
  #set.bounds(lprec, upper = 48.98, columns = 4)
  #RowNames <- c("THISROW", "THATROW", "LASTROW")
  #ColNames <- c("Com_1", "Com_2", "Com_3", "Com_4")
  
  #print(get.objective(lprec))
  #print(get.variables(lprec))
  # print(get.constraints(lprec))
  
  print(solve(lprec))
  
  sol<-get.variables(lprec)
  
  
  
  return(sol)
  
  
  # solve(lprec)
  # get.objective(lprec)
  # get.variables(lprec)
  # get.constraints(lprec)
}

indv_optimize_5(1)



##################################################################################################################33
##################################################################################################################33
##################################################################################################################33

#indv_optimize(1)

#create our output dataframe with no data

optimum_df_5 <- data.frame(matrix(ncol = 7, nrow = 0))


#This inserts into our dataframe
for (i in 1:1002){
  df_i <- data.frame(t(c(i,indv_optimize_5(i)))) # this transposes the data before inserting into the dataframe
  optimum_df_5<-rbind(optimum_df_5, df_i)
}

colnames(optimum_df_5) <- c("Indnum", "Com_17", "Com_18", "Com_19", "Com_20", "Com_21", "Com_22")
print(optimum_df_5)
#print(c(i,indv_optimize(i)))

##################################################################################################################33
##################################################################################################################33
##################################################################################################################33
##############3######### Join All the results together #############################################################

optimum_df_4 <-Consume %>% select(Indnum, Com_16= Consumption_16)
optimum_df_6 <-Consume %>% select(Indnum, Com_23= Consumption_23, Com_24= Consumption_24, Com_25= Consumption_25, Com_26= Consumption_26, Com_27= Consumption_27)

optimum  <- left_join(optimum_df,optimum_df_2, by = c("Indnum" = "Indnum")) 
optimum  <- left_join(optimum,optimum_df_3, by = c("Indnum" = "Indnum")) 
optimum  <- left_join(optimum,optimum_df_4, by = c("Indnum" = "Indnum"))
optimum  <- left_join(optimum,optimum_df_5, by = c("Indnum" = "Indnum")) 
optimum  <- left_join(optimum,optimum_df_6, by = c("Indnum" = "Indnum")) 


Change <- optimum-Consume

optimum <- optimum%>%mutate(Com_1=round(Com_1))
optimum <- optimum%>%mutate(Com_2=round(Com_2))
optimum <- optimum%>%mutate(Com_3=ceiling(Com_3))
optimum <- optimum%>%mutate(Com_4=round(Com_4))

#Here is the optimal consumtion for each activity.
#View(optimum)

# Here is the change of the optimal consumption - the initial consumption
#View(Change)

##################################################################################################################33
##################################################################################################################33
##################################################################################################################33
##############3######### Finding the Relationships #################################################################

install.packages("fitdistrplus")

library("fitdistrplus")



plot(data=Consume, Consumption_1~Consumption_2)

Consume_plot <- Consume %>% 
  mutate(Diff_3_1 = Consumption_3-Consumption_1) %>% 
  mutate(Diff_1_2 = Consumption_1-Consumption_2) %>% 
  mutate(Diff_4_2 = Consumption_4-Consumption_2)%>% 
  mutate(Diff_6_5 = Consumption_6+Consumption_5+Consumption_7+Consumption_8)

Consume_plot_1 <- optimum %>% 
  mutate(Diff_3_1 = Com_3-Com_1) %>% 
  mutate(Diff_1_2 = Com_1-Com_2) %>% 
  mutate(Diff_4_2 = Com_4-Com_2)%>% 
  mutate(Diff_6_5 = Com_6+Com_5+Com_7+Com_8)

df_cnt <- count(Consume_plot, Diff_1_2)

df_cnt_1 <- count(Consume_plot_1, Diff_1_2)

plot(count(Consume_plot, Diff_1_2))


df_cnt <- df_cnt %>% mutate(n=n/1002) %>% mutate(Diff_1_2=Diff_1_2+9)

colnames(df_cnt) <-c("Diff_1_2","Cnt")

plot(df_cnt,col="red" )
plot(dnorm(df_cnt$Diff_1_2,mean(df_cnt$Diff_1_2),sqrt(var(df_cnt$Diff_1_2))),add=TRUE)


chisq.test(df_cnt$Diff_1_2,df_cnt$Cnt)


plot(df_cnt,  xlim=c(-10, 10), ylim=c(0, 120))
plot(df_cnt_1,  xlim=c(-10, 10), ylim=c(0, 120))
# This is exactly what I thought it should be



plot(count(Consume_plot, Diff_3_1))

plot(count(Consume_plot, Diff_4_2))

plot(count(Consume_plot, Diff_6_5))

plot(count(Consume_plot, Consumption_8))


##################################################################################################################33
##################################################################################################################33
##################################################################################################################33
##############3######### Export Results ############################################################################

write.csv(Consume, "consumption_initial.csv")

write.csv(optimum, "consumption_optimized_finalized.csv")
