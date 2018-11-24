#setwd("C:/Users/abanerj4/Documents/School Work/Fall 2018/DSA 6000/term-project")
setwd("/Users/anupambanerjee/Documents/Fall-2018-School/DSA6000/term-project")

library("readxl")
library("dplyr")
library("tidyr")
library("rlist")

data.raw <- read_excel("Data+campus_challenge_FINAL.xlsx")
data.CF <- read_excel("carbon-footprint.xlsx")

data.raw[is.na(data.raw)] <- 0
data.CF[is.na(data.CF)] <- 0

test <- data.raw %>% group_by(Group)
solar_water <-
  summarize(
    test,
    count = n(),
    solar_water_count = sum(solar_powered__water_heater)
  )
gas_water <-
  summarize(test,
            count = n(),
            gas_water_count = sum(gas_water_heater))
gas <- summarize(test, count = n(), gas_count = sum(gas))
natural_gas <-
  summarize(test,
            count = n(),
            natural_gas_count = sum(natural_gas))
hybrid <- summarize(test, count = n(), hybrid_count = sum(hybrid))
electric_water_peak <-
  summarize(
    test,
    count = n(),
    electric_water_peak_count = sum(electric_water_heater___peak_hou)
  )
electric_water_off <-
  summarize(
    test,
    count = n(),
    electric_water_off_count = sum(electric_water_heater___off_peak)
  )
electric_peak <-
  summarize(test,
            count = n(),
            electric_peak_count = sum(electric___peak_hours))
electric_off <-
  summarize(
    test,
    count = n(),
    electric_off_count = sum(electric___off_peak_hours)
  )
jetfuel <-
  summarize(test, count = n(), jetfuel_count = sum(jetfuel))


source <-
  Reduce(
    function(x, y)
      merge(x, y, all = TRUE),
    list(
      solar_water,
      gas_water,
      gas,
      natural_gas,
      hybrid,
      electric_water_peak,
      electric_water_off,
      electric_peak,
      electric_off,
      jetfuel
    )
  )

View(source)
View(data.raw)
View(data.CF)


