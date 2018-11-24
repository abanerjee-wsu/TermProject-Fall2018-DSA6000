# Below function will generate take input as individual consumption and carbon footprint lookup table as inputs.
setwd("/Users/anupambanerjee/Documents/Fall-2018-School/DSA6000/term-project")
library("readxl")

data.raw <-
  as.data.frame(read_excel("Data+campus_challenge_FINAL.xlsx"))
data.CF <- as.data.frame(read_excel("carbon-footprint.xlsx"))
#impute NA with 0
data.raw[is.na(data.raw)] <- 0
data.CF[is.na(data.CF)] <- 0
# reorder the position of CF file to match Individual file
data.CF <- data.CF[c(1, 2, 3, 4, 5, 6, 7, 8, 9, 12, 13, 14, 10, 11)]
View(data.CF)
View(data.raw)


#individual_CF_generator <- function(ind.num, data.raw, data.CF) {
# define ind.num for now, we will get it from calling function.
ind.num <- 1
important.group = c(1, 2, 3, 4, 5)

data.ind <- data.frame(matrix(ncol = 3, nrow = 0))

row.index.start <- ind.num * 27 - 27

data.ind <- data.raw[(row.index.start):(row.index.start+27),]

View(data.ind)

data.ind[["waste management"]] <- 0

distance.travelled = 0

for (i in row.index.start:row.index.start + 27) {
  activity <- data.ind[row.index.start, "Activity"]
  header <- "waste management"
  consumption <- data.ind[row.index.start, "Consumption"]
  #  multiplier <- 0
  #  footprint <- 0
  importance <-
    data.ind[row.index.start, "Quality_of_Life_Importance__1_10"]
  if (data.ind$Group == 6) {
    #retrieve waste management data.
    unitCF <-
      data.CF[which(data.CF$Activity == activity), "waste management"]
    change <- consumption *  var
    if (change != 0 & consumption > 0 & consumption > 0) {
      CF <- CF + change
    }
    
  } else {
    data.ind[7:10] <-
      data.ind[row.index.start:row.index.start, 7:10] * data.CF[which(data.CF$Activity == activity), 4:10] * consumption
    
  }
}

#}
