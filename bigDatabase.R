#set current working directory
setwd("~/R scripts")
library(ggplot2)
library(readODS)
library(dplyr)
source(file="functions.R")

start <- Sys.time()

ds <- read_ods("data.ods") 
myData <- ds %>%
  select("ACADEMIC","STUDENT")

uni <- ds %>%
  select("UNIVERSITY") %>%
  mutate(skyline = 0)

prefs <- c("high","high")

i <- 1
while(nrow(myData > 0)){
  pareto <- calculatePareto(myData,prefs)
  
  #get dataframe index number and place i in uni list
  indexes <- data.frame(as.integer(rownames(pareto)))
  for (s in 1:nrow(indexes)){
    a <- indexes[s,1]
    uni[a,2] <- i
  }
  
  myData <- recalculate(myData, pareto)
  i <- i+1
}
end <- Sys.time()

cat("Proccess finished in ",end-start,"\n")
cat (i-1 , " Skylines have been extracted.\n")
print(uni)
