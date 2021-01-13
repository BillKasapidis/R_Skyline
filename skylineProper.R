#set current working directory
setwd("~/R scripts")

library(ggplot2)
source(file="functions.R")
start <- Sys.time()
ds <- read.csv(file = "test3d.csv")
myData <- sanitizeData(ds)

preferences <- c("low","low","high")

pareto1 <- calculatePareto(myData,preferences)

myData <- recalculate(myData,pareto1)


pareto2 <- calculatePareto(myData,preferences)

myData <- recalculate(myData,pareto2)

pareto3 <- calculatePareto(myData,preferences)

myData <- recalculate(myData,pareto3)


plot1 <- ggplot(ds, aes(x = price, y = distance)) +
  geom_point(data = pareto1, size = 3) +
  geom_point(data = pareto2, size = 3,aes(colour="blue"),show.legend = FALSE) +
  geom_point(data = pareto3, size = 3,aes(colour="green"),show.legend = FALSE) +
  geom_point(data = ds, size = 1) +
  geom_step(data=pareto1) +
  geom_step(data=pareto2,aes(colour="blue"),show.legend = FALSE) +
  geom_step(data=pareto3,aes(colour="green"),show.legend = FALSE)
  
plot(plot1)

end <- Sys.time()
print (end-start)