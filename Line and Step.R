#Line Chart
data <- c(18,21,29,54,12,30.22,6,8,30,44)

View(data)
library(tidyverse)
library(reshape2)
library(ggplot2)
head(data)

plot(data, type="l", main="Average Chicago Winter Temp", xlab="Days in Jan", 
     ylab="Temp", col="Blue", ylim=c(0,70))

data %>% ggplot(aes(x = report_year, y = robberies_percapita)) + geom_line()

#Step Chart
d=data.frame(x=c(1,2,4,5,7,8,9), y=c(1,2,3,5,6,7,9))
ggplot() +
  geom_step(data=d, mapping=aes(x=x, y=y)) +
  geom_step(data=d, mapping=aes(x=x, y=y), direction="vh", linetype=3) +
  geom_point(data=d, mapping=aes(x=x, y=y), color="red") 

d + labs(title = "Step_Chart",
  x = "hours", y = "miles")
