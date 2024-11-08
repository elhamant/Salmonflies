##This is trying to plot the length of time the leaves were available in the containers during consumption treatments, 7 days 
library(dplyr)
library(ggplot2)

Time=read.csv("C:\\Users\\emily\\OneDrive\\Documents\\Undergrad\\Undergrad year 4\\Thesis\\Analysis\\Leafavailabilitytime7.csv", header=T)

Time<-
  Time |>
  mutate(Only_Cottonwood = Days.with.Available.Cottonwood - Days.with.available.other)

ggplot(Time)+
  geom_point(aes(Days.with.Available.Cottonwood, Days.with.available.other, color=Treatment))

ggplot(Time)+
  geom_dotplot(aes(Only_Cottonwood, fill=Treatment))
