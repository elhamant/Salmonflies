# Load Packages -----------------------------------------------------------

library(tidyverse)
library(dplyr)
library(car)
# install.packages("rstatix")
library(rstatix)
library(knitr)


# load data -------------------------------------------------------------


raw_data<-read_csv(
  "C:\\Users\\emily\\OneDrive\\Documents\\Undergrad\\Undergrad year 4\\Thesis\\Analysis\\Data\\25LeafConsumption.csv", 
  n_max=31) 

##didn't read the last row because willow was at 0, and I would have to get rid of that row anyways


# clean data, remove Nas, calculate prop eaten, prop eaten by Cottonwood --------


clean_LA <- raw_data |>
  select("treatment", "replicate", "leaf", "initial.area", "final.area" )|>
  rename("leaftype"="leaf", "initial_LA"="initial.area", "final_LA"="final.area")|>
  na.omit()|>
  convert_as_factor(leaftype, treatment)|>
  mutate(prop_eaten=(initial_LA-final_LA)/initial_LA)|>
  group_by(treatment, replicate)|>
  mutate(diffprop=prop_eaten-prop_eaten[leaftype=="Cottonwood"])

clean_LA|>
str()


# see how much of each group I have  --------------------------------------


clean_LA|>
  group_by(treatment)|>
  count()


# plotting ----------------------------------------------------------------


clean_LA|>
  ggplot(aes(treatment, prop_eaten, color=leaftype))+
  geom_boxplot()+
  labs(x="Treatment", y="Proportion of Leaf Consumed", color="Leaf Type")+
  theme_bw()
###saved above plot

clean_LA|>
  ggplot(aes(treatment, diffprop, color=leaftype))+
  geom_boxplot()+
  labs(x="Treatment", y="Difference of Proportion of Leaf Consumed compared to Cottonwood", 
       color="Leaf Type")+
  theme_bw()



# general differences in means  -------------------------------------------

#plot data 

clean_LA|>
  group_by(treatment, leaftype)|>
  summarize(count=n(), mean=mean(prop_eaten), sd=sd(prop_eaten))

##check sample size
table(clean_LA$treatment, clean_LA$leaftype)

##it is small, so check for normality
mod=aov(prop_eaten~treatment+leaftype, data=clean_LA)
qqPlot(mod$residuals)
shapiro.test(mod$residuals) ##residuals are not normal, p=.0436


##pairwise T test using Kruskal Wallis test

clean_LA |>
  group_by(treatment)|>
  wilcox_test(prop_eaten~leaftype, p.adjust.method="bonferroni", ref.group = "Cottonwood")|>
  kable()





