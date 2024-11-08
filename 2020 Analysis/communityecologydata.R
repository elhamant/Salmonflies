library(tidyverse)
library(dplyr)
library(car)
install.packages("rstatix")
library(rstatix)
library(knitr)

##load data

raw_data<-read_csv("C:\\Users\\emily\\OneDrive\\Documents\\Undergrad\\Undergrad year 4\\Thesis\\Analysis\\Data\\communityecologydata_2022.csv", n_max=84)


## clean data, remove Nas, calculate proportion eaten, proportion eaten compared to Cottonwood
clean_LA <- raw_data |>
  select("treatment", "replicate", "leaf type", "leaf area initial", "leaf area final" )|>
  rename("leaftype"="leaf type", "initial_LA"="leaf area initial", "final_LA"="leaf area final")|>
  na.omit()|>
  convert_as_factor(leaftype, treatment)|>
  mutate(prop_eaten=(initial_LA-final_LA)/initial_LA)|>
  group_by(treatment, replicate)|>
  mutate(diffprop=prop_eaten-prop_eaten[leaftype=="Cottonwood"])

clean_LA|>
str()

##see how much of each group I have 

clean_LA|>
  group_by(treatment)|>
  count()

### plotting 

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


## general differences in means 

#plot data 

clean_LA|>
  group_by(treatment, leaftype)|>
  summarize(count=n(), mean=mean(prop_eaten), sd=sd(prop_eaten))

##check sample size
table(clean_LA$treatment, clean_LA$leaftype)

##it is small, so check for normality
mod=aov(prop_eaten~treatment+leaftype, data=clean_LA)
qqPlot(mod$residuals)
shapiro.test(mod$residuals) ##residuals are normal, p=.31


### print model results 
summary(mod)

TukeyHSD(mod, conf.level = .95)



## Pairwise T tests using bonferroni correction methods 

pairwiseT <-clean_LA |>
  group_by(treatment)|>
  pairwise_t_test(prop_eaten~leaftype, 
                  paired=TRUE, p.adjust.method = "bonferroni")

pairwiseT|>
  kable()


