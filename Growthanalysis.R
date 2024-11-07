growth=read.csv("C:\\Users\\Emily\\Documents\\College year 4\\Thesis\\Analysis\\7daygrowth.csv", header=T)

###remaking columns and deleting old####
growth$species=growth$ï..Species
growth$initial=growth$Initial.Weight..g.
growth$final=growth$X14.day.weight
growth$difference=growth$X14.day.weight-growth$Initial.Weight..g.
growth=growth[,-c(1:3)]
growth$relative=(growth$difference)/growth$initial

#####means of species#####
aggregate(growth[,4], list(growth$species), mean)

####testing for differences of means####
reg1=lm(difference~species, data=growth)
summary(reg1)
windows()
par(mfrow=c(2,2))
plot(reg1)
tapply(X=growth$difference, INDEX= growth$species, FUN=sd)
car::leveneTest(aov(difference ~ species, data=growth))
plot(TukeyHSD(reg1), las=1)



####graphing differences by species#####
library(ggplot2)
ggplot(data=growth, aes(x=species, y=difference, group=species))+
  geom_boxplot()+
  labs(x="Leaf Species", y="Growth of Stoneflies over 14 days (g)")+
  theme_bw()
  
  


##### 7 day growth v initial mass####

library(ggplot2)
ggplot(data=growth, aes(x=initial, y=difference, col=species))+
  geom_point()+
  labs(x="Initial Weight of Stonefly (g)", y="Growth (g)")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  

####### 7 day growth - relative #####
library(ggplot2)
ggplot(data=growth, aes(x=initial, y=relative, col=species))+
  geom_point()+
  labs(x="Initial Weight of Stonefly (g)", y="Relative Growth (g)")+
  theme_bw()




######fitted growth model - difference #######

growthlm=lm(difference~initial*species, data=growth)
summary(growthlm)
anova(growthlm)

#######fitted growth model - relative #####
growthlm=lm(relative~initial*species, data=growth)
summary(growthlm)
anova(growthlm)

#######testing growth on subsets of IW. if IW. than .37, "small"#####

library(dplyr)
growth=growth %>% mutate(size = case_when(
  initial > .37 ~ "large",
  initial <.37  ~ "small"))

######modelling with size####

sizelm7=lm(difference~size, data=growth)
summary(sizelm7) 
anova(sizelm7)

###################################
######################################
####### 25 DAY GROWTH ##############

growth25=read.csv("C:\\Users\\Emily\\Documents\\College year 4\\Thesis\\Analysis\\25daygrowth.csv")


###remakedata####
growth25$species=growth25$ï..Species
growth25$initial=growth25$Initial.Weight..g.
growth25$final=growth25$X14.day.weight
growth25$difference=growth25$final-growth25$initial
growth25=growth25[,-c(1:3)]
growth25$relative=growth$difference/growth$initial

####graphing boxplot differences by species#####
ggplot(data=growth25, aes(x=species, y=difference, group=species))+
  geom_boxplot()+
  labs(x="Leaf Species", y="Growth of Stoneflies over 14 days (g)")+
  theme_bw()
  
##### 25 day growth v initial mass####

library(ggplot2)
ggplot(data=growth25, aes(x=initial, y=difference, col=species))+
  geom_point()+
  labs(x="Initial Weight of Stonefly (g)", y="Growth (g)")+
  theme_bw()+
  

##### 25 day growth v relative mass####

library(ggplot2)
ggplot(data=growth25, aes(x=initial, y=relative, col=species))+
  geom_point()+
  labs(x="Initial Weight of Stonefly (g)", y="Relative Growth (g)")+
  theme_bw()

######fitted growth model - difference #######

growth25lm=lm(difference~initial*species, data=growth25)
summary(growth25lm)
anova(growth25lm)

#######fitted growth model - relative #####
growth25rlm=lm(relative~initial*species, data=growth25)
summary(growth25rlm)
anova(growth25rlm)


#####means of species#####
aggregate(growth25[,4], list(growth25$species), mean)


####testing for differences of means####
reg2=lm(difference~species, data=growth25)
anova(reg2)
windows()
par(mfrow=c(2,2))
plot(reg2)
tapply(X=growth25$difference, INDEX= growth25$species, FUN=sd)
car::leveneTest(reg2)
plot(TukeyHSD(aov(difference~species, data=growth25)), las=1)


library(dplyr)
growth25=growth25 %>% mutate(size = case_when(
  initial > .37 ~ "large",
  initial <.37  ~ "small"))

######modelling with size####

sizelm25=lm(difference~size, data=growth25)
summary(sizelm25) 
anova(sizelm25)

###########################################
#############################################
###############################################
###########Test for difference between conditioning period###########

growthtotal=cbind(growth, growth25$difference)
growthtotal$DIFFDIFF=abs(growthtotal$`growth25$difference`)-abs(growthtotal$difference)



ggplot(data=growthtotal, aes(x=species, y=DIFFDIFF, group=species))+
  geom_boxplot()+
  labs(y="Difference")


GTlm=aov(DIFFDIFF~species, data=growthtotal)
anova(GTlm)
plot(GTlm)
car::leveneTest(GTlm)
plot(TukeyHSD(GTlm))

##########################################
####25 day growth v initial mass####


library(ggplot2)
ggplot(data=growth25, aes(x=initial, y=difference, col=species))+
  geom_point()+
  labs(x="Initial Weight (g)", y="Growth (g)")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


#####fitted model of above######
growthlm25=lm(difference~initial, data=growth25)
summary(growthlm25)
anova(growthlm25)
confint(growthlm25)


#########################################
##################All Growth#################

all=read.csv("C:\\Users\\Emily\\Documents\\College year 4\\Thesis\\Analysis\\Allgrowth.csv", header=T)
all$relative=all$Difference/all$Initial
library(ggplot2)
ggplot(all, aes(x=Initial, y=Difference, col=Conditioning))+
  geom_point()+
  labs(x="Initial Weight of Stonefly (g)", y="Growth (g)")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


alllm=lm(relative~Initial*Conditioning, data=all)
summary(alllm)
anova(alllm)

alllmleaf=lm(relative~Initial*Conditioning*ï..Species, data=all)
summary(alllmleaf)
anova(alllmleaf)

leafcheck=aov(Difference~Conditioning*ï..Species, data=all)
anova(leafcheck)
TukeyHSD(leafcheck)
plot(TukeyHSD(leafcheck))


###############sidebyside boxplots 7 and 25####

ggplot(data=all, aes(x=ï..Species, y=Difference))+
  facet_wrap(~Conditioning, scale="free")+
  geom_boxplot()+
  ylim(-.1, .075)+
  labs(x="Leaf Species", y="Growth of Stoneflies over 14 days (g)")+
  theme_bw()

