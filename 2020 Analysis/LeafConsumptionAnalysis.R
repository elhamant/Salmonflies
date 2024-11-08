Area=read.csv("C:\\Users\\Emily\\Documents\\College year 4\\Thesis\\Analysis\\Copy of clean.community.ecology.data.csv", header=T)
Area$diff=(Area$leaf.area.eaten)/Area$leaf.area.initial
Area$leaf.type=as.factor(Area$leaf.type)
Area7=Area[which(Area$conditioning.time=="7"),]
Area25=Area[ which(Area$conditioning.time=="25"),]


#####plotting######
library(ggplot2)
ggplot(Area7, aes(x=treatment, y=diff, fill=factor(leaf.type)))+
  geom_boxplot(position=position_dodge(1))+
  labs(x="Treatment", y="Proportion Consumed", fill="Leaf Type")+
  theme_bw()+
    theme(axis.text.x=element_text(angle=0))

ggplot(Area25, aes(x=treatment, y=diff, fill=factor(leaf.type)))+
  geom_boxplot(position=position_dodge(1))+
  labs(x="Treatment", y="Proportion Consumed", fill="Leaf Type")+
  theme_bw()+
  theme(axis.text.x=element_text(angle=0, hjust=1))

ggplot(Area, aes(x=treatment, y=diff, fill=factor(leaf.type)))+
  facet_wrap(~conditioning.time, ) +
  geom_boxplot(width=.7, position=position_dodge(width=.8))+
  labs(x="Treatment Group", y="Proportion Consumed", fill="Leaf Species")+
  theme_bw()+
  theme(legend.position="bottom")+
  scale_fill_manual(
    values=c("royalblue4", "grey", "light green", "violet", "light blue", "pink"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())


#####means of consumption#####
aggregate(Area[,6], list(Area$ï..Leaf), mean)


####testing for General differences of means####
table(Area1$leaf.type)
Area$leaf.type<-relevel(Area$leaf.type, ref="cottonwood")
reg3=lm(diff~leaf.type, data=Area)
summary(reg3)
windows()
par(mfrow=c(2,2))
plot(reg3)
tapply(X=Area$diff7, INDEX= Area$ï..Leaf, FUN=sd)
car::leveneTest(aov(diff7~ï..Leaf, data=Area))
TukeyHSD(reg3)
plot(TukeyHSD(reg3), las=2)
par(mar=c(4.2,9.5,3.8,2))


######Alder######

Alder7 <- Area[ which(Area$treatment=='a' & Area$conditioning.time=="7"), ]
Alder25<-Area[ which(Area$treatment=='a' & Area$conditioning.time=="25"), ]
t.test(Alder7$diff[Alder7$leaf.type=="alder"], Alder7$diff[Alder7$leaf.type=="cottonwood"], alternative="greater", var.equal=T)
t.test(Alder25$diff[Alder25$leaf.type=="alder"], Alder25$diff[Alder25$leaf.type=="cottonwood"], alternative="less", var.equal=T)


#######Chokecherry######
CC7<- Area[ which(Area$treatment=='c' & Area$conditioning.time=="7"), ]
CC25<-Area[ which(Area$treatment=='c' & Area$conditioning.time=="25"), ]
t.test(CC7$diff[CC7$leaf.type=="chokecherry"], CC7$diff[CC7$leaf.type=="cottonwood"], alternative="greater", var.equal=T)
t.test(CC25$diff[CC25$leaf.type=="chokecherry"], CC25$diff[CC25$leaf.type=="cottonwood"], alternative="less", var.equal=T)

######Dogwood######
D7<- Area[ which(Area$treatment=='b' & Area$conditioning.time=="7"), ]
D25<-Area[ which(Area$treatment=='b' & Area$conditioning.time=="25"), ]
t.test(D7$diff[D7$leaf.type=="dogwood"], D7$diff[D7$leaf.type=="cottonwood"], alternative="greater", var.equal=T)
t.test(D25$diff[D25$leaf.type=="dogwood"], D25$diff[D25$leaf.type=="cottonwood"], alternative="greater", var.equal=T)


#######Willow#####
W7<- Area[ which(Area$treatment=='d' & Area$conditioning.time=="7"), ]
W25<-Area[ which(Area$treatment=='d' & Area$conditioning.time=="25"), ]
t.test(W7$diff[W7$leaf.type=="willow"], W7$diff[W7$leaf.type=="cottonwood"], alternative="greater", var.equal=T)
t.test(W25$diff[W25$leaf.type=="willow"], W25$diff[W25$leaf.type=="cottonwood"], alternative="greater", var.equal=T)

#############cottonwood.stem###############
C7<- Area[ which(Area$treatment=='e' & Area$conditioning.time=="7"), ]
C25<-Area[ which(Area$treatment=='e' & Area$conditioning.time=="25"), ]
t.test(C7$diff[C7$leaf.type=="cottonwood.stem"], C7$diff[C7$leaf.type=="cottonwood"], alternative="greater", var.equal=T)
t.test(C25$diff[C25$leaf.type=="cottonwood.stem"], C25$diff[C25$leaf.type=="cottonwood"], alternative="less", var.equal=T)


#################################
#################################################
#########25 v 7 days of conditioning for each species########


######Alder######
Alder7 <- Area[ which(Area$treatment=='a' & Area$conditioning.time=="7"), ]
Alder25<-Area[ which(Area$treatment=='a' & Area$conditioning.time=="25"), ]
t.test(Alder7$diff[Alder7$leaf.type=="alder"], Alder25$diff[Alder25$leaf.type=="alder"], alternative="greater", var.equal=T)


#######Chokecherry######
CC7<- Area[ which(Area$treatment=='c' & Area$conditioning.time=="7"), ]
CC25<-Area[ which(Area$treatment=='c' & Area$conditioning.time=="25"), ]
t.test(CC7$diff[CC7$leaf.type=="chokecherry"], CC25$diff[CC25$leaf.type=="chokecherry"], alternative="greater", var.equal=F)

######Dogwood######
D7<- Area[ which(Area$treatment=='b' & Area$conditioning.time=="7"), ]
D25<-Area[ which(Area$treatment=='b' & Area$conditioning.time=="25"), ]
t.test(D7$diff[D7$leaf.type=="dogwood"], D25$diff[D25$leaf.type=="dogwood"], alternative="less", var.equal=T)


#######Willow#####
W7<- Area[ which(Area$treatment=='d' & Area$conditioning.time=="7"), ]
W25<-Area[ which(Area$treatment=='d' & Area$conditioning.time=="25"), ]
t.test(W7$diff[W7$leaf.type=="willow"], W25$diff[W25$leaf.type=="willow"], alternative="less", var.equal=T)

#############cottonwood.stem###############
C7<- Area[ which(Area$treatment=='e' & Area$conditioning.time=="7"), ]
C25<-Area[ which(Area$treatment=='e' & Area$conditioning.time=="25"), ]
t.test(C7$diff[C7$leaf.type=="cottonwood.stem"], C25$diff[C25$leaf.type=="cottonwood.stem"], alternative="greater", var.equal=T)


###cottonwood####
Cottonwood<-Area[which(Area$leaf.type=="cottonwood"),]
t.test(Cottonwood$diff[Cottonwood$conditioning.time=="7"], Cottonwood$diff[Cottonwood$conditioning.time=="25"], alternative="less" )
