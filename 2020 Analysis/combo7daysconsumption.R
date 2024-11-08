###This is the combination of James' community ecology lab data and my data for 7 days of conditioning with the 0s taken out
library(ggplot2)

Area7=read.csv("C:\\Users\\emily\\OneDrive\\Documents\\Undergrad\\Undergrad year 4\\Thesis\\Analysis\\7dayconsumption_combo.csv", header=T)
Area7$leaf.area.eaten=replace(Area7$leaf.area.eaten, Area7$leaf.area.eaten==0, NA)
Area7$diff=(Area7$leaf.area.eaten)/Area7$leaf.area.initial
Area7$leaf.type=as.factor(Area7$leaf.type)


ggplot(Area7, aes(x=treatment, y=diff, fill=factor(leaf.type)))+
  geom_boxplot(position=position_dodge(1))+
  labs(x="Treatment", y="Proportion Consumed", fill="Leaf Type")+
  theme_bw()+
  theme(axis.text.x=element_text(angle=0))


fitted_models <- Area7 |> group_by(treatment) |> do(model = lm(diff~leaf.type, data = .))

fitted_models


###rename linear models
treatalm<-fitted_models[[2]][[1]]
treatblm<-fitted_models[[2]][[2]]
treatclm<-fitted_models[[2]][[3]]
treatdlm<-fitted_models[[2]][[4]]
treatelm<-fitted_models[[2]][[5]]

summary(treatalm)
summary(treatblm)
summary(treatclm)
summary(treatdlm)
summary(treatelm)
