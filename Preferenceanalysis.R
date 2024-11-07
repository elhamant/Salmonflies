###############################
###########################
####### 7 day preference ##########

preference=read.csv("C:\\Users\\Emily\\Documents\\College year 4\\Thesis\\7daypreference.csv", header=T)
str(preference)

####finding total Cottonwood######
preference$Cottonwood.Occurrence=as.numeric(paste(preference$Cottonwood.Occurrence))
sum(preference$Cottonwood.Occurrence, na.rm=T)

#####finding expected occurences####
preference$Expected=1/2*(preference$Total.Occurences)
sum(preference$Expected[preference$ï..Cup=="Willow"], na.rm=T)


#####finding Cottonwood expected occurences######
preference$Total.Occurences=(as.numeric(paste(preference$Total.Occurences)))
sum(preference$Total.Occurences, na.rm=T)
1/2*228

#####finding Observed values####
sum(preference$Total.Occurences[preference$ï..Cup=="Chokecherry"]-preference$Cottonwood.Occurrence[preference$ï..Cup=="Chokecherry"], na.rm=T)


#####calculating chi-sq#####
obs=c(84,21,6,7,170)
null.p=c(60/288, 27/288, 19.5/288, 37.5/288, 1/2)
sum(null.p)
chisq.test(obs, p=null.p)


##########alder########
obsa=c(84,36)
null.p.a=c(.5,.5)
chisq.test(obsa, p=null.p.a)

#######chokecherry######
obsc=c(6,33)
nullp.c=c(.5,.5)
chisq.test(obsc, p=null.p.a)

######Dogwood#####
obsd=c(21,33)
null.p.d=c(.5,.5)
chisq.test(obsd, p=null.p.d)

######Willow######
obsw=c(7,68)
null.p.w=c(.5,.5)
chisq.test(obsw, p=null.p.w)


##########################################
##########################################
############25 Day Preference#############
#########################################

pref25=read.csv("C:\\Users\\Emily\\Documents\\College year 4\\Thesis\\25daypreference.csv", header=T)
str(pref25)

####finding total Cottonwood######
pref25$Cottonwood.Occurrence=as.numeric(paste(pref25$Cottonwood.Occurrence))
sum(pref25$Cottonwood.Occurrence, na.rm=T)


#####finding expected occurrences####
pref25$Expected=1/2*(pref25$Total.Occurences)
sum(pref25$Expected[pref25$ï..Cup=="Chokecherry"], na.rm=T)

#####finding expected cottonwood occurrence####
sum(pref25$Total.Occurences)/2


####finding observed values####
sum(pref25$Total.Occurences[pref25$ï..Cup=="Willow"]-pref25$Cottonwood.Occurrence[pref25$ï..Cup=="Willow"], na.rm=T)


#####calculating chi-sq#####
obs25=c(64,50,18,6,332)
null.p1=c(6/47, 6/47, 6/47, 55/470, 1/2)
sum(null.p1)
chisq.test(obs, p=null.p1)


##########alder########
obsa=c(64, 56)
null.p.a=c(.5,.5)
chisq.test(obsa, p=null.p.a)

#######chokecherry######
obsc=c(18,102)
nullp.c=c(.5,.5)
chisq.test(obsc, p=null.p.a)

######Dogwood#####
obsd=c(70,50)
null.p.d=c(.5,.5)
chisq.test(obsd, p=null.p.d)

######Willow######
obsw=c(6,104)
null.p.w=c(.5,.5)
chisq.test(obsw, p=null.p.w)
