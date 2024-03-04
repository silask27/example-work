ema=read.csv("EMAwave1MRonly-011022.csv")
p3=read.csv("122121_M_SD_byBinLabel_4TaskorHalves_correctOnly_20trialmin.csv")

library("ggpubr")
library("glmmTMB")
library("ggplot2")
library("emmeans")
library("dplyr")
library("mgcv")
library("tidymv")
library("stats")
library("jtools")
library("stringr")

#select p3 data
p3.selected = p3[(p3$binlabel=='Alcohol' & p3$taskhalf=='combined'),]
names(ema)
names(p3)
#merge datasets
p3.selected$User = as.factor(p3.selected$subID)
length(levels(p3.selected$User))

ema$User = as.factor(ema$User)

dat = left_join(ema, p3.selected, by=c("User"))
table(dat$dem_1e)
dat$sex = as.factor(ifelse(dat$dem_1e==1, 'female',
                           ifelse(dat$dem_1e==0, 'male', NA)))
table(dat$sex)

contrasts(dat$sex)
contrasts(dat$sex)=contr.sum
contrasts(dat$sex)

View(dat$drinks)
str(dat)
names(dat)

table(dat$num_drinks)
#adjusting that 0 to .5
dat$num_drinks=ifelse(dat$num_drinks==0,0.5,dat$num_drinks)
table(dat$num_drinks)
#scaling is informative as is BUT we need to disaggregate the within from between person effects
#pull person mean across MRs as the between subject component

#create within-person deviation from person mean across MRs as the within subject component
dat$num_drinks_BTWN=NA
dat$num_drinks_WTN=NA
for (myperson in unique(dat$User)){
  dat[dat$User==myperson,"num_drinks_BTWN"]=mean(dat[dat$User==myperson,"num_drinks"],na.rm=T)
  dat[dat$User==myperson,"num_drinks_WTN"]=dat[dat$User==myperson,"num_drinks"]-dat[dat$User==myperson,"num_drinks_BTWN"]
}

dat$str_rating_BTWN=NA
dat$str_rating_WTN=NA
for (myperson in unique(dat$User)){
  dat[dat$User==myperson,"str_rating_BTWN"]=mean(dat[dat$User==myperson,"str_rating"],na.rm=T)
  dat[dat$User==myperson,"str_rating_WTN"]=dat[dat$User==myperson,"str_rating"]-dat[dat$User==myperson,"str_rating_BTWN"]
}

ASQdat <- read.csv("S1_scoredASQ_07122022.csv")
View(ASQdat)
str(dat)
View(dat)
colnames(ASQdat)[1] <- "User"
ASQdat$User <- as.factor(ASQdat$User)
ASQdat2 <- ASQdat[,c(1,3)]
ASQdatL <- ASQdat[,c(1,4)]
ASQdatH <- ASQdat[,c(1,5)]
View(ASQdat2)
length(unique(ASQdat2$User))
length(unique(dat$User))

asqmr = left_join(dat, ASQdat2, by=c("User"))
asqmrL <- left_join(dat, ASQdatL, by=c("User"))
asqmrH <- left_join(dat, ASQdatH, by=c("User"))
View(asqmr)

library(dplyr)
n_distinct(asqmr$User)


View(dat)
dat$BMI <- ((dat$weight_lbs.s1 * 703)/(dat$height_cm.s1/2.54)^2)
asqmr$BMI <- ((dat$weight_lbs.s1 * 703)/(dat$height_cm.s1/2.54)^2)
asqmrL$BMI <- ((dat$weight_lbs.s1 * 703)/(dat$height_cm.s1/2.54)^2)
asqmrH$BMI <- ((dat$weight_lbs.s1 * 703)/(dat$height_cm.s1/2.54)^2)
#HSS gaussian


HSSGLMM1=glmmTMB(HSS_score ~ 1 + (1|User), data=dat[dat$ln_drink==1,], ziformula=~0, family=gaussian(link="identity"))
HSSGLMM2=glmmTMB(HSS_score ~ str_rating_BTWN + str_rating_WTN + num_drinks_WTN + num_drinks_BTWN + str_rating_BTWN:num_drinks_WTN + str_rating_WTN:num_drinks_BTWN + str_rating_WTN:num_drinks_WTN + str_rating_BTWN:num_drinks_BTWN + (1|User), data=dat[dat$ln_drink==1,], ziformula=~0, family=gaussian(link="identity"))
HSSGLMM3 = glmmTMB(HSS_score ~ str_rating_BTWN + str_rating_WTN + (1|User), data=dat[dat$ln_drink==1,], ziformula=~0, family=gaussian(link="identity"))
HSSGLMM4=glmmTMB(HSS_score ~ sex + num_drinks + sex:num_drinks + (1|User), data=dat[dat$ln_drink==1,], ziformula=~0, family=gaussian(link="identity"))
HSSPersonal=glmmTMB(HSS_score ~ sex + age_s1 + BMI + (1|User), data=dat[dat$ln_drink==1,], ziformula=~0, family=gaussian(link="identity"))
HSSDrinking=glmmTMB(HSS_score ~ num_drinks_BTWN + num_drinks_WTN + (1|User), data=dat[dat$ln_drink==1,], ziformula=~0, family=gaussian(link="identity"))
HSSFeeling=glmmTMB(intent ~ HSS_score + (1|User), data=dat[dat$ln_drink==1,], ziformula=~0, family=gaussian(link="identity"))
HSSCircumstance=glmmTMB(HSS_score ~ DayofWeek + weekend + hourofday + (1|User), data=dat[dat$ln_drink==1,], ziformula=~0, family=gaussian(link="identity"))
HSSExperience = glmmTMB(HSS_score ~ worth_it + rate_it + lv_more + lv_feelbad + lv_guilt + (1|User), data=dat[dat$ln_drink==1,], ziformula=~0, family=gaussian(link="identity"))
HSSASQ <- glmmTMB(HSS_score ~ avgTotal.z + num_drinks_WTN + num_drinks_BTWN + avgTotal.z:num_drinks_WTN + avgTotal.z:num_drinks_BTWN + BMI + (1|User), data=asqmr[asqmr$ln_drink==1,], ziformula=~0, family=gaussian(link="identity"))
HSSASQL <- glmmTMB(HSS_score ~ avgL.z + num_drinks_WTN + num_drinks_BTWN + avgL.z:num_drinks_WTN + avgL.z:num_drinks_BTWN + BMI + (1|User), data=asqmrL[asqmrL$ln_drink==1,], ziformula=~0, family=gaussian(link="identity"))
HSSASQH <- glmmTMB(HSS_score ~ avgH.z + num_drinks_WTN + num_drinks_BTWN + avgH.z:num_drinks_WTN + avgH.z:num_drinks_BTWN + BMI + (1|User), data=asqmrH[asqmrH$ln_drink==1,], ziformula=~0, family=gaussian(link="identity"))
HSSASQ2 <- glmmTMB(HSS_score ~ avgTotal.z + num_drinks_WTN + num_drinks_BTWN + avgTotal.z:num_drinks_WTN + BMI + (1|User), data=asqmr[asqmr$ln_drink==1,], ziformula=~0, family=gaussian(link="identity"))
HSSASQL2 <- glmmTMB(HSS_score ~ avgL.z + num_drinks_WTN + num_drinks_BTWN + avgL.z:num_drinks_WTN + BMI + (1|User), data=asqmrL[asqmrL$ln_drink==1,], ziformula=~0, family=gaussian(link="identity"))
HSSASQH2 <- glmmTMB(HSS_score ~ avgH.z + num_drinks_WTN + num_drinks_BTWN + avgH.z:num_drinks_WTN + BMI + (1|User), data=asqmrH[asqmrH$ln_drink==1,], ziformula=~0, family=gaussian(link="identity"))
HSSASQ3 <- glmmTMB(HSS_score ~ avgTotal.z + num_drinks_WTN + num_drinks_BTWN + avgTotal.z:num_drinks_WTN + BMI + (1|User), data=asqmr[asqmr$ln_drink==1,], ziformula=~0, family=gaussian(link="identity"))
HSSASQL3 <- glmmTMB(HSS_score ~ avgL.z + num_drinks_WTN + num_drinks_BTWN + avgL.z:num_drinks_WTN + BMI + (1|User), data=asqmrL[asqmrL$ln_drink==1,], ziformula=~0, family=gaussian(link="identity"))
HSSASQH3 <- glmmTMB(HSS_score ~ avgH.z + num_drinks_WTN + num_drinks_BTWN + avgH.z:num_drinks_WTN + BMI + (1|User), data=asqmrH[asqmrH$ln_drink==1,], ziformula=~0, family=gaussian(link="identity"))
HSSASQ4 <- glmmTMB(HSS_score ~ avgTotal.z + num_drinks_WTN + num_drinks_BTWN + BMI + (1|User), data=asqmr[asqmr$ln_drink==1,], ziformula=~0, family=gaussian(link="identity"))
HSSASQL4 <- glmmTMB(HSS_score ~ avgL.z + num_drinks_WTN + num_drinks_BTWN + BMI + (1|User), data=asqmrL[asqmrL$ln_drink==1,], ziformula=~0, family=gaussian(link="identity"))
HSSASQH4 <- glmmTMB(HSS_score ~ avgH.z + num_drinks_WTN + num_drinks_BTWN + BMI + (1|User), data=asqmrH[asqmrH$ln_drink==1,], ziformula=~0, family=gaussian(link="identity"))
HSSASQ5 <- glmmTMB(HSS_score ~ avgTotal.z + num_drinks_WTN + BMI + (1|User), data=asqmr[asqmr$ln_drink==1,], ziformula=~0, family=gaussian(link="identity"))
HSSASQL5 <- glmmTMB(HSS_score ~ avgL.z + num_drinks_WTN + BMI + (1|User), data=asqmrL[asqmrL$ln_drink==1,], ziformula=~0, family=gaussian(link="identity"))
HSSASQH5 <- glmmTMB(HSS_score ~ avgH.z + num_drinks_WTN + BMI + (1|User), data=asqmrH[asqmrH$ln_drink==1,], ziformula=~0, family=gaussian(link="identity"))



summary(HSSGLMM1)
summary(HSSGLMM2)
summary(HSSGLMM3)
summary(HSSGLMM4)
summary(HSSPersonal)
summary(HSSDrinking)
summary(HSSFeeling)
summary(HSSCircumstance)
summary(HSSExperience)
summary(HSSASQ)
summary(HSSASQL)
summary(HSSASQH)
summary(HSSASQ2)
summary(HSSASQL2)
summary(HSSASQH2)
summary(HSSASQ3)
summary(HSSASQL3)
summary(HSSASQH3)
summary(HSSASQ4)
summary(HSSASQL4)
summary(HSSASQH4)
summary(HSSASQ5)
summary(HSSASQL5)
summary(HSSASQH5)


#HSS Gamma




summary(dat$HSS_score)
dat$HSS_score_adj=dat$HSS_score+.001
summary(dat$HSS_score_adj)

HSSGLMM4=glmmTMB(HSS_score_adj ~ 1 + (1|User), data=dat[dat$ln_drink==1,], ziformula=~0, family=Gamma(link="log"))
HSSGLMM5=glmmTMB(HSS_score_adj ~ str_rating_BTWN + str_rating_WTN + num_drinks_WTN + num_drinks_BTWN + str_rating_BTWN:num_drinks_WTN + str_rating_WTN:num_drinks_BTWN + str_rating_WTN:num_drinks_WTN + str_rating_BTWN:num_drinks_BTWN + (1|User), data=dat[dat$ln_drink==1,], ziformula=~0, family=Gamma(link="log"))
HSSGLMM6 = glmmTMB(HSS_score_adj ~ str_rating_BTWN + str_rating_WTN + (1|User), data=dat[dat$ln_drink==1,], ziformula=~0, family=Gamma(link="log"))
HSSGLMM7=glmmTMB(HSS_score_adj ~ sex + num_drinks + sex:num_drinks + (1|User), data=dat[dat$ln_drink==1,], ziformula=~0, family=Gamma(link="log"))
HSSGLMM8=glmmTMB(HSS_score_adj ~ sex + got_home + sex:got_home + (1|User), data=dat[dat$ln_drink==1,], ziformula=~0, family=Gamma(link="log"))
HSSPersonal2=glmmTMB(HSS_score_adj ~ sex + age_s1 + BMI + (1|User), data=dat[dat$ln_drink==1,], ziformula=~0, family=Gamma(link="log"))
HSSDrinking2=glmmTMB(HSS_score_adj ~ num_drinks_BTWN + num_drinks_WTN + (1|User), data=dat[dat$ln_drink==1,], ziformula=~0, family=Gamma(link="log"))
HSSFeeling2=glmmTMB(intent ~ HSS_score_adj + (1|User), data=dat[dat$ln_drink==1,], ziformula=~0, family=Gamma(link="log"))
HSSCircumstance2=glmmTMB(HSS_score_adj ~ DayofWeek + weekend + hourofday + (1|User), data=dat[dat$ln_drink==1,], ziformula=~0, family=Gamma(link="log"))
HSSExperience2 = glmmTMB(HSS_score_adj ~ worth_it + rate_it + lv_more + lv_feelbad + lv_guilt + (1|User), data=dat[dat$ln_drink==1,], ziformula=~0, family=Gamma(link="log"))
HSSASQ2 = glmmTMB(HSS_score_adj ~ avgTotal.z + num_drinks_WTN + num_drinks_BTWN + avgTotal.z:num_drinks_WTN + avgTotal.z:num_drinks_BTWN + BMI + (1|User), data=asqmr[asqmr$ln_drink==1,], ziformula=~0, family=Gamma(link="log"))
HSSASQL2 <- glmmTMB(HSS_score_adj ~ avgL.z + num_drinks_WTN + num_drinks_BTWN + avgL.z:num_drinks_WTN + avgL.z:num_drinks_BTWN + BMI + (1|User), data=asqmrL[asqmrL$ln_drink==1,], ziformula=~0, family=Gamma(link="log"))
HSSASQH2 <- glmmTMB(HSS_score_adj ~ avgH.z + num_drinks_WTN + num_drinks_BTWN + avgH.z:num_drinks_WTN + avgH.z:num_drinks_BTWN + BMI + (1|User), data=asqmrH[asqmrH$ln_drink==1,], ziformula=~0, family=Gamma(link="log"))



summary(HSSGLMM4)
summary(HSSGLMM5)
summary(HSSGLMM6)
summary(HSSGLMM7)
summary(HSSGLMM8)
summary(HSSPersonal2)
summary(HSSDrinking2)
summary(HSSFeeling2)
summary(HSSCircumstance2)
summary(HSSExperience2)
summary(HSSASQ2)
summary(HSSASQL2)
summary(HSSASQH2)


#Any Consequences

ACGLMM1=glmmTMB(AnyConsequence ~ 1 + (1|User), data=dat[dat$ln_drink==1,], ziformula=~0, family=binomial(link="logit"))
ACGLMM2=glmmTMB(AnyConsequence ~ sex + num_drinks + sex:num_drinks + (1|User), data=dat[dat$ln_drink==1,], ziformula=~0, family=binomial(link="logit"))
ACGLMM3=glmmTMB(AnyConsequence ~ str_rating_BTWN + str_rating_WTN + num_drinks_WTN + num_drinks_BTWN + str_rating_BTWN:num_drinks_WTN + str_rating_WTN:num_drinks_BTWN + str_rating_WTN:num_drinks_WTN + str_rating_BTWN:num_drinks_BTWN + (1|User), 
data=dat[dat$ln_drink==1,], ziformula=~0, family=binomial(link="logit"))
ACGLMM4=glmmTMB(AnyConsequence ~ str_rating_BTWN + str_rating_WTN  + (1|User), data=dat[dat$ln_drink==1,], ziformula=~0, family=binomial(link="logit"))
ACPersonal=glmmTMB(AnyConsequence ~ sex + age_s1 + BMI + (1|User), data=dat[dat$ln_drink==1,], ziformula=~0, family=binomial(link="logit"))
ACDrinking=glmmTMB(AnyConsequence ~ num_drinks_BTWN + num_drinks_WTN + (1|User), data=dat[dat$ln_drink==1,], ziformula=~0, family=binomial(link="logit"))
ACFeeling=glmmTMB(intent ~ AnyConsequence + (1|User), data=dat[dat$ln_drink==1,], ziformula=~0, family=gaussian(link="identity"))
ACCircumstance=glmmTMB(AnyConsequence ~ DayofWeek + weekend + hourofday + (1|User), data=dat[dat$ln_drink==1,], ziformula=~0, family=binomial(link="logit"))
ACExperience = glmmTMB(AnyConsequence ~ worth_it + rate_it + lv_more + lv_feelbad + lv_guilt + (1|User), data=dat[dat$ln_drink==1,], ziformula=~0, family=binomial(link="logit"))
ACASQ = glmmTMB(AnyConsequence ~ avgTotal.z + num_drinks_WTN + num_drinks_BTWN + avgTotal.z:num_drinks_WTN + avgTotal.z:num_drinks_BTWN + BMI + (1|User), data=asqmr[asqmr$ln_drink==1,], ziformula=~0, family=binomial(link="logit"))
ACASQL <- glmmTMB(AnyConsequence ~ avgL.z + num_drinks_WTN + num_drinks_BTWN + avgL.z:num_drinks_WTN + avgL.z:num_drinks_BTWN + BMI + (1|User), data=asqmrL[asqmrL$ln_drink==1,], ziformula=~0, family=binomial(link="logit"))
ACASQH <- glmmTMB(AnyConsequence ~ avgH.z + num_drinks_WTN + num_drinks_BTWN + avgH.z:num_drinks_WTN + avgH.z:num_drinks_BTWN + BMI + (1|User), data=asqmrH[asqmrH$ln_drink==1,], ziformula=~0, family=binomial(link="logit"))
ACASQ2 = glmmTMB(AnyConsequence ~ avgTotal.z + num_drinks_WTN + num_drinks_BTWN + avgTotal.z:num_drinks_WTN + BMI + (1|User), data=asqmr[asqmr$ln_drink==1,], ziformula=~0, family=binomial(link="logit"))
ACASQL2 <- glmmTMB(AnyConsequence ~ avgL.z + num_drinks_WTN + num_drinks_BTWN + avgL.z:num_drinks_WTN + BMI + (1|User), data=asqmrL[asqmrL$ln_drink==1,], ziformula=~0, family=binomial(link="logit"))
ACASQH2 <- glmmTMB(AnyConsequence ~ avgH.z + num_drinks_WTN + num_drinks_BTWN + avgH.z:num_drinks_WTN + BMI + (1|User), data=asqmrH[asqmrH$ln_drink==1,], ziformula=~0, family=binomial(link="logit"))
ACASQ3 = glmmTMB(AnyConsequence ~ avgTotal.z + num_drinks_WTN + num_drinks_BTWN + BMI + (1|User), data=asqmr[asqmr$ln_drink==1,], ziformula=~0, family=binomial(link="logit"))
ACASQL3 <- glmmTMB(AnyConsequence ~ avgL.z + num_drinks_WTN + num_drinks_BTWN + BMI + (1|User), data=asqmrL[asqmrL$ln_drink==1,], ziformula=~0, family=binomial(link="logit"))
ACASQH3 <- glmmTMB(AnyConsequence ~ avgH.z + num_drinks_WTN + num_drinks_BTWN + BMI + (1|User), data=asqmrH[asqmrH$ln_drink==1,], ziformula=~0, family=binomial(link="logit"))
ACASQ4 = glmmTMB(AnyConsequence ~ avgTotal.z + num_drinks_WTN + num_drinks_BTWN + avgTotal.z:num_drinks_WTN + avgTotal.z:num_drinks_BTWN + BMI + (1|User), data=asqmr[asqmr$ln_drink==1,], ziformula=~0, family=binomial(link="logit"))
ACASQL4 <- glmmTMB(AnyConsequence ~ avgL.z + num_drinks_WTN + num_drinks_BTWN + avgL.z:num_drinks_WTN + avgL.z:num_drinks_BTWN + BMI + (1|User), data=asqmrL[asqmrL$ln_drink==1,], ziformula=~0, family=binomial(link="logit"))
ACASQH4 <- glmmTMB(AnyConsequence ~ avgH.z + num_drinks_WTN + num_drinks_BTWN + avgH.z:num_drinks_WTN + avgH.z:num_drinks_BTWN + BMI + (1|User), data=asqmrH[asqmrH$ln_drink==1,], ziformula=~0, family=binomial(link="logit"))

summary(ACGLMM1)
summary(ACGLMM2)
summary(ACGLMM3)
summary(ACGLMM4)
summary(ACPersonal)
summary(ACDrinking)
summary(ACFeeling)
summary(ACCircumstance)
summary(ACExperience)
summary(ACASQ)
summary(ACASQL)
summary(ACASQH)
summary(ACASQ2)
summary(ACASQL2)
summary(ACASQH2)
summary(ACASQ3)
summary(ACASQL3)
summary(ACASQH3)

#Consequence Count


CCGLMM1=glmmTMB(ConsequenceCount ~ 1 + (1|User), data=dat[dat$ln_drink==1,], ziformula=~0, family=poisson(link="log"))
CCGLMM2=glmmTMB(ConsequenceCount ~ sex + num_drinks + sex:num_drinks + (1|User), data=dat[dat$ln_drink==1,], ziformula=~0, family=poisson(link="log"))
CCGLMM3=glmmTMB(ConsequenceCount ~ str_rating_BTWN + str_rating_WTN + num_drinks_WTN + num_drinks_BTWN + str_rating_BTWN:num_drinks_WTN + str_rating_WTN:num_drinks_BTWN + str_rating_WTN:num_drinks_WTN + str_rating_BTWN:num_drinks_BTWN + (1|User), 
                data=dat[dat$ln_drink==1,], ziformula=~0, family=poisson(link="log"))
CCGLMM4=glmmTMB(ConsequenceCount ~ str_rating_BTWN + str_rating_WTN  + (1|User), data=dat[dat$ln_drink==1,], ziformula=~0, family=poisson(link="log"))
CCPersonal=glmmTMB(ConsequenceCount ~ sex + age_s1 + BMI + (1|User), data=dat[dat$ln_drink==1,], ziformula=~0, family=poisson(link="log"))
CCDrinking=glmmTMB(ConsequenceCount ~ num_drinks_BTWN + num_drinks_WTN + (1|User), data=dat[dat$ln_drink==1,], ziformula=~0, family=poisson(link="log"))
CCFeeling=glmmTMB(intent ~ ConsequenceCount + (1|User), data=dat[dat$ln_drink==1,], ziformula=~0, family=poisson(link="log"))
CCCircumstance=glmmTMB(ConsequenceCount ~ DayofWeek + weekend + hourofday + (1|User), data=dat[dat$ln_drink==1,], ziformula=~0, family=poisson(link="log"))
CCExperience = glmmTMB(ConsequenceCount ~ worth_it + rate_it + lv_more + lv_feelbad + lv_guilt + (1|User), data=dat[dat$ln_drink==1,], ziformula=~0, family=poisson(link="log"))
CCASQ <- glmmTMB(ConsequenceCount ~ avgTotal.z + num_drinks_WTN + num_drinks_BTWN + avgTotal.z:num_drinks_WTN + avgTotal.z:num_drinks_BTWN + BMI + (1|User), data=asqmr[asqmr$ln_drink==1,], ziformula=~0, family=poisson(link="log"))
CCASQL <- glmmTMB(ConsequenceCount ~ avgL.z + num_drinks_WTN + num_drinks_BTWN + avgL.z:num_drinks_WTN + avgL.z:num_drinks_BTWN + BMI + (1|User), data=asqmrL[asqmrL$ln_drink==1,], ziformula=~0, family=poisson(link="log"))
CCASQH <- glmmTMB(ConsequenceCount ~ avgH.z + num_drinks_WTN + num_drinks_BTWN + avgH.z:num_drinks_WTN + avgH.z:num_drinks_BTWN + BMI + (1|User), data=asqmrH[asqmrH$ln_drink==1,], ziformula=~0, family=poisson(link="log"))
CCASQ2 <- glmmTMB(ConsequenceCount ~ avgTotal.z + num_drinks_BTWN + num_drinks_WTN + BMI + (1|User), data=asqmr[asqmr$ln_drink==1,], ziformula=~0, family=poisson(link="log"))
CCASQL2 <- glmmTMB(ConsequenceCount ~ avgL.z + num_drinks_BTWN + num_drinks_WTN + avgL.z:num_drinks_WTN + BMI + (1|User), data=asqmrL[asqmrL$ln_drink==1,], ziformula=~0, family=poisson(link="log"))
CCASQH2 <- glmmTMB(ConsequenceCount ~ avgH.z + num_drinks_BTWN + num_drinks_WTN + avgH.z:num_drinks_WTN + BMI + (1|User), data=asqmrH[asqmrH$ln_drink==1,], ziformula=~0, family=poisson(link="log"))
CCASQ3 <- glmmTMB(ConsequenceCount ~ avgTotal.z + num_drinks_WTN + avgTotal.z:num_drinks_WTN + BMI + (1|User), data=asqmr[asqmr$ln_drink==1,], ziformula=~0, family=poisson(link="log"))
CCASQL3 <- glmmTMB(ConsequenceCount ~ avgL.z + num_drinks_WTN + avgL.z:num_drinks_WTN + BMI + (1|User), data=asqmrL[asqmrL$ln_drink==1,], ziformula=~0, family=poisson(link="log"))
CCASQH3 <- glmmTMB(ConsequenceCount ~ avgH.z + num_drinks_WTN + avgH.z:num_drinks_WTN + BMI + (1|User), data=asqmrH[asqmrH$ln_drink==1,], ziformula=~0, family=poisson(link="log"))
CCASQ4 <- glmmTMB(ConsequenceCount ~ avgTotal.z + num_drinks_WTN + BMI + (1|User), data=asqmr[asqmr$ln_drink==1,], ziformula=~0, family=poisson(link="log"))
CCASQL4 <- glmmTMB(ConsequenceCount ~ avgL.z + num_drinks_WTN + BMI + (1|User), data=asqmrL[asqmrL$ln_drink==1,], ziformula=~0, family=poisson(link="log"))
CCASQH4 <- glmmTMB(ConsequenceCount ~ avgH.z + num_drinks_WTN + BMI + (1|User), data=asqmrH[asqmrH$ln_drink==1,], ziformula=~0, family=poisson(link="log"))
CCASQ5 <- glmmTMB(ConsequenceCount ~ avgTotal.z + num_drinks_BTWN + BMI + (1|User), data=asqmr[asqmr$ln_drink==1,], ziformula=~0, family=poisson(link="log"))
CCASQL5 <- glmmTMB(ConsequenceCount ~ avgL.z + num_drinks_BTWN + BMI + (1|User), data=asqmrL[asqmrL$ln_drink==1,], ziformula=~0, family=poisson(link="log"))
CCASQH5 <- glmmTMB(ConsequenceCount ~ avgH.z + num_drinks_BTWN + BMI + (1|User), data=asqmrH[asqmrH$ln_drink==1,], ziformula=~0, family=poisson(link="log"))


summary(CCGLMM1)
summary(CCGLMM2)
summary(CCGLMM3)
summary(CCGLMM4)
summary(CCPersonal)
summary(CCDrinking)
summary(CCFeeling)
summary(CCCircumstance)
summary(CCExperience)
summary(CCASQ)
summary(CCASQL)
summary(CCASQH)
summary(CCASQ2)
summary(CCASQL2)
summary(CCASQH2)
summary(CCASQ3)
summary(CCASQL3)
summary(CCASQH3)
summary(CCASQ4)
summary(CCASQL4)
summary(CCASQH4)
summary(CCASQ5)
summary(CCASQL5)
summary(CCASQH5)


plot(asqmr$avgTotal.z, asqmr$ConsequenceCount)
plot(asqmrL$avgL.z, asqmrL$ConsequenceCount)
plot(asqmrH$avgH.z, asqmrH$ConsequenceCount)

cor(asqmr$avgTotal.z, asqmr$num_drinks_BTWN, use = "complete.obs")
cor(asqmr$avgTotal.z, asqmr$num_drinks_WTN, use = "complete.obs")
cor(asqmr$num_drinks_BTWN, asqmr$num_drinks_WTN, use = "complete.obs")

#Number of Drinks
NDGLMM1=glmmTMB(num_drinks ~ 1 + (1|User), data=dat[dat$ln_drink==1,], ziformula=~0, family=poisson(link="log"))
NDGLMM2=glmmTMB(num_drinks ~ str_rating_BTWN + str_rating_WTN  + (1|User), data=dat[dat$ln_drink==1,], ziformula=~0, family=poisson(link="log"))
NDPersonal=glmmTMB(num_drinks ~ sex + age_s1 + BMI + (1|User), data=dat[dat$ln_drink==1,], ziformula=~0, family=poisson(link="log"))
NDFeeling=glmmTMB(intent ~ num_drinks + (1|User), data=dat[dat$ln_drink==1,], ziformula=~0, family=poisson(link="log"))
NDCircumstance=glmmTMB(num_drinks ~ DayofWeek + weekend + hourofday + (1|User), data=dat[dat$ln_drink==1,], ziformula=~0, family=poisson(link="log"))
NDExperience = glmmTMB(num_drinks ~ worth_it + rate_it + lv_more + lv_feelbad + lv_guilt + (1|User), data=dat[dat$ln_drink==1,], ziformula=~0, family=poisson(link="log"))
NDASQ <- glmmTMB(num_drinks ~ avgTotal.z + BMI + (1|User), data=asqmr[asqmr$ln_drink==1,], ziformula=~0, family=poisson(link="log"))
NDASQL <- glmmTMB(num_drinks ~ avgL.z  + BMI + (1|User), data=asqmrL[asqmrL$ln_drink==1,], ziformula=~0, family=poisson(link="log"))
NDASQH <- glmmTMB(num_drinks ~ avgH.z + BMI + (1|User), data=asqmrH[asqmrH$ln_drink==1,], ziformula=~0, family=poisson(link="log"))

summary(NDGLMM1)
summary(NDGLMM2)
summary(NDPersonal)
summary(NDFeeling)
summary(NDCircumstance)
summary(NDExperience)
summary(NDASQ)
summary(NDASQL)
summary(NDASQH)


#LN Drink
LNGLMM1=glmmTMB(ln_drink ~ 1 + (1|User), data=dat, ziformula=~0, family=binomial(link="logit"))
LNGLMM2=glmmTMB(ln_drink ~ str_rating_BTWN + str_rating_WTN  + (1|User), data=dat, ziformula=~0, family=binomial(link="logit"))
LNPersonal=glmmTMB(ln_drink ~ sex + age_s1 + BMI + (1|User), data=dat, ziformula=~0, family=binomial(link="logit"))
LNFeeling=glmmTMB(intent ~ ln_drink + (1|User), data=dat, ziformula=~0, family=gaussian(link="identity"))
LNCircumstance=glmmTMB(ln_drink ~ DayofWeek + weekend + hourofday + (1|User), data=dat, ziformula=~0, family=binomial(link="logit"))
LNASQ = glmmTMB(ln_drink ~ avgTotal.z + (1|User), data=asqmr, ziformula=~0, family=binomial(link="logit"))
LNASQL <- glmmTMB(ln_drink ~ avgL.z + (1|User), data=asqmrL, ziformula=~0, family=binomial(link="logit"))
LNASQH <- glmmTMB(ln_drink ~ avgH.z + (1|User), data=asqmrH, ziformula=~0, family=binomial(link="logit"))

summary(LNGLMM1)
summary(LNGLMM2)
summary(LNPersonal)
summary(LNFeeling)
summary(LNCircumstance)
summary(LNASQ)
summary(LNASQL)
summary(LNASQH)