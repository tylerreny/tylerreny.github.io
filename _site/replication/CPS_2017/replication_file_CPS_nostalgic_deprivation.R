########
# This file reproduces the figures and tables 
# in Roots of the Radical Right: Nostalgic Deprivation
# in the US and Britain by 
# Justin Gest, Tyler Reny, and Jeremy Mayer
# The survey questions are in the article appendix
# Please contact Tyler Reny with any questions
# ttreny@gmail.com
########

rm(list=ls())
library(tidyverse)
library(simcf)
library(MASS)
library(psych)
library(stargazer)
library(RColorBrewer)

select <- dplyr::select
options(dplyr.print_max = 1e9)

#you will want to set this to the replication folder
#setwd()

# read in us data
us <- read_csv("replication_data_us.csv")

# create scales
us$social_deprivation <- ((us$socDep + us$socDep2)/6)*-1
us$political_deprivation <- ((us$polDep1 + us$polDep2)/20)*-1
us$economic_deprivation <- (us$ecDep/10)*-1

# dependent variables rescaled
us$teaparty <- us$teaparty/5
us$trump <- us$trump/5
us$third_party <- us$third_party/4

# partisanship
us <- us %>% mutate(repub = pid7 > 4)
us <- us %>% mutate(dem = pid7 < 4)
us <- us %>% mutate(indep = pid7 == 4)

us$repub3 <- NA
us$repub3[us$pid7==4] <- 2
us$repub3[us$pid7 > 4] <- 3
us$repub3[us$pid7<4] <- 1

#read in uk data
uk <- read_csv("replication_data_uk.csv")

#subset to whites
uk <- uk[uk$white==1,]

#rename weight
uk$wt <- uk$W8

#create IV deprivation scales
uk$social_deprivation <- ((uk$dep_soc + uk$dep_soc2)/6)*-1
uk$political_deprivation <- ((uk$dep_pol + uk$dep_power)/20)*-1
uk$economic_deprivation <- (uk$dep_econ/10)*-1

# partisanship
uk <- uk %>% mutate(repub = wpb2a_UK == 2| wpb2a_UK == 4 )
uk <- uk %>% mutate(dem = wpb2a_UK == 1 )
uk$pid2 <- NA
uk$pid2[uk$repub == 1] <- 2
uk$pid2[uk$dem == 1] <- 1

######
# support for UKIP
######

uk$ukip <- uk$wpb3d_UK
uk$ukip[uk$ukip == 99] <- NA
uk$ukip <- (uk$ukip-1)/10

######
# Support for BNP/EDL
######

uk$bnp_edl <- 0
uk$bnp_edl[uk$vote_campaign == 2 | uk$right_wing == 2] <- 1
uk$bnp_edl[uk$vote_campaign == 2 & uk$right_wing == 2] <- 2
uk$bnp_edl[uk$vote_campaign == 3 | uk$right_wing == 3] <- 3
uk$bnp_edl[uk$vote_campaign == 3 & uk$right_wing == 3] <- 4
uk$bnp_edl <- uk$bnp_edl/4

#####
# smaller clean data
#####

dat <- bnp_edl ~ social_deprivation * pid2 + political_deprivation * 
    pid2 + economic_deprivation * pid2 + age + college + female + 
    own_home + married + wt + ukip + soc + soc30 + power + power30 + 
  pol + pol30 + econ + econ30
ukdata <- extractdata(dat, data=uk, na.rm=TRUE)

###
# Descriptives 
###

# Check internal consistency of scales
alpha(cbind(us$socDep,us$socDep2)) 
alpha(cbind(us$polDep1,us$polDep2)) 
alpha(cbind(uk$dep_soc,uk$dep_soc2)) 
alpha(cbind(uk$dep_pol,uk$dep_power)) 

# correlations between IVs
with(us,cor(data.frame(social_deprivation,political_deprivation,economic_deprivation)))
with(ukdata,cor(data.frame(social_deprivation,political_deprivation,economic_deprivation)))

####
# Figure 1 : Different Types of Deprivation By Sample
####

us.means <- us %>%
  summarise(more1=weighted.mean(social_deprivation > 0,wt=wt),
            no1=mean(social_deprivation == 0,wt=wt),
            less1=mean(social_deprivation < 0,wt=wt),
            more2=weighted.mean(political_deprivation > 0,wt=wt),
            no2=mean(political_deprivation == 0,wt=wt),
            less2=mean(political_deprivation < 0,wt=wt),
            more3=weighted.mean(economic_deprivation > 0,wt=wt),
            no3=mean(economic_deprivation == 0,wt=wt),
            less3=mean(economic_deprivation < 0,wt=wt))
temp <- data.frame(y=t(us.means),
           var=c(rep('Social',3),
                  rep('Political',3),
                  rep('Economic',3)),
           valence=c(rep(c('Worse Off','No Difference','Better Off'),3)))
temp$valence <- factor(temp$valence,levels=c('Worse Off',"No Difference", 'Better Off'))
temp$country <- 'United States'

uk.means <- ukdata %>%
  summarise(more1=weighted.mean(social_deprivation > 0,wt=wt),
            no1=mean(social_deprivation == 0,wt=wt),
            less1=mean(social_deprivation < 0,wt=wt),
            more2=weighted.mean(political_deprivation > 0,wt=wt),
            no2=mean(political_deprivation == 0,wt=wt),
            less2=mean(political_deprivation < 0,wt=wt),
            more3=weighted.mean(economic_deprivation > 0,wt=wt),
            no3=mean(economic_deprivation == 0,wt=wt),
            less3=mean(economic_deprivation < 0,wt=wt))
temp2 <- data.frame(y=t(uk.means),
           var=c(rep('Social',3),
                  rep('Political',3),
                  rep('Economic',3)),
           valence=c(rep(c('Worse Off','No Difference','Better Off'),3)))
temp2$valence <- factor(temp2$valence,levels=c('Worse Off',"No Difference", 'Better Off'))
temp2$country <- 'Britain'

out <- rbind(temp2,temp)
out$country <- factor(out$country,levels=c('United States','Britain'))
cols <- brewer.pal(n = 3,name = 'Set3')
ggplot(out, aes(x=var, y=y, fill=factor(valence), label=paste(round(y,2)*100,'%',sep=''))) + 
  geom_bar(stat='identity') + 
  coord_flip() +
  labs(x='', y='', title='') + 
  scale_fill_manual(values=c('lightcoral','palegoldenrod','darkseagreen'),name='') +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  facet_wrap(~country,ncol=1) +
  theme_minimal() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x =element_blank(),
        text=element_text(size=15))

#######
# Correlations for Table 2
#######

r.dat <- us[us$repub3==3,]
ind.dat <- us[us$repub3==2,]
dem.dat <- us[us$repub3==1,]

cor(with(r.dat,cbind(social_deprivation,teaparty,trump,third_party)))
cor(with(r.dat,cbind(political_deprivation,teaparty,trump,third_party)))
cor(with(r.dat,cbind(economic_deprivation,teaparty,trump,third_party)))

cor(with(ind.dat,cbind(social_deprivation,teaparty,trump,third_party)))
cor(with(ind.dat,cbind(political_deprivation,teaparty,trump,third_party)))
cor(with(ind.dat,cbind(economic_deprivation,teaparty,trump,third_party)))

cor(with(dem.dat,cbind(social_deprivation,teaparty,trump,third_party)))
cor(with(dem.dat,cbind(political_deprivation,teaparty,trump,third_party)))
cor(with(dem.dat,cbind(economic_deprivation,teaparty,trump,third_party)))

cor(with(us,cbind(social_deprivation,teaparty,trump,third_party)))
cor(with(us,cbind(political_deprivation,teaparty,trump,third_party)))
cor(with(us,cbind(economic_deprivation,teaparty,trump,third_party)))

uk.rep <- ukdata[ukdata$pid2==2,]
uk.dem <- ukdata[ukdata$pid2==1,]

cor(with(ukdata,cbind(social_deprivation,bnp_edl,ukip)))
cor(with(ukdata,cbind(political_deprivation,bnp_edl,ukip)))
cor(with(ukdata,cbind(economic_deprivation,bnp_edl,ukip)))

cor(with(uk.rep,cbind(social_deprivation,bnp_edl,ukip)))
cor(with(uk.rep,cbind(political_deprivation,bnp_edl,ukip)))
cor(with(uk.rep,cbind(economic_deprivation,bnp_edl,ukip)))

cor(with(uk.dem,cbind(social_deprivation,bnp_edl,ukip)))
cor(with(uk.dem,cbind(political_deprivation,bnp_edl,ukip)))
cor(with(uk.dem,cbind(economic_deprivation,bnp_edl,ukip)))

########
# Figure 2
# Nostalgic Deprivation
# and vote for Radical Right
#########

teapartymodel <- teaparty ~ social_deprivation*repub3 + political_deprivation*repub3 + economic_deprivation*repub3 + age + college  + female + own_home + married
teapartyout <- lm(teapartymodel, data=us, weights=wt)
pe <- coef(teapartyout)
vc <- vcov(teapartyout)
simbetas.teaparty <- mvrnorm(1000, pe, vc)

trumpmodel <- trump~ social_deprivation*repub3 + political_deprivation*repub3 + economic_deprivation*repub3 + age + college  + female + own_home + married
trumpout <- lm(trumpmodel, data=us, weights=wt)
pe <- coef(trumpout)
vc <- vcov(trumpout)
simbetas.trump <- mvrnorm(1000, pe, vc)

thirdpartymodel <- third_party ~ social_deprivation*repub3 + political_deprivation*repub3 + economic_deprivation*repub3 + age + college  + female + own_home + married
thirdpartyout <- lm(thirdpartymodel, data=us, weights=wt)
pe <- coef(thirdpartyout)
vc <- vcov(thirdpartyout)
simbetas.third <- mvrnorm(1000, pe, vc)

#simulations min to max on deprivation
dat <- extractdata(teapartymodel, data=us, na.rm=TRUE)
mm <- cfMake(teapartymodel, dat, nscen=9)
mm <- cfChange(mm,'social_deprivation', xpre=-1,x=1, scen=1:3)
mm <- cfChange(mm,'political_deprivation', xpre=-1,x=1, scen=4:6)
mm <- cfChange(mm,'economic_deprivation', xpre=-1,x=1, scen=7:9)
mm <- cfChange(mm, 'repub3', x=3, scen=c(1,4,7))
mm <- cfChange(mm, 'repub3', x=1, scen=c(2,5,8))
mm <- cfChange(mm, 'repub3', x=2, scen=c(3,6,9))
tea <- linearsimfd(mm,simbetas.teaparty,ci=0.95)
trump <- linearsimfd(mm,simbetas.trump,ci=0.95)
third <- linearsimfd(mm,simbetas.third,ci=0.95)

#combine to dataframe
dat <- data.frame(party=rep(c('Republican','Democrat','Independent'),9),
                  pe=c(tea$pe,trump$pe,third$pe),
                  lower=c(tea$lower,trump$lower,third$lower),
                  upper=c(tea$upper,trump$upper,third$upper),
                  x=rep(c(.9,1,1.1,1.9,2,2.1,2.9,3,3.1),3),
                  dv=c(rep("Tea Party",9),rep("Trump",9),rep("Third Party",9)))


ggplot(dat, aes(x=x,y=pe,group=party,color=party,shape=party,label=round(pe,2))) + geom_point() + 
  geom_segment(aes(x=x,xend=x,y=lower,yend=upper)) +
  coord_flip() +
  geom_hline(yintercept = 0,linetype=2,color='red') +
  scale_x_continuous(breaks=c(1,2,3),
                     labels=c('Social Deprivation','Political Deprivation','Economic Deprivation')) +
  labs(y='Predicted Change in Support',x='', title='United States') +
  scale_color_manual(values=c('blue','darkgreen','red'),name='Party') +
  scale_shape_manual(values=c(1,3,2),name='Party') +
  scale_y_continuous(limits=c(-0.6,0.6),breaks=c(-0.3,0,.3,0.6),labels=c(-0.3,0,0.3,0.6)) + 
  theme(text = element_text(size = 12)) +
  facet_wrap(~dv)


#models

bnp_model <- bnp_edl ~ social_deprivation*pid2 + political_deprivation*pid2 + economic_deprivation*pid2 +  age + college  + female + own_home + married
bnp.out <- lm(bnp_model, data=uk, weights=wt)
pe <- coef(bnp.out)
vc <- vcov(bnp.out)
simbetas.bnp <- mvrnorm(1000, pe, vc)

ukip_model <- ukip ~ social_deprivation*pid2 + political_deprivation*pid2 + economic_deprivation*pid2 +  age + college  + female + own_home + married
ukip.out <- lm(ukip_model, data=uk, weights=wt)
pe <- coef(ukip.out)
vc <- vcov(ukip.out)
simbetas.ukip <- mvrnorm(1000, pe, vc)

#counterfactual simulations
dat <- extractdata(bnp_model, data=uk, na.rm=TRUE)
mm <- cfMake(bnp_model, dat, nscen=6)
mm <- cfChange(mm,'social_deprivation', xpre=-1,x=1, scen=1:2)
mm <- cfChange(mm,'political_deprivation', xpre=-1,x=1, scen=3:4)
mm <- cfChange(mm,'economic_deprivation', xpre=-1,x=1, scen=5:6)
mm <- cfChange(mm, 'pid2', x=2, scen=c(1,3,5))
mm <- cfChange(mm, 'pid2', x=1, scen=c(2,4,6))
ukip.res <- linearsimfd(mm,simbetas.ukip,ci=0.95)
bnp.res <- linearsimfd(mm,simbetas.bnp,ci=c(0.95))

#combine to dataframe
dat <- data.frame(party=rep(c('Conservative','Labour'),6),
                  pe=c(ukip.res$pe,bnp.res$pe),
                  lower=c(ukip.res$lower,bnp.res$lower),
                  upper=c(ukip.res$upper,bnp.res$upper),
                  x=rep(c(.9,1,1.9,2,2.9,3),2),
                  dv=c(rep("UKIP",6),rep("BNP or EDL",6)))

#plot
ggplot(dat, aes(x=x,y=pe,group=party,color=party,shape=party,label=round(pe,2))) + 
  geom_point() + 
  geom_segment(aes(x=x,xend=x,y=lower,yend=upper)) +
  coord_flip() +
  geom_hline(yintercept = 0,linetype=2,color='red') +
  scale_x_continuous(breaks=c(1,2,3),
                     labels=c('Social Deprivation','Political Deprivation','Economic Deprivation')) +
  labs(y='Predicted Change in Support',x='', title='Britain') +
  scale_color_manual(values=c('red','blue'),name='Party') +
  scale_shape_manual(values=c(2,1),name='Party') +
  theme(text = element_text(size = 12)) +
  facet_wrap(~dv,scales='free_x')

####
# Regression Tables for Figure 2
####

stargazer(teapartyout,trumpout,thirdpartyout,
          ukip.out,bnp.out, 
          type='html')

########
# Appendix
########

#####
# histograms deprivation (iv)
# Figure A1

out <- us %>%
  gather(Deprivation,support,
         which(colnames(us) %in% c('economic_deprivation','political_deprivation','social_deprivation')))
out$Deprivation[out$Deprivation=='economic_deprivation'] <- "Economic Deprivation"
out$Deprivation[out$Deprivation=='political_deprivation'] <- "Political Deprivation"
out$Deprivation[out$Deprivation=='social_deprivation'] <- "Social Deprivation"
ggplot(out,aes(support)) +
  geom_histogram(binwidth = 0.2, fill='white',color='black') +
  geom_vline(data = out %>% 
               group_by(Deprivation) %>% 
               dplyr::summarise(res=mean(support)) , aes(xintercept=res),linetype=2,color='red') +
  facet_wrap(~Deprivation) +
  labs(y='',x='Deprivation \n(Higher Values = More Deprived)',title='US Perceived Deprivation')

out <- ukdata %>% 
  gather(Deprivation,support,
         which(colnames(ukdata) %in% c('economic_deprivation','political_deprivation','social_deprivation')))
out$Deprivation[out$Deprivation=='economic_deprivation'] <- "Economic Deprivation"
out$Deprivation[out$Deprivation=='political_deprivation'] <- "Political Deprivation"
out$Deprivation[out$Deprivation=='social_deprivation'] <- "Social Deprivation"
ggplot(out,aes(support)) +
  geom_histogram(binwidth = 0.2, fill='white',color='black') +
  geom_vline(data = out %>% 
               group_by(Deprivation) %>% 
               dplyr::summarise(res=mean(support)) , 
             aes(xintercept=res),linetype=2,color='red') +
  facet_wrap(~Deprivation)  +
  labs(y='',x='Deprivation \n(Higher Values = More Deprived)',title='Britain Perceived Deprivation')

# histograms DV (support radical right)
# Figure A2

out <- us %>% gather(DV,support,which(colnames(us) %in% c('teaparty','third_party','trump')))
out$DV[out$DV=='teaparty'] <- "Tea Party"
out$DV[out$DV=='third_party'] <- "Third Party"
out$DV[out$DV=='trump'] <- "Trump"
ggplot(out,aes(support)) +
  geom_histogram(binwidth = 0.2, fill='white',color='black') +
  geom_vline(data = out %>% 
               group_by(DV) %>% 
               dplyr::summarise(res=mean(support)) , 
             aes(xintercept=res),linetype=2,color='red') +
  facet_wrap(~DV) +
  labs(y='',x='Support',title='US Radical Right Support')

out <- ukdata %>% 
  gather(DV,support,
         which(colnames(ukdata) %in% c('bnp_edl','ukip')))
out$DV[out$DV=='bnp_edl'] <- "BNP or EDL"
out$DV[out$DV=='ukip'] <- "UKIP"
ggplot(out,aes(support)) +
  geom_histogram(binwidth = 0.2, fill='white',color='black') +
  geom_vline(data = out %>% 
               group_by(DV) %>% 
               dplyr::summarise(res=mean(support)), 
             aes(xintercept=res),linetype=2,color='red') +
  facet_wrap(~DV)+
  labs(y='',x='Support',title='Britain Radical Right Support')


######
# Checking other deprivation measures
# Only looking at deprived now
# Figure A3
######

# recode deprivation scales
# deprived now
us$social_deprivation <- (abs(us$sd1-5)-1)/3
us$political_deprivation <- (abs(us$pd1 + us$pd3 - 20)/20)
us$economic_deprivation <- (abs(us$ec1-10))/10

teapartymodel <- teaparty ~ social_deprivation*repub3 + political_deprivation*repub3 + economic_deprivation*repub3 + age + college  + female + own_home + married
teapartyout <- lm(teapartymodel, data=us, weights=wt)
pe <- coef(teapartyout)
vc <- vcov(teapartyout)
simbetas.teaparty <- mvrnorm(1000, pe, vc)

trumpmodel <- trump~ social_deprivation*repub3 + political_deprivation*repub3 + economic_deprivation*repub3 + age + college  + female + own_home + married
trumpout <- lm(trumpmodel, data=us, weights=wt)
pe <- coef(trumpout)
vc <- vcov(trumpout)
simbetas.trump <- mvrnorm(1000, pe, vc)

thirdpartymodel <- third_party ~ social_deprivation*repub3 + political_deprivation*repub3 + economic_deprivation*repub3 + age + college  + female + own_home + married
thirdpartyout <- lm(thirdpartymodel, data=us, weights=wt)
pe <- coef(thirdpartyout)
vc <- vcov(thirdpartyout)
simbetas.third <- mvrnorm(1000, pe, vc)

#counterfactuals
dat <- extractdata(teapartymodel, data=us, na.rm=TRUE)
mm <- cfMake(teapartymodel, dat, nscen=9)
mm <- cfChange(mm,'social_deprivation', xpre=0,x=1, scen=1:3)
mm <- cfChange(mm,'political_deprivation', xpre=0,x=1, scen=4:6)
mm <- cfChange(mm,'economic_deprivation', xpre=0,x=1, scen=7:9)
mm <- cfChange(mm, 'repub3', x=3, scen=c(1,4,7))
mm <- cfChange(mm, 'repub3', x=1, scen=c(2,5,8))
mm <- cfChange(mm, 'repub3', x=2, scen=c(3,6,9))
tea <- linearsimfd(mm,simbetas.teaparty,ci=0.95)
trump <- linearsimfd(mm,simbetas.trump,ci=0.95)
third <- linearsimfd(mm,simbetas.third,ci=0.95)

dat <- data.frame(party=rep(c('Republican','Democrat','Independent'),9),
                  pe=c(tea$pe,trump$pe,third$pe),
                  lower=c(tea$lower,trump$lower,third$lower),
                  upper=c(tea$upper,trump$upper,third$upper),
                  x=rep(c(.9,1,1.1,1.9,2,2.1,2.9,3,3.1),3),
                  dv=c(rep("Tea Party",9),rep("Trump",9),rep("Third Party",9)))

ggplot(dat, aes(x=x,y=pe,group=party,color=party,shape=party,label=round(pe,2))) + 
  geom_point() + 
  geom_segment(aes(x=x,xend=x,y=lower,yend=upper)) +
  coord_flip() +
  geom_hline(yintercept = 0,linetype=2,color='red') +
  scale_x_continuous(breaks=c(1,2,3),
                     labels=c('Socially \nDeprived \nNow','Politically \nDeprived \nNow','Economically \nDeprived \nNow')) +
  labs(y='Predicted Change in Support',x='', title='United States') +
  scale_color_manual(values=c('blue','darkgreen','red'),name='Party') +
  scale_shape_manual(values=c(1,3,2),name='Party') +
  scale_y_continuous(limits=c(-0.6,0.6),breaks=c(-0.3,0,.3,0.6),labels=c(-0.3,0,0.3,0.6)) + 
  theme(text = element_text(size = 12)) +
  facet_wrap(~dv)

uk$social_deprivation <- (abs(uk$soc-5)-1)/3
uk$political_deprivation <- (abs(uk$power + uk$pol - 20)/20)
uk$economic_deprivation <- (abs(uk$econ-10))/10

bnp_model <- bnp_edl ~ social_deprivation*pid2 + political_deprivation*pid2 + economic_deprivation*pid2 +  age + college  + female + own_home + married
bnp.out <- lm(bnp_model, data=uk, weights=wt)
pe <- coef(bnp.out)
vc <- vcov(bnp.out)
simbetas.bnp <- mvrnorm(1000, pe, vc)

ukip_model <- ukip ~ social_deprivation*pid2 + political_deprivation*pid2 + economic_deprivation*pid2 +  age + college  + female + own_home + married
ukip.out <- lm(ukip_model, data=uk, weights=wt)
pe <- coef(ukip.out)
vc <- vcov(ukip.out)
simbetas.ukip <- mvrnorm(1000, pe, vc)

dat <- extractdata(bnp_model, data=uk, na.rm=TRUE)
mm <- cfMake(bnp_model, dat, nscen=6)
mm <- cfChange(mm,'social_deprivation', xpre=0,x=1, scen=1:2)
mm <- cfChange(mm,'political_deprivation', xpre=0,x=1, scen=3:4)
mm <- cfChange(mm,'economic_deprivation', xpre=0,x=1, scen=5:6)
mm <- cfChange(mm, 'pid2', x=2, scen=c(1,3,5))
mm <- cfChange(mm, 'pid2', x=1, scen=c(2,4,6))
ukip.res <- linearsimfd(mm,simbetas.ukip,ci=0.95)
bnp.res <- linearsimfd(mm,simbetas.bnp,ci=c(0.95))

dat <- data.frame(party=rep(c('Conservative','Labour'),6),
                  pe=c(ukip.res$pe,bnp.res$pe),
                  lower=c(ukip.res$lower,bnp.res$lower),
                  upper=c(ukip.res$upper,bnp.res$upper),
                  x=rep(c(.9,1,1.9,2,2.9,3),2),
                  dv=c(rep("UKIP",6),rep("BNP or EDL",6)))
ggplot(dat, aes(x=x,y=pe,group=party,color=party,shape=party,label=round(pe,2))) + geom_point() + 
  geom_segment(aes(x=x,xend=x,y=lower,yend=upper)) +
  coord_flip() +
  geom_hline(yintercept = 0,linetype=2,color='red') +
  scale_x_continuous(breaks=c(1,2,3),
                     labels=c('Socially \nDeprived \nNow','Politically \nDeprived \nNow','Economically \nDeprived \nNow')) +
  labs(y='Predicted Change in Support',x='', title='Britain') +
  scale_color_manual(values=c('red','blue'),name='Party') +
  scale_shape_manual(values=c(2,1),name='Party') +
  theme(text = element_text(size = 12)) +
  facet_wrap(~dv,scales='free_x')

#table
stargazer(teapartyout,trumpout,thirdpartyout,
          ukip.out,bnp.out, type='html')

####
# Always deprived
# Figure A4
####

us$social_deprivation <- ifelse(us$sd1 > 2 & us$sd2 > 2,1,0)
us$political_deprivation <- ifelse(us$pd1 <=5 & us$pd2 <=5 | us$pd3 <=5 & us$pd4 <=5 ,1,0)
us$economic_deprivation <- ifelse(us$ec1 <=5 & us$ec2 <=5  ,1,0)

teapartymodel <- teaparty ~ social_deprivation*repub3 + political_deprivation*repub3 + economic_deprivation*repub3 + age + college  + female + own_home + married
teapartyout <- lm(teapartymodel, data=us, weights=wt)
pe <- coef(teapartyout)
vc <- vcov(teapartyout)
simbetas.teaparty <- mvrnorm(1000, pe, vc)

trumpmodel <- trump~ social_deprivation*repub3 + political_deprivation*repub3 + economic_deprivation*repub3 + age + college  + female + own_home + married
trumpout <- lm(trumpmodel, data=us, weights=wt)
pe <- coef(trumpout)
vc <- vcov(trumpout)
simbetas.trump <- mvrnorm(1000, pe, vc)

thirdpartymodel <- third_party ~ social_deprivation*repub3 + political_deprivation*repub3 + economic_deprivation*repub3 + age + college  + female + own_home + married
thirdpartyout <- lm(thirdpartymodel, data=us, weights=wt)
pe <- coef(thirdpartyout)
vc <- vcov(thirdpartyout)
simbetas.third <- mvrnorm(1000, pe, vc)

dat <- extractdata(teapartymodel, data=us, na.rm=TRUE)
mm <- cfMake(teapartymodel, dat, nscen=9)
mm <- cfChange(mm,'social_deprivation', xpre=0,x=1, scen=1:3)
mm <- cfChange(mm,'political_deprivation', xpre=0,x=1, scen=4:6)
mm <- cfChange(mm,'economic_deprivation', xpre=0,x=1, scen=7:9)
mm <- cfChange(mm, 'repub3', x=3, scen=c(1,4,7))
mm <- cfChange(mm, 'repub3', x=1, scen=c(2,5,8))
mm <- cfChange(mm, 'repub3', x=2, scen=c(3,6,9))
tea <- linearsimfd(mm,simbetas.teaparty,ci=0.95)
trump <- linearsimfd(mm,simbetas.trump,ci=0.95)
third <- linearsimfd(mm,simbetas.third,ci=0.95)

dat <- data.frame(party=rep(c('Republican','Democrat','Independent'),9),
                  pe=c(tea$pe,trump$pe,third$pe),
                  lower=c(tea$lower,trump$lower,third$lower),
                  upper=c(tea$upper,trump$upper,third$upper),
                  x=rep(c(.9,1,1.1,1.9,2,2.1,2.9,3,3.1),3),
                  dv=c(rep("Tea Party",9),rep("Trump",9),rep("Third Party",9)))

ggplot(dat, aes(x=x,y=pe,group=party,color=party,shape=party,label=round(pe,2))) + geom_point() + 
  geom_segment(aes(x=x,xend=x,y=lower,yend=upper)) +
  coord_flip() +
  geom_hline(yintercept = 0,linetype=2,color='red') +
  scale_x_continuous(breaks=c(1,2,3),
                     labels=c('Always \nSocially \nDeprived','Always \nPolitically \nDeprived','Always \nEconomically \nDeprived')) +
  labs(y='Predicted Change in Support',x='', title='United States') +
  scale_color_manual(values=c('blue','darkgreen','red'),name='Party') +
  scale_shape_manual(values=c(1,3,2),name='Party') +
  scale_y_continuous(limits=c(-0.6,0.6),breaks=c(-0.3,0,.3,0.6),labels=c(-0.3,0,0.3,0.6)) + 
  theme(text = element_text(size = 12)) +
  facet_wrap(~dv)

#######
# Always deprived
#######

#recode deprivation scales
uk$social_deprivation <- ifelse(uk$soc > 2 & uk$soc30 > 2,1,0)
uk$political_deprivation <- ifelse(uk$power <= 5 & uk$power30 <= 5 & uk$pol <= 5 & uk$pol30 <= 5, 1,0)
uk$economic_deprivation <- ifelse(uk$econ <= 5 & uk$econ30 <= 5,1,0)

bnp_model <- bnp_edl ~ social_deprivation*pid2 + political_deprivation*pid2 + economic_deprivation*pid2 +  age + college  + female + own_home + married
bnp.out <- lm(bnp_model, data=uk, weights=wt)
pe <- coef(bnp.out)
vc <- vcov(bnp.out)
simbetas.bnp <- mvrnorm(1000, pe, vc)

ukip_model <- ukip ~ social_deprivation*pid2 + political_deprivation*pid2 + economic_deprivation*pid2 +  age + college  + female + own_home + married
ukip.out <- lm(ukip_model, data=uk, weights=wt)
pe <- coef(ukip.out)
vc <- vcov(ukip.out)
simbetas.ukip <- mvrnorm(1000, pe, vc)

#counterfactuals
dat <- extractdata(bnp_model, data=uk, na.rm=TRUE)
mm <- cfMake(bnp_model, dat, nscen=6)
mm <- cfChange(mm,'social_deprivation', xpre=0,x=1, scen=1:2)
mm <- cfChange(mm,'political_deprivation', xpre=0,x=1, scen=3:4)
mm <- cfChange(mm,'economic_deprivation', xpre=0,x=1, scen=5:6)
mm <- cfChange(mm, 'pid2', x=2, scen=c(1,3,5))
mm <- cfChange(mm, 'pid2', x=1, scen=c(2,4,6))
ukip.res <- linearsimfd(mm,simbetas.ukip,ci=0.95)
bnp.res <- linearsimfd(mm,simbetas.bnp,ci=c(0.95))

dat <- data.frame(party=rep(c('Conservative','Labour'),6),
                  pe=c(ukip.res$pe,bnp.res$pe),
                  lower=c(ukip.res$lower,bnp.res$lower),
                  upper=c(ukip.res$upper,bnp.res$upper),
                  x=rep(c(.9,1,1.9,2,2.9,3),2),
                  dv=c(rep("UKIP",6),rep("BNP or EDL",6)))
ggplot(dat, aes(x=x,y=pe,group=party,color=party,shape=party,label=round(pe,2))) + geom_point() + 
  geom_segment(aes(x=x,xend=x,y=lower,yend=upper)) +
  coord_flip() +
  geom_hline(yintercept = 0,linetype=2,color='red') +
  scale_x_continuous(breaks=c(1,2,3),
                     labels=c('Always \nSocially \nDeprived','Always \nPolitically \nDeprived','Always \nEconomically \nDeprived')) +
  labs(y='Predicted Change in Support',x='', title='Britain') +
  scale_color_manual(values=c('red','blue'),name='Party') +
  scale_shape_manual(values=c(2,1),name='Party') +
  theme(text = element_text(size = 12)) +
  facet_wrap(~dv,scales='free_x')

#regression tables
stargazer(teapartyout,trumpout,thirdpartyout,
          ukip.out,bnp.out, 
          type='html')
