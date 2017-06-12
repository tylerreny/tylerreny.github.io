rm(list=ls())

library(foreign)
library(dplyr)
library(simcf)
library(MASS)
library(ggplot2)
library(car)
require(RColorBrewer)
library(stargazer)

##############
# LOAD DATA ----
##############

df <- read.csv("replication_data_reny_PRQ_2017.csv", stringsAsFactors=F)

#subset data
data.r <- df[df$repub==1,]
data.d <- df[df$repub==0,]

#######
# Model R ----
#######

model.r <- (dvlogit ~ I(pct_regvote^2)*behind + I(pct_regvote^2)*compete + I(pct_regvote^2)*changes_10yr + 
              pct_regvote*behind + pct_regvote*compete + pct_regvote*changes_10yr + romneyvote2012 + unemp +
              gop_nativism + general + incumbent + year_2012 + year_2014) 

mdata.r <- extractdata(model.r, data=data.r, na.rm=TRUE)

fit.r <- glm(model.r, data=mdata.r, family=binomial(link=logit))

summary(fit.r)

pe.r <- coef(fit.r)
vc.r <- vcov(fit.r)
sims <- 1000
simbetas.r <- mvrnorm(sims, pe.r, vc.r)

#####################
# MODEL DEMOCRAT PRO-APPEALS
#####################

model.d <- (dvlogitpro ~ compete*pct_regvote + behind*pct_regvote + changes_10yr + romneyvote2012  +  
              general + incumbent +dem_nativism + whitedem_state + year_2012 + year_2014 + unemp)

mdata.d <- extractdata(model.d, data=data.d, na.rm=TRUE)

fit.d <- glm(model.d, data=mdata.d, family=binomial(link=logit))
summary(fit.d)

pe.d <- coef(fit.d)
vc.d <- vcov(fit.d)

#for simulations
sims <- 1000
simbetas.d <- mvrnorm(sims, pe.d, vc.d)

################
# Regression Tables
################

stargazer(fit.r, fit.d,
            type="html", 
            single.row=T, 
            title="Immigration Appeals", 
            out="tab1.html",  
            dep.var.labels = c("Anti-Immigrant (R)", "Pro-Immigrant (D)"))

################
## LINE PLOT IMMIGRANT POPULATION GROWTH
################

pop.seq <- seq(24, 162, 3)
n_scen <- length(pop.seq)

ch <- cfMake(model.d, data=mdata.d, nscen=n_scen)
ch <- cfChange(ch, "changes_10yr", x=pop.seq, scen=1:n_scen)
compD <- logitsimev(ch, simbetas.d, ci=0.95) 

ch <- cfMake(model.r, data=mdata.r, nscen=n_scen)
ch <- cfChange(ch, "changes_10yr", x=pop.seq, scen=1:n_scen)
compR <- logitsimev(ch, simbetas.r, ci=0.95) 

data <- data.frame(x = pop.seq, 
           dem.pe = compD$pe,
           dem.lower = compD$lower,
           dem.upper = compD$upper,
           rep.pe = compR$pe,
           rep.lower = compR$lower,
           rep.upper = compR$upper)
      jitter <- rnorm(length(df$changes_10yr), 0, .5)
g2 <- ggplot(data, aes(x=x, y=rep.pe)) + geom_line(color="red") + geom_line(aes(y=dem.pe), color="blue") + 
  geom_ribbon(aes(ymin=dem.lower, ymax=dem.upper), fill="blue", alpha=.2) + 
  geom_ribbon(aes(ymin=rep.lower, ymax=rep.upper), fill="red", alpha=.2) + theme_minimal() +
  labs(y="Probability Immigration Appeals", x="Growth Latino Population (%)") +
  annotate("text", x=60, y=.85, label="Republican (Anti)", size=6, color="red") + 
  annotate("text", x=120, y=.4, label="Democrat (Pro)", size=6, color="blue") +
  geom_segment(data=df, aes(x=changes_10yr + jitter, xend=changes_10yr + jitter, y=0, yend=0.02), alpha=0.5) +
  theme(text = element_text(size=20)) 

################
## FD IMMIGRANT POPULATION GROWTH
################

pop.seq <- seq(24, 162, 3)
n_scen <- length(pop.seq)

ch <- cfMake(model.r, data=mdata.r, nscen=1)
ch <- cfChange(ch, "changes_10yr", xpre=min(pop.seq), x=max(pop.seq), scen=1)
logitsimfd(ch, simbetas.r, ci=0.95) 

ch <- cfMake(model.d, data=mdata.d, nscen=1)
ch <- cfChange(ch, "changes_10yr", xpre=min(pop.seq), x=max(pop.seq), scen=1)
logitsimfd(ch, simbetas.d, ci=0.95) 

##############
# IN SAMPLE
##############

#DEMOCRATS
xpre <- model.matrix(model.d, data=mdata.d)
xpre[,"changes_10yr"] <- rep(min(mdata.d$changes_10yr), nrow(mdata.d))
x <- model.matrix(model.d, data=mdata.d)
x[,"changes_10yr"] <- rep(max(mdata.d$changes_10yr), nrow(mdata.d))

ci=0.95
nscen <- nrow(mdata.d)
nci <- length(ci)

res <- list(pe = rep(NA, nscen), 
            lower = matrix(NA, nrow = nscen, ncol = nci), 
            upper = matrix(NA, nrow = nscen, ncol = nci)) #create empty list for results
for (i in 1:nscen) {
  simmu1 <- simbetas.d %*% xpre[i, ]
  simmu2 <- simbetas.d %*% x[i, ]
  simy <- 1/(1 + exp(-simmu2)) - 1/(1 + exp(-simmu1))
  res$pe[i] <- mean(simy)
  cint <- quantile(simy, probs = c((1 - ci)/2, (1 - (1 - ci)/2)))
  res$lower[i,] <- cint[1]
  res$upper[i,] <- cint[2]
}
res$lower <- drop(res$lower)
res$upper <- drop(res$upper)

results <- data.frame(pe=mean(res$pe), 
                      lower=mean(res$lower), 
                      upper=mean(res$upper))
results

#REPUBLICANS

xpre <- model.matrix(model.r, data=mdata.r)
xpre[,"changes_10yr"] <- rep(min(mdata.r$changes_10yr), nrow(mdata.r))
x <- model.matrix(model.r, data=mdata.r)
x[,"changes_10yr"] <- rep(max(mdata.r$changes_10yr), nrow(mdata.r))

ci=0.95
nscen <- nrow(mdatar2)
nci <- length(ci)

res <- list(pe = rep(NA, nscen), 
            lower = matrix(NA, nrow = nscen, ncol = nci), 
            upper = matrix(NA, nrow = nscen, ncol = nci))
for (i in 1:nscen) {
  simmu1 <- simbetas.r %*% xpre[i, ]
  simmu2 <- simbetas.r %*% x[i, ]
  simy <- 1/(1 + exp(-simmu2)) - 1/(1 + exp(-simmu1))
  res$pe[i] <- mean(simy)
  cint <- quantile(simy, probs = c((1 - ci)/2, (1 - (1 - ci)/2)))
  res$lower[i,] <- cint[1]
  res$upper[i,] <- cint[2]
}

res$lower <- drop(res$lower)
res$upper <- drop(res$upper)

results <- data.frame(pe = mean(res$pe), 
                      lower = mean(res$lower), 
                      upper = mean(res$upper))
results

#######################
# DEMOCRATIC PRO APPEALS -----
#######################

pop.seq <- seq(.2,40, .5)
n_scen <- length(pop.seq)

ch <- cfMake(model.d, data=mdata.d, nscen=n_scen)
ch <- cfChange(ch, "pct_regvote", x=pop.seq, scen=1:n_scen)
ch <- cfChange(ch, "compete", x=1, scen=1:n_scen)
ch <- cfChange(ch, "behind", x=0, scen=1:n_scen)
comp <- logitsimev(ch, simbetas.d, ci=c(0.68)) 

ch <- cfMake(model.d, data=mdata.d, nscen=n_scen)
ch <- cfChange(ch, "pct_regvote", x=pop.seq, scen=1:n_scen)
ch <- cfChange(ch, "compete", x=0, scen=1:n_scen)
ch <- cfChange(ch, "behind", x=1, scen=1:n_scen)
behind <- logitsimev(ch, simbetas.d, ci=c(0.68)) 

ch <- cfMake(model.d, data=mdata.d, nscen=n_scen)
ch <- cfChange(ch, "pct_regvote", x=pop.seq, scen=1:n_scen)
ch <- cfChange(ch, "compete", x=0, scen=1:n_scen)
ch <- cfChange(ch, "behind", x=0, scen=1:n_scen)
ahead <- logitsimev(ch, simbetas.d, ci=c(0.68)) 

data <- data.frame(x = pop.seq,
                   comp.pe = comp$pe,
                   comp.upper = comp$upper,
                   comp.lower = comp$lower,
                   behind.pe = behind$pe,
                   behind.upper = behind$upper,
                   behind.lower = behind$lower,
                   ahead.pe = ahead$pe,
                   ahead.upper = ahead$upper,
                   ahead.lower = ahead$lower)
jitter <- rnorm(294,0,.5)

g <- ggplot(data, aes(x=x, y=comp.pe)) + geom_line(color="red") + 
  #geom_ribbon(aes(ymin=comp.lower, ymax=comp.upper), fill="red", alpha=.2) +
  geom_line(aes(x=x, y=behind.pe), linetype=4, color="purple", size=1) + 
  geom_line(aes(x=x, y=ahead.pe), linetype=3, color="darkblue", size=1) +
  theme_bw() +
  labs(y="Probability Pro-Immigrant Appeals", x="Latino Voters % Reg Voters") + 
  theme(text = element_text(size=20)) + 
  annotate("segment", x=0, xend=3, y=1,yend=1, color="red", size=1.5) +
  annotate("segment", x=0, xend=3, y=.95,yend=.95, color="blue", linetype=3, size=1.5) +
  annotate("segment", x=0, xend=3, y=.9,yend=.9, color="purple", linetype=4, size=1.5) +
  annotate("text", x=4, y=1, color="red", label="Competitive", hjust=0) +
  annotate("text", x=4, y=.95, color="blue", label="Ahead", hjust=0) +
  annotate("text", x=4, y=.9, color="purple", label="Behind", hjust=0) +
  geom_segment(data=data.d, aes(x=pct_regvote + jitter, xend=pct_regvote + jitter, y=0, yend=0.02), alpha=0.5)

g

###############
# DEM Pro Appeals First Differences
###############

ch <- cfMake(model.d, data=mdata.d, nscen=1)
ch <- cfChange(ch, "pct_regvote", xpre=mean(mdata.d$pct_regvote), x=mean(mdata.d$pct_regvote) + sd(mdata.d$pct_regvote), scen=1)
ch <- cfChange(ch, "compete", x=1, scen=1:n_scen)
ch <- cfChange(ch, "behind", x=0, scen=1:n_scen)
logitsimfd(ch, simbetas.d, ci=c(0.95)) 

###############
# REP ANTI IMMIGRANT APPEALS ------
###############

pop.seq <- seq(1, 38, 1)
growth.seq <- seq(100, 28, length.out=length(pop.seq))
n_scen <- length(pop.seq)

ch <- cfMake(model.r, data=mdata.r, nscen=n_scen)
ch <- cfChange(ch, "pct_regvote", x=pop.seq, scen=1:n_scen)
ch <- cfChange(ch, "compete", x=1, scen=1:n_scen)
ch <- cfChange(ch, "behind", x=0, scen=1:n_scen)
ch <- cfChange(ch, "changes_10yr", x=growth.seq, scen=1:n_scen)
res1 <- logitsimev(ch, simbetas.r, ci=c(0.68))

ch <- cfMake(model.r, data=mdata.r, nscen=n_scen)
ch <- cfChange(ch, "pct_regvote", x=pop.seq, scen=1:n_scen)
ch <- cfChange(ch, "compete", x=0, scen=1:n_scen)
ch <- cfChange(ch, "behind", x=1, scen=1:n_scen)
ch <- cfChange(ch, "changes_10yr", x=growth.seq, scen=1:n_scen)
res2 <- logitsimev(ch, simbetas.r, ci=c(0.68)) 

ch <- cfMake(model.r, data=mdata.r, nscen=n_scen)
ch <- cfChange(ch, "pct_regvote", x=pop.seq, scen=1:n_scen)
ch <- cfChange(ch, "compete", x=0, scen=1:n_scen)
ch <- cfChange(ch, "behind", x=0, scen=1:n_scen)
ch <- cfChange(ch, "changes_10yr", x=growth.seq, scen=1:n_scen)
res3 <- logitsimev(ch, simbetas.r, ci=c(0.68)) 

data <- data.frame(x=pop.seq,
                   res1.pe = res1$pe,
                   res1.upper = res1$upper,
                   res1.lower = res1$lower,
                   res2.pe = res2$pe,
                   res2.upper = res2$upper,
                   res2.lower = res2$lower,
                   res3.pe = res3$pe,
                   res3.upper = res3$upper,
                   res3.lower = res3$lower)
jitter <- rnorm(362,0,.5)
g1 <- ggplot(data, aes(x=x, y=res1.pe)) + geom_line(color="red") + theme_bw() + 
  geom_line(aes(y=res2.pe), linetype=3, color="blue", size=1.25) +
  geom_line(aes(y=res3.pe), linetype=4, color="purple", size=1.25) + 
  scale_y_continuous(limits = c(0,1)) +
  labs(x="% Latino Registered Voters", y= "Probability Anti-Immigrant Appeal") + 
  theme(text = element_text(size=20)) +
  annotate("segment", x=0, xend=2, y=1,yend=1, color="red", size=1) +
  annotate("segment", x=0, xend=2, y=.95,yend=.95, color="blue", linetype=3, size=1) +
  annotate("segment", x=0, xend=2, y=.9,yend=.9, color="purple", linetype=4, size=1) +
  annotate("text", x=3, y=1, color="red", label="Competitive", hjust=0) +
  annotate("text", x=3, y=.95, color="blue", label="Behind", hjust=0) +
  annotate("text", x=3, y=.9, color="purple", label="Ahead", hjust=0) +
  geom_segment(data=data.r, aes(x=pct_regvote + jitter, xend=pct_regvote + jitter, y=0, yend=0.02), alpha=0.5)
g1

###########################
## TABLES
#############################

stargazer(fit.r, fit.d,
            type="latex", 
            single.row=T, 
            title="Immigration Appeals", 
            out="tab1.tex",  
            dep.var.labels = c("Anti-Immigrant (R)", "Pro-Immigrant (D)"))


##################
# Attitude Data
##################

#######
# 2010
#######

rm(list=ls())
library(foreign)
library(plyr)

#this section requires going and getting the 2010 and 2012 CCES data

df_2010 <- read.dta('cces_2010.dta', convert.factors=F)
df_2012 <- read.dta('cces_2012.dta', convert.factors=F)

#partisan ID V212d
df_2010$pidnew <- df_2010$V212d #7 is strong republican
df_2010$pidnew[df_2010$pidnew==8] <- NA 

df_2012$pidnew <- df_2012$pid7 #7 is strong republican
df_2012$pidnew[df_2012$pidnew==8] <- NA 

#race of respondent V211 (which racial or ethnic group best describes you?)
table(df_2010$V211)
df_2010$white <- rep(0, length(df_2010$V211))
df_2010$white[df_2010$V211==1] <- 1

df_2010$whitedem <- rep(0, length(df_2010$V211))
df_2010$whitedem[df_2010$white == 1 & df_2010$pidnew == 1 | 
                   df_2010$pidnew == 2 | df_2010$pidnew == 3] <- 1

df_2010$whiterep <- rep(0, length(df_2010$V211))
df_2010$whiterep[df_2010$white == 1 & df_2010$pidnew == 5 | 
                   df_2010$pidnew == 6 | df_2010$pidnew == 7] <- 1

#2012
df_2012$white <- rep(0, length(df_2012$race))
df_2012$white[df_2012$race==1] <- 1

df_2012$whitedem <- rep(0, nrow(df_2012))
df_2012$whitedem[df_2012$white == 1 & df_2012$pidnew == 1 | 
                   df_2012$pidnew == 2 | df_2012$pidnew == 3] <- 1

df_2012$whiterep <- rep(0, nrow(df_2012))
df_2012$whiterep[df_2012$white == 1 & df_2012$pidnew == 5 | 
                   df_2012$pidnew == 6 | df_2012$pidnew == 7] <- 1


# Immigration questions CC322
df_2010$CC322_1[df_2010$CC322_1==2] <- 0  #fine businesses
df_2010$CC322_1[is.na(df_2010$CC322_1)] <- 0

df_2010$CC322_2[df_2010$CC322_2==2] <- 0 #legal status
df_2010$CC322_2[df_2010$CC322_2==1] <- -1
df_2010$CC322_2[is.na(df_2010$CC322_2)] <- 0

df_2010$CC322_3[df_2010$CC322_3==2] <- 0 #more guest workers
df_2010$CC322_3[df_2010$CC322_3==1] <- -1
df_2010$CC322_3[is.na(df_2010$CC322_3)] <- 0

df_2010$CC322_4[df_2010$CC322_4==2] <- 0 #more border patrols
df_2010$CC322_4[is.na(df_2010$CC322_4)] <- 0

df_2010$CC322_5[df_2010$CC322_5==2] <- 0 #police question
df_2010$CC322_5[is.na(df_2010$CC322_5)] <- 0

df_2010$imm_view <-  df_2010$CC322_1 + df_2010$CC322_2 + df_2010$CC322_3 + 
  df_2010$CC322_4 + df_2010$CC322_5

dem_nativism <- ddply(df_2010[df_2010$whitedem==1,], "V206", summarize, dem_nativism=mean(imm_view, use="complete.obs"))
gop_nativism  <- ddply(df_2010[df_2010$whiterep==1,], "V206", summarize, gop_nativism=mean(imm_view, use="complete.obs"))
ind_nativism <- ddply(df_2010[df_2010$pidnew==4,], "V206", summarize, ind_nativism=mean(imm_view, use="complete.obs"))

nativism <- cbind(dem_nativism, gop_nativism, ind_nativism[1:51,])
nativism <- nativism[,c(2, 4, 6)]

nativism_2010 <- cbind(state.abb, nativism[1:50,])
colnames(nativism_2010) <- c("state", "dem_nat_2010", "gop_nat_2010", "ind_nat_2010")
nativism_2010[,1] <- as.character(nativism_2010[,1])
str(nativism_2010)


#imm views 2012
df_2012$CC322_1[df_2012$CC322_1==2] <- 0 
df_2012$CC322_1[is.na(df_2012$CC322_1)] <- 0
df_2012$CC322_1[df_2012$CC322_2==1] <- -1

df_2012$CC322_2[df_2012$CC322_2==2] <- 0 #legal status
df_2012$CC322_2[is.na(df_2012$CC322_2)] <- 0

df_2012$CC322_3[df_2012$CC322_3==2] <- 0 #more guest workers
df_2012$CC322_3[is.na(df_2012$CC322_3)] <- 0

df_2012$CC322_4[df_2012$CC322_4==2] <- 0 #more border patrols
df_2012$CC322_4[is.na(df_2012$CC322_4)] <- 0

df_2012$CC322_5[df_2012$CC322_5==2] <- 0 #police question
df_2012$CC322_5[is.na(df_2012$CC322_5)] <- 0

df_2012$CC322_6[df_2012$CC322_6==2] <- 0 #police question
df_2012$CC322_6[is.na(df_2012$CC322_6)] <- 0

df_2012$imm_view <-  df_2012$CC322_1 + df_2012$CC322_2 + df_2012$CC322_3 + 
  df_2012$CC322_4 + df_2012$CC322_5 + df_2012$CC322_6

dem_nativism <- ddply(df_2012[df_2012$whitedem==1,], "inputstate", summarize, dem_nativism=mean(imm_view, use="complete.obs"))
gop_nativism  <- ddply(df_2012[df_2012$whiterep==1,], "inputstate", summarize, gop_nativism=mean(imm_view, use="complete.obs"))
ind_nativism <- ddply(df_2012[df_2012$pidnew==4,], "inputstate", summarize, ind_nativism=mean(imm_view, use="complete.obs"))

nativism <- cbind(dem_nativism, gop_nativism, ind_nativism[1:51,])
nativism <- nativism[,c(2, 4, 6)]

nativism_2012 <- cbind(state.abb, nativism[1:50,])
colnames(nativism_2012) <- c("state", "dem_nat_2012", "gop_nat_2012", "ind_nat_2012")
nativism_2012[,1] <- as.character(nativism_2012[,1])

######
# Robustness
#######

#latino turnout

model1 <- (dvlogit ~ I(pct_regvote^2)*behind + I(pct_regvote^2)*compete + I(pct_regvote^2)*changes_10yr + 
              pct_regvote*behind + pct_regvote*compete + pct_regvote*changes_10yr + romneyvote2012 + unemp +
              gop_nativism + general + incumbent + year_2012 + year_2014) 
model2 <- (dvlogit ~ I(pct_regvote^2)*behind + I(pct_regvote^2)*compete + I(pct_regvote^2)*changes_10yr + 
              pct_regvote*behind + pct_regvote*compete + pct_regvote*changes_10yr + romneyvote2012 + unemp +
              gop_nativism + general + incumbent + year_2012 + year_2014 + turnout) 
model3 <- (dvlogitpro ~ compete*pct_regvote + behind*pct_regvote + changes_10yr + romneyvote2012  + unemp + 
              general + incumbent +dem_nativism + whitedem_state + year_2012 + year_2014)
model4<- (dvlogitpro ~ compete*pct_regvote + behind*pct_regvote + changes_10yr + romneyvote2012  +  unemp +
              general + incumbent +dem_nativism + whitedem_state + year_2012 + year_2014 + turnout)

mdata1 <- extractdata(model1, data=data.r, na.rm=TRUE)
mdata2 <- extractdata(model2, data=data.r, na.rm=TRUE)
mdata3 <- extractdata(model3, data=data.d, na.rm=TRUE)
mdata4 <- extractdata(model4, data=data.d, na.rm=TRUE)

fit1 <- glm(model1, data=mdata1, family=binomial(link="logit"))
fit2 <- glm(model2, data=mdata2, family=binomial(link="logit"))
fit3 <- glm(model3, data=mdata3, family=binomial(link="logit"))
fit4 <- glm(model4, data=mdata4, family=binomial(link="logit"))

stargazer(fit1,fit2,fit3,fit4,
          type="html", 
          single.row=T, 
          title="Immigration Appeals", 
          out="turnout_robust.html",  
          dep.var.labels = c("Anti-Immigrant (R)","Anti-Immigrant (R)", "Pro-Immigrant (D)","Pro-Immigrant (D)"))

# descriptives on IVs
model1 <- (dvlogit ~ I(pct_regvote^2)*behind + I(pct_regvote^2)*compete + I(pct_regvote^2)*changes_10yr + 
              pct_regvote*behind + pct_regvote*compete + pct_regvote*changes_10yr + romneyvote2012 + unemp +
              gop_nativism + general + incumbent + year_2012 + year_2014) 

out <- df %>%
  group_by(year) %>%
  summarise(compete_mean = mean(compete,na.rm=T),
            sd_compete = sd(compete,na.rm = T),
            ahead_mean = mean(ahead, na.rm=T),
            sdahead = sd(ahead, na.rm=T),
            behind_mean = mean(behind, na.rm=T),
            sdbehind = sd(behind, na.rm=T),
            latino_voters_mean = mean(pct_regvote,na.rm=T)/100,
            sdlatino_voters = sd(pct_regvote,na.rm=T)/100,
            latino_growth_mean=mean(changes_10yr,na.rm=T)/100,
            sdlatino_growth=sd(changes_10yr,na.rm=T)/100,
            romneyvote2012_mean=mean(romneyvote2012,na.rm=T),
            sdromneyvote2012=sd(romneyvote2012,na.rm=T),
            unemp_mean=mean(unemp,na.rm=T),
            sdunemp=sd(unemp,na.rm=T),
            gop_nativism_mean = mean(gop_nativism,na.rm=T),
            sdgop_nativism = sd(gop_nativism,na.rm=T),
            incumbent_mean = mean(incumbent,na.rm=T),
            sdincumbent = sd(incumbent,na.rm=T))
round(t(out),3)
kable(round(t(out),3),format = 'latex')

# non parametric plot
ggplot(df[df$repub==1,],aes(pct_regvote,dvlogit)) + geom_jitter(height=.01,width=.1) +
  stat_smooth(method = "lm", formula = y ~ splines::bs(x, 2),color='red',se=F) +
    labs(title='Republican Immigration Appeals',x='Latino Reg Voters',y='') +
     scale_y_discrete(limits=c(0,1),labels=c('no appeal','appeal'))

