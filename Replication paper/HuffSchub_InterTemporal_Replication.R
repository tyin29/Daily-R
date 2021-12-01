########################
## Replication File for Huff and Schub
## "The Inter-Temporal Tradeoff in Mobilizing Support for War"
## ISQ
########################


#####
# Contains code for 
   # (I) Base survey results
     # (I.1) Manuscript
       # Figure 1 Panel A
       # Figure 1 Panel B
       # Figure 2
     # (I.2) Supporting Files 
       # Figure A1
       # Figure A2
       # Tables A1-A6 
       # Figure A5
       # Result described in 3.4   
   
   
   # (II) Mechanisms
     # (II.1) Manuscript
       # Figure 3
       # Figure 4
     # (II.2) Supporting Files  
       # Figure A6

   # (III) Partisanship
     # (III.1) Supporting Files
       # Figure A3
       # Figure A4
#####



###################
### (I) Base survey results
###################

### Set working directory and load the data
rm(list=ls())
library(foreign)
data <- read.dta("HuffSchub_InterTemporal_BaseSurvey.dta")
head(data)

########
## (I.1) Manuscript


#### Figure 1 Panel A

mobil<-lm(r1approvalexclude~r1long,data=data)
summary(mobil) 
effect2<-summary(mobil)$coefficients[,1][2]
se2<-summary(mobil)$coefficients[,2][2]

par(mfrow=c(1,1))
par(oma=c(1,1,1,1))
par(mar=c(3,1,4,1))
plot(effect2,1,cex=3,pch=20,xlim=c(-.4,0.05),yaxt="n",ylab="",xaxt="n")
segments((effect2-1.96*se2),1,(effect2+1.96*se2),1,lwd=4)
abline(v=0,lty=3)
axis(1,at=c(seq(-.4,0.05,by=0.05)),labels=c("-40", "-35", "-30","-25","-20","-15","-10","-5","0","5"))
mtext("Marginal Effect of Round 1 Long Message",side=3,cex=1.4,line=1.5)
mtext("on Initial Approval",side=3,cex=1.4,line=0.3)
mtext("Percentage Point Change in Public Approval",side=1,cex=1.1,line=2.3)
text(-0.05,0.65,"baseline=48%")


#### Figure 1 Panel B

 # (a) Effect on R2 approval
correctr2<-lm(r2approvalexclude~r1long,data=data)
summary(correctr2) 
effect3<-summary(correctr2)$coefficients[,1][2]
se3<-summary(correctr2)$coefficients[,2][2]
 # (b) Effect on R3 approval
correctr3<-lm(r3approvalexclude~r1long,data=data)
summary(correctr3)
effect4<-summary(correctr3)$coefficients[,1][2]
se4<-summary(correctr3)$coefficients[,2][2]
 # (c) Effect on RF approval
correctrf<-lm(rfapprovalexclude~r1long,data=data[data$longwar==1,])
summary(correctrf)
effect5<-summary(correctrf)$coefficients[,1][2]
se5<-summary(correctrf)$coefficients[,2][2]

par(mfrow=c(1,1))
par(oma=c(3,2,3,1))
plot(effect3,3,cex=3,pch=20,xlim=c(-0.15,0.3),yaxt="n",ylab="",ylim=c(0,4),xlab="",xaxt="n")
segments((effect3-1.96*se3),3,(effect3+1.96*se3),3,lwd=4)
abline(v=0,lty=3)
mtext("Long War: Marginal Effect of Initial Message",side=3,cex=1.4,line=1.5)
mtext("Being Correct on Subsequent Approval",side=3,cex=1.4,line=0.3)
points(effect4,2,cex=3,pch=20)
segments((effect4-1.96*se4),2,(effect4+1.96*se4),2,lwd=4)
points(effect5,1,cex=3,pch=20)
segments((effect5-1.96*se5),1,(effect5+1.96*se5),1,lwd=4)
axis(2,at=c(1:3),labels=c("Final","R3","R2"),cex.axis=1.2)
axis(1,at=c(seq(-0.1,0.3,by=0.1)),labels=c("-10","0","10","20","30"))
mtext("Percentage Point Change in Public Approval",side=1,cex=1.1,line=2.3)
text(-0.05,2.96,"baseline=12%")
text(-0.05,1.96,"baseline=6%")
text(-0.05,0.96,"baseline=11%")


#### Figure 2

  ## Lower Panel R3
staysr3<-lm(r3approvalexclude~sss, data=data[data$r1long==0,])
summary(staysr3)
effect7<-summary(staysr3)$coefficients[,1][2]
se7<-summary(staysr3)$coefficients[,2][2]
   # Lower Panel RF
staysrf<-lm(rfapprovalexclude~sss, data=data[data$r1long==0 & data$longwar==1,])
summary(staysrf)
effect8<-summary(staysrf)$coefficients[,1][2]
se8<-summary(staysrf)$coefficients[,2][2]

   ## Upper Panel R3 
staysr3b<-lm(r3approvalexclude~sss, data=data[data$r1long==0 & data$r2long==0,])
summary(staysr3b)
effect7b<-summary(staysr3b)$coefficients[,1][2]
se7b<-summary(staysr3b)$coefficients[,2][2]
   ## Upper Panel RF
staysrfb<-lm(rfapprovalexclude~sss, data=data[data$r1long==0 & data$longwar==1 & data$r2long==0,])
summary(staysrfb)
effect8b<-summary(staysrfb)$coefficients[,1][2]
se8b<-summary(staysrfb)$coefficients[,2][2]


par(mfrow=c(2,1))
par(oma=c(1,1,1,1))
par(mar=c(3,2,3,1)) 
plot(effect7b,2,cex=3,pch=20,xlim=c(-0.11,0.11), yaxt="n",ylab="",ylim=c(0.5,2.5),xaxt="n")
mtext("Marginal Effect of Short-Short-Short Message vs.",side=3,cex=1.4,line=1.5)
mtext("Short-Short-Long on Subsequent Approval",side=3,cex=1.4,line=0.3)
segments((effect7b-1.96*se8),2,(effect7b+1.96*se8),2,lwd=4)
points(effect8b,1,cex=3,pch=20)
segments((effect8b-1.96*se8),1,(effect8b+1.96*se8),1,lwd=4)
axis(2,at=c(1:2),labels=c("Final","R3"),cex.axis=1.2)
axis(1,at=c(seq(-0.1,0.1,by=0.05)),labels=c("-10","-5","0","5","10"))
abline(v=0,lty=3)
mtext("Percentage Point Change in Public Approval",side=1,cex=1.1,line=2.3)
text(0.04,1.75,"baseline=8%")
text(0.04,0.75,"baseline=9%")
plot(effect7,4,cex=3,pch=20,xlim=c(-0.11,0.11),yaxt="n",ylab="",ylim=c(2.5,4.5),xaxt="n")
abline(v=0,lty=3)
mtext("Marginal Effect of Short-Short-Short Message vs.",side=3,cex=1.4,line=1.5)
mtext("Short-X-X on Subsequent Approval",side=3,cex=1.4,line=0.3)
segments((effect7-1.96*se7),4,(effect7+1.96*se7),4,lwd=4)
points(effect8,3,cex=3,pch=20)
segments((effect8-1.96*se8),3,(effect8+1.96*se8),3,lwd=4)
axis(2,at=c(3:4),labels=c("Final","R3"),cex.axis=1.2)
axis(1,at=c(seq(-0.1,0.1,by=0.05)),labels=c("-10","-5","0","5","10"))
mtext("Percentage Point Change in Public Approval",side=1,cex=1.1,line=2.3)
text(0.04,2.75,"baseline=11%")
text(0.036,3.75,"baseline=7%")



########
## (I.2) Supporting Files

#### Figure A1

# ideology
avg<-(lm(r1approvalexclude~r1long, data=data))
cons<-(lm(r1approvalexclude~r1long, data=data[data$cons==1,]))
lib<-(lm(r1approvalexclude~r1long, data=data[data$lib==1,]))
mod<-(lm(r1approvalexclude~r1long, data=data[data$mod==1,]))
# education
degree<-(lm(r1approvalexclude~r1long, data=data[data$fyrdegree ==1,]))
nodegree<-(lm(r1approvalexclude~r1long, data=data[data$nfyrdegree ==1,]))
# gender
male<-(lm(r1approvalexclude~r1long, data=data[data$gender ==1,]))
female<-(lm(r1approvalexclude~r1long, data=data[data$gender ==2,]))
# race
white<-(lm(r1approvalexclude~r1long, data=data[data$race ==1,]))
nowhite<-(lm(r1approvalexclude~r1long, data=data[data$race>1,]))  #pooled all other races (small n)
# age
u31<-(lm(r1approvalexclude~r1long, data=data[data$birthyr >1984,]))
o31<-(lm(r1approvalexclude~r1long, data=data[data$birthyr<=1984,])) 
# enemy
enemyno<-(lm(r1approvalexclude~r1long, data=data[data$enemy==1,]))
enemyally<-(lm(r1approvalexclude~r1long, data=data[data$enemy==2,]))
enemyterror<-(lm(r1approvalexclude~r1long, data=data[data$enemy==3,]))
# boots
noboots<-(lm(r1approvalexclude~r1long, data=data[data$boots==0,]))
boots<-(lm(r1approvalexclude~r1long, data=data[data$boots==1,]))

# all together
effectsh1<-c(summary(avg)$coefficients[,1][2],summary(cons)$coefficients[,1][2],summary(lib)$coefficients[,1][2],summary(mod)$coefficients[,1][2],summary(degree)$coefficients[,1][2],summary(nodegree)$coefficients[,1][2],summary(male)$coefficients[,1][2],summary(female)$coefficients[,1][2],summary(white)$coefficients[,1][2],summary(nowhite)$coefficients[,1][2],summary(u31)$coefficients[,1][2],summary(o31)$coefficients[,1][2],summary(enemyno)$coefficients[,1][2],summary(enemyally)$coefficients[,1][2],summary(enemyterror)$coefficients[,1][2],summary(noboots)$coefficients[,1][2],summary(boots)$coefficients[,1][2])
length(effectsh1)
seh1<-c(summary(avg)$coefficients[,2][2],summary(cons)$coefficients[,2][2],summary(lib)$coefficients[,2][2],summary(mod)$coefficients[,2][2],summary(degree)$coefficients[,2][2],summary(nodegree)$coefficients[,2][2],summary(male)$coefficients[,2][2],summary(female)$coefficients[,2][2],summary(white)$coefficients[,2][2],summary(nowhite)$coefficients[,2][2],summary(u31)$coefficients[,2][2],summary(o31)$coefficients[,2][2],summary(enemyno)$coefficients[,2][2],summary(enemyally)$coefficients[,2][2],summary(enemyterror)$coefficients[,2][2],summary(noboots)$coefficients[,2][2],summary(boots)$coefficients[,2][2])

effectsh1<-rev(effectsh1)
seh1<-rev(seh1)

par(mfrow=c(1,1))
par(oma=c(1,7,2,1))
par(mar=c(3,1,1,1))
plot(effectsh1,1:17,xlim=c(-.4,.05),cex=1.2,pch=19,yaxt="n",ylab="",xaxt="n")
segments(effectsh1-1.96*seh1,1:17,effectsh1+1.96*seh1,1:17,lwd=2)
segments(effectsh1-1.645*seh1,1:17,effectsh1+1.645*seh1,1:17,lwd=4)
abline(v=0,lty=3)
axis(2,at=c(1:17),labels=c("boots","no boots","terror","ally","unspecified","old","young","not white","white","female","male","no degree","degree","moderate","liberal","conservative","total"),cex.axis=1.4,las=1)
mtext("Marginal Effect of Round 1 Long Message",side=3,cex=1.4,line=1.5)
mtext("on Initial Approval",side=3,cex=1.4,line=0.3)
mtext("Percentage Point Change in Public Approval",side=1,cex=1.1,line=2.3)
axis(1,at=seq(-0.4,0,by=0.1),labels=c("-40","-30","-20","-10","0"),cex.axis=1.3)


#### Figure A2

# partisan
avg<-(lm(r2approvalexclude~r1long, data=data[data$longwar==1,]))
cons<-(lm(r2approvalexclude~r1long, data=data[data$cons==1& data$longwar==1,]))
lib<-(lm(r2approvalexclude~r1long, data=data[data$lib==1& data$longwar==1,]))
mod<-(lm(r2approvalexclude~r1long, data=data[data$mod==1& data$longwar==1,]))

# education
degree<-(lm(r2approvalexclude~r1long, data=data[data$fyrdegree ==1& data$longwar==1,]))
nodegree<-(lm(r2approvalexclude~r1long, data=data[data$nfyrdegree ==1& data$longwar==1,]))

# gender
male<-(lm(r2approvalexclude~r1long, data=data[data$gender ==1& data$longwar==1,]))
female<-(lm(r2approvalexclude~r1long, data=data[data$gender ==2& data$longwar==1,]))

# race
white<-(lm(r2approvalexclude~r1long, data=data[data$race ==1& data$longwar==1,]))
nowhite<-(lm(r2approvalexclude~r1long, data=data[data$race>1& data$longwar==1,]))  #pooled all other races (small n)

# age
u31<-(lm(r2approvalexclude~r1long, data=data[data$birthyr >1984& data$longwar==1,]))
o31<-(lm(r2approvalexclude~r1long, data=data[data$birthyr<=1984& data$longwar==1,])) 

# enemy
enemyno<-(lm(r2approvalexclude~r1long, data=data[data$enemy==1& data$longwar==1,]))
enemyally<-(lm(r2approvalexclude~r1long, data=data[data$enemy==2& data$longwar==1,]))
enemyterror<-(lm(r2approvalexclude~r1long, data=data[data$enemy==3& data$longwar==1,]))

# boots
noboots<-(lm(r2approvalexclude~r1long, data=data[data$boots==0& data$longwar==1,]))
boots<-(lm(r2approvalexclude~r1long, data=data[data$boots==1& data$longwar==1,]))

# all together
effectsh2<-c(summary(avg)$coefficients[,1][2],summary(cons)$coefficients[,1][2],summary(lib)$coefficients[,1][2],summary(mod)$coefficients[,1][2],summary(degree)$coefficients[,1][2],summary(nodegree)$coefficients[,1][2],summary(male)$coefficients[,1][2],summary(female)$coefficients[,1][2],summary(white)$coefficients[,1][2],summary(nowhite)$coefficients[,1][2],summary(u31)$coefficients[,1][2],summary(o31)$coefficients[,1][2],summary(enemyno)$coefficients[,1][2],summary(enemyally)$coefficients[,1][2],summary(enemyterror)$coefficients[,1][2],summary(noboots)$coefficients[,1][2],summary(boots)$coefficients[,1][2])
length(effectsh1)
seh2<-c(summary(avg)$coefficients[,2][2],summary(cons)$coefficients[,2][2],summary(lib)$coefficients[,2][2],summary(mod)$coefficients[,2][2],summary(degree)$coefficients[,2][2],summary(nodegree)$coefficients[,2][2],summary(male)$coefficients[,2][2],summary(female)$coefficients[,2][2],summary(white)$coefficients[,2][2],summary(nowhite)$coefficients[,2][2],summary(u31)$coefficients[,2][2],summary(o31)$coefficients[,2][2],summary(enemyno)$coefficients[,2][2],summary(enemyally)$coefficients[,2][2],summary(enemyterror)$coefficients[,2][2],summary(noboots)$coefficients[,2][2],summary(boots)$coefficients[,2][2])

effectsh2<-rev(effectsh2)
seh2<-rev(seh2)

par(oma=c(1,7,2,1))
par(mar=c(3,1,1,1))
plot(effectsh2,1:17,xlim=c(-.05,.35),cex=1.2,pch=19,yaxt="n",ylab="",xaxt="n")
segments(effectsh2-1.96*seh2,1:17,effectsh2+1.96*seh2,1:17,lwd=2)
segments(effectsh2-1.645*seh2,1:17,effectsh2+1.645*seh2,1:17,lwd=4)
abline(v=0,lty=3)
axis(2,at=c(1:17),labels=c("boots","no boots","terror","ally","unspecified","old","young","not white","white","female","male","no degree","degree","moderate","liberal","conservative","total"),cex.axis=1.4,las=1)
mtext("Marginal Effect of Round 1 Long Message",side=3,cex=1.4,line=1.5)
mtext("on Round 2 Approval",side=3,cex=1.4,line=0.3)
mtext("Percentage Point Change in Public Approval",side=1,cex=1.1,line=2.3)
axis(1,at=seq(0,0.3,by=0.1),labels=c("0","10","20","30"),cex.axis=1.3)


#### Tables A1-A6 

# Table A1
a11<-lm(r1approvalexclude ~ r1long, data=data)
a12<-lm(r1approvalexclude ~ r1long + Ideology + education +  gender + birthyr + race, data=data)
a13<-glm(r1approvalexclude ~ r1long, data=data, family="binomial")
a14<-glm(r1approvalexclude ~ r1long + Ideology + education +  gender + birthyr + race, data=data, family="binomial")

summary(a11)
summary(a12)
summary(a13)
summary(a14)

# Table A2
a21<-lm(r1approvalexclude ~ r1long + Ideology + education + gender + birthyr + race, data=data)
a22<-lm(r1approvalbinary ~ r1long + Ideology + education + gender + birthyr + race, data=data)
a23<-lm(r1disapprovalbinary ~ r1long + Ideology + education + gender + birthyr + race, data=data)
a24<-lm(r1approvalordered ~ r1long + Ideology + education + gender + birthyr + race, data=data)

summary(a21)
summary(a22)
summary(a23)
summary(a24)

# Table A3
a31<-lm(r2approvalexclude ~ r1long, data=data[data$longwar==1,])
a32<-lm(r2approvalexclude ~ r1long + Ideology + education + gender + birthyr + race, data=data[data$longwar==1,])
a33<-glm(r2approvalexclude ~ r1long, data=data[data$longwar==1,], family="binomial")
a34<-glm(r2approvalexclude ~ r1long + Ideology + education + gender + birthyr + race, data=data[data$longwar==1,], family="binomial")

summary(a31)
summary(a32)
summary(a33)
summary(a34)


# Table A4
a41<-lm(r2approvalexclude ~ r1long + Ideology + education + gender + birthyr + race, data=data[data$longwar==1,])
a42<-lm(r2approvalbinary ~ r1long + Ideology + education + gender + birthyr + race, data=data[data$longwar==1,])
a43<-lm(r2disapprovalbinary ~ r1long + Ideology + education + gender + birthyr + race, data=data[data$longwar==1,])
a44<-lm(r2approvalordered ~ r1long + Ideology + education + gender + birthyr + race, data=data[data$longwar==1,])

summary(a41)
summary(a42)
summary(a43)
summary(a44)


# Table A5
a51<-lm(r3approvalexclude ~ sss, data=data[data$longwar==1 & data$r1long==0 & data$r2long==0,])
a52<-lm(r3approvalexclude ~ sss + Ideology + education + gender + birthyr + race, data=data[data$longwar==1 & data$r1long==0 & data$r2long==0,])
a53<-glm(r3approvalexclude ~ sss, data=data[data$longwar==1 & data$r1long==0 & data$r2long==0,], family="binomial")
a54<-glm(r3approvalexclude ~ sss + Ideology + education + gender + birthyr + race, data=data[data$longwar==1 & data$r1long==0 & data$r2long==0,], family="binomial")

summary(a51)
summary(a52)
summary(a53)
summary(a54)


# Table A6
a61<-lm(r3approvalexclude ~ sss + Ideology + education + gender + birthyr + race, data=data[data$longwar==1 & data$r1long==0 & data$r2long==0,])
a62<-lm(r3approvalbinary ~ sss + Ideology + education + gender + birthyr + race, data=data[data$longwar==1 & data$r1long==0 & data$r2long==0,])
a63<-lm(r3disapprovalbinary ~ sss + Ideology + education + gender + birthyr + race, data=data[data$longwar==1 & data$r1long==0 & data$r2long==0,])
a64<-lm(r3approvalordered ~ sss + Ideology + education + gender + birthyr + race, data=data[data$longwar==1 & data$r1long==0 & data$r2long==0,])

summary(a61)
summary(a62)
summary(a63)
summary(a64)


#### Figure A5

correctr2short<-lm(rfapprovalexclude ~r1long,data=data[data$longwar==0,])   
summary(correctr2short) 
effect3s<-summary(correctr2short)$coefficients[,1][2]
se3s<-summary(correctr2short)$coefficients[,2][2]

par(mfrow=c(1,1))
par(oma=c(3,2,3,1))
plot(-effect3s,1,cex=3,pch=20,xlim=c(-0.2,0.2), yaxt="n",ylab="",ylim=c(0.5,1.5),xlab="",xaxt="n")
segments((-effect3s-1.96*se3s),1,(-effect3s+1.96*se3s),1,lwd=4)
abline(v=0,lty=3)
mtext("Percentage Point Change in Public Approval",side=1,cex=1.1,line=2.3)
text(0.06,0.58,"baseline=85%")
mtext("Short War: Marginal Effect of Initial Message",side=3,cex=1.4,line=1.5)
mtext("Being Correct on Subsequent Approval",side=3,cex=1.4,line=0.3)
axis(side=2, at=1,labels="Final Approval",cex.axis=1.2)
axis(1,at=c(seq(-0.2,0.2,by=0.1)),labels=c("-20", "-10","0","10","20"))


#### Result described in 3.4 

longwar<-lm(rfapprovalexclude~longwar, data=data)
summary(longwar)










###################
### (II) Mechanism survey results
###################

### Set working directory and load the data
library(foreign)
library(mediation)


########
## (II.1) Manuscript


#### Figure 3

mech1 <- read.dta("HuffSchub_InterTemporal_Mechanism1.dta")
head(mech1)

# First Stage: Mediator ~ Treatment
competent<-(lm(competent~r1long,data=mech1))  
optimistic<-(lm(optimistic~r1long,data=mech1)) 
honest<-(lm(honest~r1long,data=mech1)) 
reckless<-(lm(reckless~r1long,data=mech1)) 
weak<-(lm(weak~r1long,data=mech1)) 
costly<-(lm(costly~r1long,data=mech1)) 
victory<-(lm(victory~r1long,data=mech1)) 
preseffects<-c(summary(competent)$coefficients[,1][2],summary(optimistic)$coefficients[,1][2],summary(honest)$coefficients[,1][2],summary(reckless)$coefficients[,1][2],summary(weak)$coefficients[,1][2])
presse<-c(summary(competent)$coefficients[,2][2],summary(optimistic)$coefficients[,2][2],summary(honest)$coefficients[,2][2],summary(reckless)$coefficients[,2][2],summary(weak)$coefficients[,2][2])
traits<-seq(1:5)
conflicteffects<-c(summary(costly)$coefficients[,1][2],summary(victory)$coefficients[,1][2])
conflictse<-c(summary(costly)$coefficients[,2][2],summary(victory)$coefficients[,2][2])
conflict<-c(1,2)
all<-rev(c(preseffects,conflicteffects))
allse<-rev(c(presse,conflictse))
tot<-seq(1:7)

# Second Stage: Outcome ~ Mediator
stage2<-lm(r1approvalexclude~r1long + competent + optimistic + honest + reckless + weak + costly + victory,data=mech1[mech1$approvalresponse==1,])
preseff<-summary(stage2)$coefficients[3:7,1]
presse2<-summary(stage2)$coefficients[3:7,2]
conflicteff<-summary(stage2)$coefficients[8:9,1]
conflictse2<-summary(stage2)$coefficients[8:9,2]
all2<-rev(c(preseff,conflicteff))
allse2<-rev(c(presse2,conflictse2))

# Mediation Analysis
honest.m<-lm(honest~r1long,data=mech1[mech1$approvalresponse==1,])
honest.o<-lm(r1approvalexclude~r1long+honest,data=mech1)
honest.out<-mediate(honest.m,honest.o,treat="r1long", mediator="honest")
optimistic.m<-lm(optimistic ~r1long,data=mech1[mech1$approvalresponse==1,])
optimistic.o<-lm(r1approvalexclude~r1long+ optimistic,data=mech1)
optimistic.out<-mediate(optimistic.m, optimistic.o,treat="r1long", mediator="optimistic")
reckless.m<-lm(reckless ~r1long,data=mech1[mech1$approvalresponse==1,])
reckless.o<-lm(r1approvalexclude~r1long+ reckless,data=mech1)
reckless.out<-mediate(reckless.m, reckless.o,treat="r1long", mediator="reckless")
weak.m<-lm(weak ~r1long,data=mech1[mech1$approvalresponse==1,])
weak.o<-lm(r1approvalexclude~r1long+ weak,data=mech1)
weak.out<-mediate(weak.m, weak.o,treat="r1long", mediator="weak")
competent.m<-lm(competent ~r1long,data=mech1[mech1$approvalresponse==1,])
competent.o<-lm(r1approvalexclude~r1long+ competent,data=mech1)
competent.out<-mediate(competent.m, competent.o,treat="r1long", mediator="competent")
costly.m<-lm(costly ~r1long,data=mech1[mech1$approvalresponse==1,])
costly.o<-lm(r1approvalexclude~r1long+ costly,data=mech1)
costly.out<-mediate(costly.m, costly.o,treat="r1long", mediator="costly")
victory.m<-lm(victory ~r1long,data=mech1[mech1$approvalresponse==1,])
victory.o<-lm(r1approvalexclude~r1long+ victory,data=mech1)
victory.out<-mediate(victory.m, victory.o,treat="r1long", mediator="victory")
presmed<-c(summary(competent.out)$n.avg,summary(optimistic.out)$n.avg,summary(honest.out)$n.avg,summary(reckless.out)$n.avg,summary(weak.out)$n.avg)
presmedlb<-c(summary(competent.out)$n.avg.ci[1],summary(optimistic.out)$n.avg.ci[1],summary(honest.out)$n.avg.ci[1],summary(reckless.out)$n.avg.ci[1],summary(weak.out)$n.avg.ci[1])
presmedub<-c(summary(competent.out)$n.avg.ci[2],summary(optimistic.out)$n.avg.ci[2],summary(honest.out)$n.avg.ci[2],summary(reckless.out)$n.avg.ci[2],summary(weak.out)$n.avg.ci[2])
conflictmed<-c(summary(costly.out)$n.avg,summary(victory.out)$n.avg)
conflictmedlb<-c(summary(costly.out)$n.avg.ci[1],summary(victory.out)$n.avg.ci[1])
conflictmedub<-c(summary(costly.out)$n.avg.ci[2],summary(victory.out)$n.avg.ci[2])
allmed<-rev(c(presmed,conflictmed))
alllb<-rev(c(presmedlb,conflictmedlb))
allub<-rev(c(presmedub,conflictmedub))
tot<-seq(1:7)

# plot
par(mfrow=c(1,3))
par(oma=c(1,6,2,1))
par(mar=c(3,1,1,1))
plot(all[3:7],tot[3:7],pch=19,cex=1.2,ylim=c(0.5,7.5),xlim=c(-1.5,1.5),yaxt="n",ylab="",cex.axis=1.3)
segments(all[3:7]-1.96*allse[3:7],tot[3:7],all[3:7]+1.96*allse[3:7],tot[3:7],lwd=4)
points(all[1:2],tot[1:2],pch=19,cex=1.2,col="grey70")
segments(all[1:2]-1.96*allse[1:2],tot[1:2],all[1:2]+1.96*allse[1:2],tot[1:2],lwd=4,col="grey70")
abline(v=0,lty=3)
axis(2,at=c(3:7),labels=c("weak","reckless","honest","optimistic","competent"),cex.axis=1.4,las=1)
axis(2,at=c(1:2),labels=c("victory","costly"),cex.axis=1.4,las=1,col.axis="grey50")
mtext("Change in Perception",side=1,cex=1.2,line=2.8,outer=F)
mtext("Effect of 'Long'",side=3,cex=1.4,line=1.5,outer=F)
mtext("on Mediator",side=3,cex=1.4,line=0,outer=F)
par(mar=c(3,1,1,1))
plot(all2[3:7],tot[3:7],pch=19,cex=1.2,ylim=c(0.5,7.5),xlim=c(-.4,.4),yaxt="n",ylab="",cex.axis=1.3,xaxt="n")
segments(all2[3:7]-1.96*allse2[3:7],tot[3:7],all2[3:7]+1.96*allse2[3:7],tot[3:7],lwd=4)
points(all2[1:2],tot[1:2],pch=19,cex=1.2,col="grey70")
segments(all2[1:2]-1.96*allse2[1:2],tot[1:2],all2[1:2]+1.96*allse2[1:2],tot[1:2],lwd=4,col="grey70")
abline(v=0,lty=3)
mtext("Change in Approval (%)",side=1,cex=1.2,line=2.8,outer=F)
mtext("Effect of Mediator",side=3,cex=1.4,line=1.5,outer=F)
mtext("on Outcome",side=3,cex=1.4,line=0,outer=F)
axis(1,at=seq(-0.4,0.4,by=0.2),labels=c("-40","-20","0","20","40"),cex.axis=1.3)
par(mar=c(3,1,1,1))
plot(allmed[3:7],tot[3:7],pch=19,cex=1.2,ylim=c(0.5,7.5),xlim=c(-1.5,1.5),yaxt="n",ylab="",cex.axis=1.3,xaxt="n")
segments(alllb[3:7],tot[3:7],allub[3:7],tot[3:7],lwd=4)
points(allmed[1:2],tot[1:2],pch=19,cex=1.2,col="grey70")
segments(alllb[1:2],tot[1:2],allub[1:2],tot[1:2],lwd=4,col="grey70")
abline(v=0,lty=3)
mtext("Proportion Mediated (%)",side=1,cex=1.2,line=2.8,outer=F)
mtext("Estimate of",side=3,cex=1.4,line=1.5,outer=F)
mtext("Mediation",side=3,cex=1.4,line=0,outer=F)
axis(1,at=seq(-1.5,1.5,by=0.75),labels=c("-150","-75","0","75","150"),cex.axis=1.3)



#### Figure 4

rm(list=ls())
mech2 <- read.dta("HuffSchub_InterTemporal_Mechanism2.dta")
head(mech2)

# First Stage: mechanism ~ treatment
honest<-(lm(honest~r1long,data=mech2)) 
reckless<-(lm(reckless~r1long,data= mech2)) 
weak<-(lm(weak~r1long,data= mech2)) 
optimistic<-(lm(optimistic~r1long,data= mech2)) 
competent<-(lm(competent~r1long,data= mech2))
costly<-(lm(costly~r1long,data= mech2)) 
victory<-(lm(victory~r1long,data= mech2))

preseffects<-c(summary(competent)$coefficients[,1][2],summary(optimistic)$coefficients[,1][2],summary(honest)$coefficients[,1][2],summary(reckless)$coefficients[,1][2],summary(weak)$coefficients[,1][2])
presse<-c(summary(competent)$coefficients[,2][2],summary(optimistic)$coefficients[,2][2],summary(honest)$coefficients[,2][2],summary(reckless)$coefficients[,2][2],summary(weak)$coefficients[,2][2])
traits<-seq(1:5)
conflicteffects<-c(summary(costly)$coefficients[,1][2],summary(victory)$coefficients[,1][2])
conflictse<-c(summary(costly)$coefficients[,2][2],summary(victory)$coefficients[,2][2])
conflict<-c(1,2)
all<-rev(c(preseffects,conflicteffects))
allse<-rev(c(presse,conflictse))
tot<-seq(1:7)

# Second Stage: outcome ~ mediator
stage2<-lm(r2approvalexclude~r1long + competent + optimistic + honest + reckless + weak + costly + victory,data= mech2[mech2 $approvalresponse1==1,])
preseff<-summary(stage2)$coefficients[3:7,1]
presse2<-summary(stage2)$coefficients[3:7,2]
conflicteff<-summary(stage2)$coefficients[8:9,1]
conflictse2<-summary(stage2)$coefficients[8:9,2]
all2<-rev(c(preseff,conflicteff))
allse2<-rev(c(presse2,conflictse2))
tot<-seq(1:7)

# Mediation Analysis
#honest
honest.m<-lm(honest~r1long,data=mech2[mech2 $approvalresponse==1,])
honest.o<-lm(r2approvalexclude~r1long+honest,data= mech2)
honest.out<-mediate(honest.m,honest.o,treat="r1long", mediator="honest")
#optimistic
optimistic.m<-lm(optimistic ~r1long,data= mech2[mech2 $approvalresponse==1,])
optimistic.o<-lm(r2approvalexclude~r1long+ optimistic,data= mech2)
optimistic.out<-mediate(optimistic.m, optimistic.o,treat="r1long", mediator="optimistic")
#reckless
reckless.m<-lm(reckless ~r1long,data= mech2[mech2 $approvalresponse==1,])
reckless.o<-lm(r2approvalexclude~r1long+ reckless,data= mech2)
reckless.out<-mediate(reckless.m, reckless.o,treat="r1long", mediator="reckless")
#weak
weak.m<-lm(weak ~r1long,data= mech2[mech2 $approvalresponse==1,])
weak.o<-lm(r2approvalexclude~r1long+ weak,data= mech2)
weak.out<-mediate(weak.m, weak.o,treat="r1long", mediator="weak")
#competent
competent.m<-lm(competent ~r1long,data= mech2[mech2 $approvalresponse==1,])
competent.o<-lm(r2approvalexclude~r1long+ competent,data= mech2)
competent.out<-mediate(competent.m, competent.o,treat="r1long", mediator="competent")
#costly
costly.m<-lm(costly ~r1long,data= mech2[mech2 $approvalresponse==1,])
costly.o<-lm(r2approvalexclude~r1long+ costly,data= mech2)
costly.out<-mediate(costly.m, costly.o,treat="r1long", mediator="costly")
#victory
victory.m<-lm(victory ~r1long,data= mech2[mech2 $approvalresponse==1,])
victory.o<-lm(r2approvalexclude~r1long+ victory,data= mech2)
victory.out<-mediate(victory.m, victory.o,treat="r1long", mediator="victory")

presmed<-c(summary(competent.out)$n.avg,summary(optimistic.out)$n.avg,summary(honest.out)$n.avg,summary(reckless.out)$n.avg,summary(weak.out)$n.avg)
presmedlb<-c(summary(competent.out)$n.avg.ci[1],summary(optimistic.out)$n.avg.ci[1],summary(honest.out)$n.avg.ci[1],summary(reckless.out)$n.avg.ci[1],summary(weak.out)$n.avg.ci[1])
presmedub<-c(summary(competent.out)$n.avg.ci[2],summary(optimistic.out)$n.avg.ci[2],summary(honest.out)$n.avg.ci[2],summary(reckless.out)$n.avg.ci[2],summary(weak.out)$n.avg.ci[2])
conflictmed<-c(summary(costly.out)$n.avg,summary(victory.out)$n.avg)
conflictmedlb<-c(summary(costly.out)$n.avg.ci[1],summary(victory.out)$n.avg.ci[1])
conflictmedub<-c(summary(costly.out)$n.avg.ci[2],summary(victory.out)$n.avg.ci[2])
allmed<-rev(c(presmed,conflictmed))
alllb<-rev(c(presmedlb,conflictmedlb))
allub<-rev(c(presmedub,conflictmedub))
tot<-seq(1:7)

# Plot
par(mfrow=c(1,3))
par(oma=c(1,6,2,1))
par(mar=c(3,1,1,1))
plot(all[3:7],tot[3:7],pch=19,cex=1.2,ylim=c(0.5,7.5),xlim=c(-1.5,1.5),yaxt="n",ylab="",cex.axis=1.3)
segments(all[3:7]-1.96*allse[3:7],tot[3:7],all[3:7]+1.96*allse[3:7],tot[3:7],lwd=4)
points(all[1:2],tot[1:2],pch=19,cex=1.2,col="grey70")
segments(all[1:2]-1.96*allse[1:2],tot[1:2],all[1:2]+1.96*allse[1:2],tot[1:2],lwd=4,col="grey70")
abline(v=0,lty=3)
axis(2,at=c(3:7),labels=c("weak","reckless","honest","optimistic","competent"),cex.axis=1.2,las=1)
axis(2,at=c(1:2),labels=c("victory","costly"),cex.axis=1.2,las=1,col.axis="grey50")
mtext("Change in Perception",side=1,cex=1.2,line=2.8,outer=F)
mtext("Effect of 'Accuracy'",side=3,cex=1.4,line=1.5,outer=F)
mtext("on Mediator",side=3,cex=1.4,line=0,outer=F)
par(mar=c(3,1,1,1))
plot(all2[3:7],tot[3:7],pch=19,cex=1.2,ylim=c(0.5,7.5),xlim=c(-.4,.4),yaxt="n",ylab="",cex.axis=1.3,xaxt="n")
segments(all2[3:7]-1.96*allse2[3:7],tot[3:7],all2[3:7]+1.96*allse2[3:7],tot[3:7],lwd=4)
points(all2[1:2],tot[1:2],pch=19,cex=1.2,col="grey70")
segments(all2[1:2]-1.96*allse2[1:2],tot[1:2],all2[1:2]+1.96*allse2[1:2],tot[1:2],lwd=4,col="grey70")
abline(v=0,lty=3)
mtext("Change in Approval (%)",side=1,cex=1.2,line=2.8,outer=F)
axis(1,at=seq(-0.4,0.4,by=0.2),labels=c("-40","-20","0","20","40"),cex.axis=1.3)
mtext("Effect of Mediator",side=3,cex=1.4,line=1.5,outer=F)
mtext("on Outcome",side=3,cex=1.4,line=0,outer=F)
par(mar=c(3,1,1,1))
plot(allmed[3:7],tot[3:7],pch=19,cex=1.2,ylim=c(0.5,7.5),xlim=c(-1,1.65),yaxt="n",ylab="",cex.axis=1.3,xaxt="n")
segments(alllb[3:7],tot[3:7],allub[3:7],tot[3:7],lwd=4)
points(allmed[1:2],tot[1:2],pch=19,cex=1.2,col="grey70")
segments(alllb[1:2],tot[1:2],allub[1:2],tot[1:2],lwd=4,col="grey70")
abline(v=0,lty=3)
mtext("Proportion Mediated (%)",side=1,cex=1.2,line=2.8,outer=F)
mtext("Estimate of",side=3,cex=1.4,line=1.5,outer=F)
mtext("Mediation",side=3,cex=1.4,line=0,outer=F)
axis(1,at=seq(-1,1.5,by=0.5),labels=c("-100","-50","0","50","100","150"),cex.axis=1.3)




########
## (II.2) Supporting Files


#### Figure A6

honest<-(lm(honest~r2long,mech2[mech2$approvalresponse==1 & mech2$r1long==0,])) 
competent<-(lm(competent~r2long,mech2[mech2$approvalresponse==1 & mech2$r1long==0,]))
optimistic<-(lm(optimistic~r2long,mech2[mech2$approvalresponse==1 & mech2$r1long==0,])) 
reckless<-(lm(reckless~r2long,mech2[mech2$approvalresponse==1 & mech2$r1long==0,])) 
weak<-(lm(weak~r2long,mech2[mech2$approvalresponse==1 & mech2$r1long==0,])) 
costly<-(lm(costly~r2long,mech2[mech2$approvalresponse==1 & mech2$r1long==0,]))
victory<-(lm(victory~r2long,mech2[mech2$approvalresponse==1 & mech2$r1long==0,])) 

preseffects<-c(summary(competent)$coefficients[,1][2],summary(optimistic)$coefficients[,1][2],summary(honest)$coefficients[,1][2],summary(reckless)$coefficients[,1][2],summary(weak)$coefficients[,1][2])
presse<-c(summary(competent)$coefficients[,2][2],summary(optimistic)$coefficients[,2][2],summary(honest)$coefficients[,2][2],summary(reckless)$coefficients[,2][2],summary(weak)$coefficients[,2][2])
traits<-seq(1:5)
conflicteffects<-c(summary(costly)$coefficients[,1][2],summary(victory)$coefficients[,1][2])
conflictse<-c(summary(costly)$coefficients[,2][2],summary(victory)$coefficients[,2][2])
conflict<-c(1,2)
all<-rev(c(preseffects,conflicteffects))
allse<-rev(c(presse,conflictse))
tot<-seq(1:7)

# Plot
par(mfrow=c(1,1))
par(oma=c(1,4,1,1))
par(mar=c(3,2,1,1))
plot(all[3:7],tot[3:7],pch=19,cex=2,ylim=c(0.5,7.5),xlim=c(-1.5,1.5),yaxt="n",ylab="")
segments(all[3:7]-1.96*allse[3:7],tot[3:7],all[3:7]+1.96*allse[3:7],tot[3:7],lwd=4)
points(all[1:2],tot[1:2],pch=19,cex=2,col="grey40")
segments(all[1:2]-1.96*allse[1:2],tot[1:2],all[1:2]+1.96*allse[1:2],tot[1:2],lwd=4,col="grey40")
abline(v=0,lty=3)
axis(2,at=c(3:7),labels=c("weak","reckless","honest","optimistic","competent"),cex.axis=1.2,las=1)
axis(2,at=c(1:2),labels=c("victory","costly"),cex.axis=1.2,las=1,col.axis="grey40")
mtext("Change in Respondent Perception",side=1,cex=1.2,line=2.3,outer=F)
mtext("Marginal Effect of Switching after Initial Inaccuracy",side=3,cex=1.4,line=-0.5,outer=T)









###################
### (III) Partisanship survey results
###################


########
## (IIII.1) Supporting Files
library(foreign)
rm(list=ls())
data<-read.dta("HuffSchub_InterTemporal_Partisan.dta")


#### Figure A3

mobil<-lm(r1approvalexclude~r1long + gender + birthyr + Ideology + education + race,data=data)
summary(mobil) 
effect2<-summary(mobil)$coefficients[,1][2]
se2<-summary(mobil)$coefficients[,2][2]

mobilco<-lm(r1approvalexclude~r1long + gender + birthyr + Ideology + education + race,data=data[data$copartisanstrict==1,])
effectco2<-summary(mobilco)$coefficients[,1][2]
seco2<-summary(mobilco)$coefficients[,2][2]

mobilnoco<-lm(r1approvalexclude~r1long + gender + birthyr + Ideology + education + race,data=data[data$copartisanstrict==0,])
effectnoco2<-summary(mobilnoco)$coefficients[,1][2]
senoco2<-summary(mobilnoco)$coefficients[,2][2]

vert<-c(1:3)
eff<-c(effectnoco2,effectco2,effect2)
ses<-c(senoco2,seco2,se2)

par(mfrow=c(1,1))
par(oma=c(1,1,1,1))
par(mar=c(3,7,3,1))
plot(eff,vert,cex=3,pch=20,xlim=c(-.4,0.05),ylim=c(0,4),yaxt="n",ylab="",xaxt="n")
segments((eff-1.96*ses),vert,(eff+1.96*ses),vert,lwd=4)
abline(v=0,lty=3)
axis(1,at=c(seq(-.4,0.05,by=0.05)),labels=c("-40", "-35", "-30","-25","-20","-15","-10","-5","0","5"))
axis(2,at=c(1:3),labels=c("non-co-partisans","co-partisans","all"),las=1)
mtext("Marginal Effect of Round 1 Long Message",side=3,cex=1.4,line=1.5)
mtext("on Initial Approval",side=3,cex=1.4,line=0.3)
mtext("Percentage Point Change in Public Approval",side=1,cex=1.1,line=2.3)
text(-0.05,2.75,"baseline=60%")
text(-0.05,1.75,"baseline=80%")
text(-0.05,0.75,"baseline=49%")



#### Figure A4

## w/o partisanship
 # (a) Effect on R2 approval
correctr2<-lm(r2approvalexclude~r1long + gender + birthyr + Ideology + education + race,data=data)
effectr2<-summary(correctr2)$coefficients[,1][2]
ser2<-summary(correctr2)$coefficients[,2][2]

 # (b) Effect on R3 approval
correctr3<-lm(r3approvalexclude~r1long + gender + birthyr + Ideology + education + race,data=data)
effectr3<-summary(correctr3)$coefficients[,1][2]
ser3<-summary(correctr3)$coefficients[,2][2]

 # (c) Effect on RF approval
correctrf<-lm(rfapprovalexclude~r1long + gender + birthyr + Ideology + education + race,data=data[data$longwar==1,])
effectrf<-summary(correctrf)$coefficients[,1][2]
serf<-summary(correctrf)$coefficients[,2][2]


## w/ partisanship
 # (a) Effect on R2 approval
correctcor2<-lm(r2approvalexclude ~r1long + gender + birthyr + Ideology + education + race,data=data[data$copartisanstrict==1,])
correctnocor2<-lm(r2approvalexclude ~r1long + gender + birthyr + Ideology + education + race,data=data[data$copartisanstrict==0,])

effectcor2<-summary(correctcor2)$coefficients[,1][2]
secor2<-summary(correctcor2)$coefficients[,2][2]
effectnocor2<-summary(correctnocor2)$coefficients[,1][2]
senocor2<-summary(correctnocor2)$coefficients[,2][2]

 # (b) Effect on R3 approval
correctcor3<-lm(r3approvalexclude ~r1long + gender + birthyr + Ideology + education + race,data=data[data$copartisanstrict==1,])
correctnocor3<-lm(r3approvalexclude ~r1long + gender + birthyr + Ideology + education + race,data=data[data$copartisanstrict==0,])

effectcor3<-summary(correctcor3)$coefficients[,1][2]
secor3<-summary(correctcor3)$coefficients[,2][2]
effectnocor3<-summary(correctnocor3)$coefficients[,1][2]
senocor3<-summary(correctnocor3)$coefficients[,2][2]

 # (c) Effect on RF approval
correctcorf<-lm(rfapprovalexclude ~r1long + gender + birthyr + Ideology + education + race,data=data[data$longwar==1 & data$copartisanstrict==1,])
correctnocorf<-lm(rfapprovalexclude ~r1long + gender + birthyr + Ideology + education + race,data=data[data$longwar==1 & data$copartisanstrict==0,])

effectcorf<-summary(correctcorf)$coefficients[,1][2]
secorf<-summary(correctcorf)$coefficients[,2][2]
effectnocorf<-summary(correctnocorf)$coefficients[,1][2]
senocorf<-summary(correctnocorf)$coefficients[,2][2]


## Upper panel: all respondents
effall<-c(effectrf,effectr3,effectr2)
seall<-c(serf,ser3,ser2)

par(mfrow=c(1,1))
par(oma=c(3,2,3,1))
par(mar=c(1,2,1,2))
plot(effall,vert,cex=3,pch=20,xlim=c(-0.15,0.4),yaxt="n",ylab="",ylim=c(0,4),xlab="",xaxt="n")
segments((effall-1.96*seall),vert,(effall+1.96*seall),vert,lwd=4)
abline(v=0,lty=3)
mtext("All Respondents: Marginal Effect of Initial Message",side=3,cex=1.4,line=1.5)
mtext("Being Correct on Subsequent Approval",side=3,cex=1.4,line=0.3)
axis(2,at=c(1:3),labels=c("Final","R3","R2"),cex.axis=1.2)
axis(1,at=c(seq(-0.1,0.4,by=0.1)),labels=c("-10","0","10","20","30","40"))
mtext("Percentage Point Change in Public Approval",side=1,cex=1.1,line=2.3)
text(-0.07,2.90,"baseline=16%")
text(-0.07,1.90,"baseline=10%")
text(-0.07,0.90,"baseline=13%")


## Middle panel: co-partisans
effcoall<-c(effectcorf,effectcor3,effectcor2)
secoall<-c(secorf,secor3,secor2)

par(mfrow=c(1,1))
par(oma=c(3,2,1,1))
par(mar=c(1,2,1,2))
plot(effcoall,vert,cex=3,pch=20,xlim=c(-0.15,0.4),yaxt="n",ylab="",ylim=c(0,4),xlab="",xaxt="n")
segments((effcoall-1.96*secoall),vert,(effcoall+1.96*secoall),vert,lwd=4)
abline(v=0,lty=3)
mtext("Co-Partisans",side=3,cex=1.4,line=0.3)
axis(2,at=c(1:3),labels=c("Final","R3","R2"),cex.axis=1.2)
axis(1,at=c(seq(-0.1,0.4,by=0.1)),labels=c("-10","0","10","20","30","40"))
mtext("Percentage Point Change in Public Approval",side=1,cex=1.1,line=2.3)
text(-0.07,2.9,"baseline=14%")
text(-0.07,1.9,"baseline=13%")
text(-0.07,0.9,"baseline=14%")


# Lower panel: non-co-partisans 
effnocoall<-c(effectnocorf,effectnocor3,effectnocor2)
senocoall<-c(senocorf,senocor3,senocor2)

par(mfrow=c(1,1))
par(oma=c(3,2,1,1))
par(mar=c(1,2,1,2))
plot(effnocoall,vert,cex=3,pch=20,xlim=c(-0.15,0.4),yaxt="n",ylab="",ylim=c(0,4),xlab="",xaxt="n")
segments((effnocoall-1.96*senocoall),vert,(effnocoall+1.96*senocoall),vert,lwd=4)
abline(v=0,lty=3)
mtext("Non-Co-Partisans",side=3,cex=1.4,line=0.3)
axis(2,at=c(1:3),labels=c("Final","R3","R2"),cex.axis=1.2)
axis(1,at=c(seq(-0.1,0.4,by=0.1)),labels=c("-10","0","10","20","30","40"))
mtext("Percentage Point Change in Public Approval",side=1,cex=1.1,line=2.3)
text(-0.07,2.90,"baseline=8%")
text(-0.07,1.90,"baseline=10%")
text(-0.07,0.90,"baseline=12%")






