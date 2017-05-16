# quickly go through the best options
library(ProjectTemplate)
load.project()

regd1 <- teenc %>%
  dplyr::select(loweduc,res_num,female,n_health,n_school,haswell,hand_pump,tubewell,app_pucca_road,bus_train,pcnt_irrig,fem_literacy ,
                pcnt_literat,sexratio_under6,hhsize.y,scst_share,households,AA0_2a,block,wishmidmineduc,higheducjob, marryover20, 
                wishhigheduc,sexist_ben,sexist_hos,educ,age) %>%
  filter(res_num!=1)

m1 <- lmer(educ~res_num*female+(1|block),data=regd1)
summary(m1)

m2 <- lmer(educ~res_num*female+(1|block)+(1|block:res_num)+(1|block:female),data=regd1)
summary(m2)

m3 <- lmer(educ~res_num*female+(1|block:res_num:female),data=regd1)
summary(m3)

m4 <- lmer(educ~res_num*female+(1+res_num*female|block),data=regd1)
summary(m4)

anova(m1,m2,m3,m4)

#this looks at the extremelys unbalanced design.

fm1 <- lmer(educ~res_num+(1|block),data=regd1[regd1$female==1,])
summary(fm1)
fm2 <- lmer(educ~res_num+(1|block:res_num),data=regd1[regd1$female==1,])
summary(fm2)
fm3 <- lmer(educ~res_num+(1+res_num|block),data=regd1[regd1$female==1,])
summary(fm3)
anova(fm1,fm2,fm3)

