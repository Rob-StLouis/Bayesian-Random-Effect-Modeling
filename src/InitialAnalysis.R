library(ProjectTemplate)
load.project()


#read in the data
load("cache/SimpleGaussianHurdleSpeedAge")
load("cache/SimpleGaussianHurdleSpeedNoPool")
load("cache/SimpleGaussianHurdleSpeedNoPoolRVar")
load("cache/SimpleGaussianHurdleSpeedHierarchicalRVar")
load("cache/BAS_model_vars")

#extract log likelihood
ll2 <- extract_log_lik(fit2)
ll3 <- extract_log_lik(fit3)
ll4 <- extract_log_lik(fit4)
ll5 <- extract_log_lik(fit5)

loo2 <- loo(ll2)
loo3 <- loo(ll3)
loo4 <- loo(ll4)
loo5 <- loo(ll5)

compare(loo2,loo3,loo4,loo5)
compare(loo4,loo5)
compare(loo3,loo5)
compare(loo2,loo5)

## look at the predictions
y_pred <- extract(fit5,pars="y_pred")
y_pred <- y_pred$y_pred
y_pred_means <- colMeans(y_pred)

y_ob <- c(y_zero,y_num)

#need to do this more systematically, but things look ok.
qplot(y_ob)
qplot(round(y_pred[900,]))

qplot(jitter(y_ob),y_pred_means)+geom_abline()+coord_cartesian(ylim=c(0,12),xlim=c(0,12))
qplot(jitter(y_ob),y_pred[900,])+geom_abline()+coord_cartesian(ylim=c(0,12),xlim=c(0,12))

sd(y_ob)
sd(round(y_pred[130,]))



#wrote helper function to get betas.
res_twice_pool <- extract_betas(fit5,1,"beta_t",colnames(district_dummys))
female_pool <- extract_betas(fit5,2,"beta_t",colnames(district_dummys))
res_twice_female_pool <- extract_betas(fit5,3,"beta_t",colnames(district_dummys))

names(res_twice_pool)[1] <- "res_twice_pool"
names(female_pool)[1] <- "female_pool"
names(res_twice_female_pool)[1] <- "res_twice_female_pool"

names(res_twice_pool)[2] <- "sdrt"
names(female_pool)[2] <- "sdf"
names(res_twice_female_pool)[2] <- "sdrtf"

res_twice_nopool <- extract_betas(fit4,1,"beta",colnames(district_dummys))
female_nopool <- extract_betas(fit4,2,"beta",colnames(district_dummys))
res_twice_female_nopool <- extract_betas(fit4,3,"beta",colnames(district_dummys))

names(res_twice_nopool)[1] <- "res_twice_nopool"
names(female_nopool)[1] <- "female_nopool"
names(res_twice_female_nopool)[1] <- "res_twice_female_nopool"

names(res_twice_nopool)[2] <- "sdrtnp"
names(female_nopool)[2] <- "sdfnp"
names(res_twice_female_nopool)[2] <- "sdrtfnp"



#look at consistency of predictions.
qplot(res_twice_female_nopool$res_twice_female_nopool,res_twice_female_pool$res_twice_female_pool)

compvar<- regd1 %>%
  group_by(block,female,res_num)%>%
  summarize(mean_educ = mean(educ,na.rm=T),
            se_educ = sd(educ,na.rm=T)/sqrt(n()),
            n = n())

compvar <-compvar %>%
  group_by(block,female)%>%
  summarize(mean_educ_dif= mean(mean_educ[res_num==2],na.rm=T)-mean(mean_educ[res_num==0],na.rm=T))

compvar <-compvar %>%
  group_by(block)%>%
  summarize(mean_educ_dif= mean(mean_educ_dif[female==1],na.rm=T)-mean(mean_educ_dif[female==0],na.rm=T))

compvar$exists <- !is.na(compvar$mean_educ_dif)

compvar %<>% left_join(res_twice_female_nopool,by="block")
compvar %<>% left_join(res_twice_female_pool,by="block")
compvar %<>% left_join(res_twice_pool,by="block")
compvar %<>% left_join(res_twice_nopool,by="block")
compvar %<>% left_join(female_pool,by="block")
compvar %<>% left_join(female_nopool,by="block")

#compare estimates across pooling strategies
qplot(res_twice_female_nopool,res_twice_female_pool,data=compvar,color=exists)+geom_abline()+coord_cartesian(ylim=c(-1,1.5),xlim=c(-1,1.5))
qplot(res_twice_nopool,res_twice_pool,data=compvar,color=exists)+geom_abline()+coord_cartesian(ylim=c(-1,1.5),xlim=c(-1,1.5))
qplot(female_nopool,female_pool,data=compvar,color=exists)+geom_abline()+coord_cartesian(ylim=c(-.5,1),xlim=c(-.5,1))
qplot(mean_educ_dif,res_twice_female_pool,data=compvar,color=exists)+geom_abline()+coord_cartesian(ylim=c(-1,1.5),xlim=c(-1,1.5))

table(regd1$female,regd1$block,regd1$res_num)

mean(compvar$res_twice_female_nopool[compvar$exists])
mean(compvar$res_twice_nopool[compvar$exists])

print(fit5,pars=c("Beta_Mu","Beta_Sigma"))


print(fit3,pars=c("beta"))
print(fit2,pars=c("beta"))
#very clearly shows this difference betwen the two models. 
print(fit5,pars=c("beta_age","beta_b_age"))
print(fit4,pars=c("beta_age","beta_b_age"))
print(fit3,pars=c("beta_age","beta_b_age"))


