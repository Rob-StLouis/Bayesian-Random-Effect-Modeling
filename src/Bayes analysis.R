library(ProjectTemplate)

load.project()

regd1 <- teenc %>%
  dplyr::select(loweduc,res_num,female,n_health,n_school,haswell,hand_pump,tubewell,app_pucca_road,bus_train,pcnt_irrig,fem_literacy ,
         pcnt_literat,sexratio_under6,hhsize.y,scst_share,households,AA0_2a,block,wishmidmineduc,higheducjob, marryover20, 
         wishhigheduc,sexist_ben,sexist_hos,educ,age) %>%
  na.omit() %>%
  mutate(res_num = as.numeric(as.character(res_num))) %>%
  filter(res_num!=1)%>%
  mutate(res_num  = as.factor(res_num))


#set up comparison set, so we can look at the blocks.
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



#build model matrix
dummyvars <-model.matrix(~regd1$res_num*regd1$female-1)

#run a set of simulations
# res_hold <- SimpleGaussianHurdleSim(2,dummyvars)

colMeans(dummyvars)

model_frame <- dummyvars
#re-paramaterize to speed up calculations
ind <- sort.int(regd1$educ,index.return = T)
y_sim <- ind$x
num_zeros <- sum(y_sim==0)
#this is silly, I just like being explicit
y_zero <- y_sim[1:num_zeros]
y_num <- y_sim[ (num_zeros+1) :nrow(model_frame)]/10

#need to do the same re-ordering for the model frame
model_temp <- model_frame[ind$ix,2:4]
model_zero <- model_temp[1:num_zeros,]
model_num <- model_temp[(num_zeros+1) :nrow(model_frame),]

educ_data_simple <- list(
  NZ=num_zeros,
  NN = nrow(model_frame)-num_zeros,
  K = 3,
  #we do this because stan works better with normalized variables, this brings most values into the 0,1 range
  y_num = y_num,
  y_zero = y_zero,
  x_num = model_num,
  x_zero= model_zero
)

fit1 <- stan(
  file = "lib/SimpleGaussianHurdleSpeed.stan",  # Stan program
  data = educ_data_simple,    # named list of data
  chains = 2,             # number of Markov chains
  warmup = 250,          # number of warmup iterations per chain
  iter = 1250,            # total number of iterations per chain
  cores = 2,              # number of cores (using 1 just for the vignette)
  refresh = 100          # show progress every 'refresh' iterations
)
 save(fit1,file="cache/SimpleGaussian")
# load(file="cache/SimpleGaussian")

# print(fit1,pars=c("alpha","beta","sigma","alpha_b","beta_b"),digits_summary = 4)
# plot(fit1,pars=c("alpha","beta","alpha_b","beta_b"))
# 
# pairs(fit1,pars=c("beta"))



#re-paramaterize to speed up calculations
ind <- sort.int(regd1$educ,index.return = T)
y_sim <- ind$x
num_zeros <- sum(y_sim==0)
#this is silly, I just like being explicit
y_zero <- y_sim[1:num_zeros]
y_num <- y_sim[ (num_zeros+1) :nrow(model_frame)]

#need to do the same re-ordering for the model frame
model_temp <- model_frame[ind$ix,2:4]
model_zero <- model_temp[1:num_zeros,]
model_num <- model_temp[(num_zeros+1) :nrow(model_frame),]

#need to do the same re-ordering for the age frame
age_temp <- regd1$age[ind$ix]
age_zero <- age_temp[1:num_zeros]
age_num <- age_temp[(num_zeros+1) :nrow(model_frame)]



educ_data_simple_age2 <- list(
  NZ=num_zeros,
  NN = nrow(model_frame)-num_zeros,
  K = ncol(model_temp),
  y_num = y_num,
  y_zero = y_zero,
  x_num = model_num,
  x_zero= model_zero,
  age_zero = age_zero,
  age_num =age_num
)



fit2 <- stan(
  file = "lib/SimpleGaussianHurdleSpeedAge.stan",  # Stan program
  data = educ_data_simple_age2,    # named list of data
  chains = 2,             # number of Markov chains
  warmup = 250,          # number of warmup iterations per chain
  iter = 1250,            # total number of iterations per chain
  cores = 2,              # number of cores (using 1 just for the vignette)
  refresh = 100          # show progress every 'refresh' iterations
)

# save(fit2,"cache/SimpleGuassianHurdleSpeedAge")
# 
# print(fit2,pars=c("beta","beta_b","beta_age","beta_b_age","sigma"))
# 
# plot(fit2,pars=c("beta","alpha"))
# plot(fit2,pars=c("beta_b","alpha_b"))
# 
# traceplot(fit2,pars=c("alpha","beta"))
# 
# pairs(fit2,pars=c("alpha","beta","sigma"))
# 
# fit2_log_lik <- extract_log_lik(fit2)
# fit2_loo <- loo(fit2_log_lik)
# fit2_pareto <- fit2_loo$pareto_k
# 
# qplot(1:3311,fit2_pareto)
# print(fit2_loo)
# 
# y_gen<-extract(fit2,pars="y_gen")
# 
# y_gen<-y_gen$y_gen
# 
# qplot(y_gen[,1000],binwidth=.1)+coord_cartesian(xlim=c(0,1.5))
# qplot(regd1$educ/10,binwidth=.1)+coord_cartesian(xlim=c(0,1.5))
# 
# 
# qplot(age,educ,data=regd1,geom=c("smooth","point"),group=female,method="loess")
# 


#re-paramaterize to speed up calculations
ind <- sort.int(regd1$educ,index.return = T)
y_sim <- ind$x
num_zeros <- sum(y_sim==0)
#this is silly, I just like being explicit
y_zero <- y_sim[1:num_zeros]
y_num <- y_sim[ (num_zeros+1) :nrow(model_frame)]

#need to do the same re-ordering for the model frame
model_temp <- model_frame[ind$ix,2:4]
model_zero <- model_temp[1:num_zeros,]
model_num <- model_temp[(num_zeros+1) :nrow(model_frame),]

#need to do the same re-ordering for the age frame
age_temp <- regd1$age[ind$ix]
age_zero <- age_temp[1:num_zeros]
age_num <- age_temp[(num_zeros+1) :nrow(model_frame)]

#the District Level
block_dummys <-model.matrix(~regd1$block-1)
block_dummys <- block_dummys[ind$ix,2:25]
block_dummys_zero <- block_dummys[1:num_zeros,]
block_dummys_num <- block_dummys[(num_zeros+1) :nrow(model_frame),]


#Simple problem
educ_data_simple_age_district <- list(
  NZ=num_zeros,
  NN = nrow(model_frame)-num_zeros,
  K = ncol(model_temp),
  y_num = y_num,
  y_zero = y_zero,
  x_num = model_num,
  x_zero= model_zero,
  age_zero = age_zero,
  age_num =age_num,
  J = ncol(block_dummys)-1,
  d_num = block_dummy_num,
  d_zero = block_dummys_zero
)

fit3 <- stan(
  file = "lib/SimpleGaussianHurdleSpeedNoPool.stan",  # Stan program
  data = educ_data_simple_age_district,    # named list of data
  chains = 2,             # number of Markov chains
  warmup = 250,          # number of warmup iterations per chain
  iter = 1250,            # total number of iterations per chain
  cores = 2,              # number of cores (using 1 just for the vignette)
  refresh = 100          # show progress every 'refresh' iterations
)


print(fit3 ,pars=c("beta","gamma","beta_b","sigma","alpha"))
traceplot(fit3 ,pars=c("beta","gamma","beta_b","sigma"))

fit3_log_lik <- extract_log_lik(fit3)
fit3_loo<- loo(fit3_log_lik)
fit3_pareto <- fit3_loo$pareto_k

compare(fit2_loo,fit3_loo)
qplot(1:3311,fit3_pareto)





model_frame <- dummyvars
#re-paramaterize to speed up calculations
ind <- sort.int(regd1$educ,index.return = T)
y_sim <- ind$x
num_zeros <- sum(y_sim==0)
#this is silly, I just like being explicit
y_zero <- y_sim[1:num_zeros]
y_num <- y_sim[ (num_zeros+1) :nrow(model_frame)]

#need to do the same re-ordering for the model frame
model_temp <- model_frame[ind$ix,2:6]
model_zero <- model_temp[1:num_zeros,]
model_num <- model_temp[(num_zeros+1) :nrow(model_frame),]

#need to do the same re-ordering for the age frame
age_temp <- regd1$age[ind$ix]
age_zero <- age_temp[1:num_zeros]
age_num <- age_temp[(num_zeros+1) :nrow(model_frame)]

#the District Level
district_dummys <-model.matrix(~regd1$block-1)
district_dummys <- district_dummys[ind$ix,]
district_dummys_zero <- district_dummys[1:num_zeros,]
district_dummys_num <- district_dummys[(num_zeros+1) :nrow(model_frame),]



# this gig

educ_data_simple_age_district_h <- list(
  NZ=num_zeros,
  NN = nrow(model_frame)-num_zeros,
  K = 5,
  y_num = y_num,
  y_zero = y_zero*5,
  x_num = model_num,
  x_zero= model_zero,
  age_zero = age_zero,
  age_num =age_num,
  J = 19,
  d_num = district_dummys_num,
  d_zero = district_dummys_zero,
  sum_vec = rep(1,5)
)


fit4 <- stan(
  file = "lib/SimpleGaussianHurdleSpeedHierarchical.stan",  # Stan program
  data = educ_data_simple_age_district_h,    # named list of data
  chains = 2,             # number of Markov chains
  warmup = 250,          # number of warmup iterations per chain
  iter = 1250,            # total number of iterations per chain
  cores = 2,              # number of cores (using 1 just for the vignette)
  refresh = 100          # show progress every 'refresh' iterations
)


pairs(fit4 ,pars=c("Beta_Mu","Beta_Sigma"))

print(fit4 ,pars=c("sigma","Beta_Mu","Beta_Sigma","beta"))
plot(fit4 ,pars=c("Beta_Mu"))
plot(fit3 ,pars=c("beta"))

traceplot(fit4 ,pars=c("beta"))

fit4_log_lik <- extract_log_lik(fit4)
fit4_loo<- loo(fit4_log_lik)
fit4_pareto <- fit4_loo$pareto_k

compare(fit3_loo,fit4_loo)
# as this shows, we had an issue with the fit, at the lower and higher values of Y, 
# suggesting our noise model might be insufficient.. 
# though this is 
qplot(jitter(c(y_zero,y_num)),fit4_pareto)


fit4.5 <- stan(
  file = "lib/SimpleGaussianHurdleSpeedNoPoolRVar.stan",  # Stan program
  data = educ_data_simple_age_district_h,    # named list of data
  chains = 2,             # number of Markov chains
  warmup = 250,          # number of warmup iterations per chain
  iter = 1250,            # total number of iterations per chain
  cores = 2,              # number of cores (using 1 just for the vignette)
  refresh = 100          # show progress every 'refresh' iterations
)

print(fit4.5,pars=c("beta"))


beta <- extract(fit4.5,pars="beta")
beta  <- (beta["beta"]
#beta t dim1 = samples , dim2 = blocks, dim3 = betas 
marginal_eff_women_nopool <- beta[,,5] %>% data.frame


compvar<- regd1 %>%
  group_by(block,female,res_num)%>%
  filter(res_num!=1)%>%
  summarize(mean_educ = mean(educ,na.rm=T),
            se_educ = sd(educ,na.rm=T)/sqrt(n()),
            n = n())


table(compvar$block,compvar$female,compvar$res_num)

compvar <-compvar %>%
  group_by(block,female)%>%
  summarize(mean_educ_dif= mean(mean_educ[res_num==2],na.rm=T)-mean(mean_educ[res_num==0],na.rm=T))

compvar <-compvar %>%
  group_by(block)%>%
  summarize(mean_educ_dif= mean(mean_educ_dif[female==1],na.rm=T)-mean(mean_educ_dif[female==0],na.rm=T))


names(marginal_eff_women_nopool) <- compvar$block

sds <- apply(marginal_eff_women_nopool,2,sd) 

data<-rbind(colMeans(marginal_eff_women_nopool),sds)  %>%t %>%data.frame
data$block<-row.names(data)

qplot(block,V1,data=data)+
  geom_errorbar(ymin=colMeans(marginal_eff_women_nopool)-sds,ymax=colMeans(marginal_eff_women_nopool)+sds)+
  coord_cartesian(ylim=c(-1,2))





data <- left_join(data,compvar,by="block")
is.nan(data$mean_educ_dif)

qplot(block,V1,data=data,color=is.nan(data$mean_educ_dif))+geom_errorbar(aes(ymin=V1-sds,ymax=V1+sds))+
  coord_cartesian(ylim=c(-1,2))

qplot(mean_educ_dif,V1,data=data, geom=c("point","smooth"))+geom_abline()+coord_cartesian(ylim=c(-2,2))+
  geom_errorbar(aes(ymin=V1-sds,ymax=V1+sds),data=data[!is.na(data$mean_educ_dif),])





fit5 <- stan(
  file = "lib/SimpleGaussianHurdleSpeedHierarchicalRVar.stan",  # Stan program
  data = educ_data_simple_age_district_h,    # named list of data
  chains = 2,             # number of Markov chains
  warmup = 250,          # number of warmup iterations per chain
  iter = 1250,            # total number of iterations per chain
  cores = 2,              # number of cores (using 1 just for the vignette)
  refresh = 100          # show progress every 'refresh' iterations
)


print(fit5,pars=c("Beta_Mu","Beta_Sigma","sigma_Mu","sigma_Sigma"))
print(fit5,pars=c("beta"))

traceplot(fit5,pars=c("Beta_Mu","Beta_Sigma","sigma_Sigma"))

pairs(fit5,pars=c("sigma"))
library(loo)

fit5_log_lik <- extract_log_lik(fit5)
fit5_loo<- loo(fit5_log_lik)
fit5_pareto <- fit5_loo$pareto_k


pred <- extract(fit5,pars="y_pred")

qplot(pred$y_pred[,800])
qplot(jitter(c(y_zero,y_num)),fit5_pareto)

beta_t <- extract(fit5,pars="beta_t")
beta_t <- beta_t$beta_t
#beta t dim1 = samples , dim2 = blocks, dim3 = betas 
marginal_eff_women <- beta_t[,,5] %>% data.frame


test <- unlist(beta_t["beta_t"][1])


test<-0
test <- extract_betas(fit4.5,5,"beta",compvar$block)




data <-extract_betas(fit4.5,5,"beta",compvar$block)


table(compvar$block,compvar$female,compvar$res_num)


test <- extract_betas(fit4.5,5,"beta",compvar$block)


qplot(block,V1,data=data)+
  geom_errorbar(ymin=colMeans(marginal_eff_women)-sds,ymax=colMeans(marginal_eff_women)+sds)+
  coord_cartesian(ylim=c(-1,2))


data <- left_join(data,compvar,by="block")
is.nan(data$mean_educ_dif)

qplot(block,V1,data=data,color=is.nan(data$mean_educ_dif))+geom_errorbar(aes(ymin=V1-sds,ymax=V1+sds))+
  coord_cartesian(ylim=c(-1,2))

qplot(mean_educ_dif,V1,data=data, geom=c("point","smooth"))+geom_abline()+coord_cartesian(ylim=c(-2,2))+
  geom_errorbar(aes(ymin=V1-sds,ymax=V1+sds),data=data[!is.na(data$mean_educ_dif),])

data[!is.na(data$mean_educ_dif),]
