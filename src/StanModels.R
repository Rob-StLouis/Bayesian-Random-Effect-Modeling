library(ProjectTemplate)

load.project()
regd1 <- teenc %>%
  dplyr::select(loweduc,res_num,female,n_health,n_school,haswell,hand_pump,tubewell,app_pucca_road,bus_train,pcnt_irrig,fem_literacy ,
         pcnt_literat,sexratio_under6,hhsize.y,scst_share,households,AA0_2a,block,wishmidmineduc,higheducjob, marryover20, 
         wishhigheduc,sexist_ben,sexist_hos,educ,age) %>%
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




### Model 1 
### simplest model, not optimized for speed yet, does not include random effects of district
#build model matrix
dummyvars <-model.matrix(~regd1$res_num*regd1$female-1)
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

### Model 2 
### same as above, just re-paramaterized to be faster.

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

save(fit2,file="cache/SimpleGaussianHurdleSpeedAge",compress = "bzip2")
rm(fit1,fit2)

#### Model 3
### Adding in block random effects.
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
block_dummys <- block_dummys[ind$ix,2:19]
block_dummys_zero <- block_dummys[1:num_zeros,]
block_dummys_num <- block_dummys[(num_zeros+1) :nrow(model_frame),]

colnames(block_dummys)

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
  J = ncol(block_dummys),
  d_num = block_dummys_num,
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

save(fit3,file="cache/SimpleGaussianHurdleSpeedNoPool",compress = "bzip2")
rm(fit3)


### Model 4
### allow effects to vary (no pooling) across blocks

model_frame <- dummyvars
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
district_dummys <-model.matrix(~regd1$block-1)
district_dummys <- district_dummys[ind$ix,]
district_dummys_zero <- district_dummys[1:num_zeros,]
district_dummys_num <- district_dummys[(num_zeros+1) :nrow(model_frame),]

### Model 4, add in independent effects for each  effect
### 

# this gig

educ_data_simple_age_district_h <- list(
  NZ=num_zeros,
  NN = nrow(model_temp)-num_zeros,
  K = ncol(model_temp),
  y_num = y_num,
  y_zero = y_zero,
  x_num = model_num,
  x_zero= model_zero,
  age_zero = age_zero,
  age_num =age_num,
  J = 19,
  d_num = district_dummys_num,
  d_zero = district_dummys_zero,
  sum_vec = rep(1,3)
)

fit4 <- stan(
  file = "lib/SimpleGaussianHurdleSpeedNoPoolRVar.stan",  # Stan program
  data = educ_data_simple_age_district_h,    # named list of data
  chains = 2,             # number of Markov chains
  warmup = 250,          # number of warmup iterations per chain
  iter = 1250,            # total number of iterations per chain
  cores = 2,              # number of cores (using 1 just for the vignette)
  refresh = 100          # show progress every 'refresh' iterations
)

save(fit4,file="cache/SimpleGaussianHurdleSpeedNoPoolRVar",compress = "bzip2")
rm(fit4)
### model 5
## add in Heirarchical Effects.

fit5 <- stan(
  file = "lib/SimpleGaussianHurdleSpeedHierarchicalRVar.stan",  # Stan program
  data = educ_data_simple_age_district_h,    # named list of data
  chains = 2,             # number of Markov chains
  warmup = 250,          # number of warmup iterations per chain
  iter = 1250,            # total number of iterations per chain
  cores = 2,              # number of cores (using 1 just for the vignette)
  refresh = 100          # show progress every 'refresh' iterations
)

save(fit5,file = "cache/SimpleGaussianHurdleSpeedHierarchicalRVar",compress="bzip2")
rm(fit5)
# this lets us recreate the important datasets for subsequent comparison. 
save(y_zero,y_num,ind,model_temp,district_dummys_num,district_dummys_zero,file="cache/BAS_model_vars")
