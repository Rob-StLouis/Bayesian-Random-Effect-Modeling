# simple function to do simulations to test how well the model recovers data.
 SimpleGaussianHurdleSim <- function(N_Sims,model_frame){
    
  N_Subs <- nrow(model_frame)
  
  sim_pars <-data.frame(stim_n = 1:N_Sims)  
  
  #Linear Regresion Model
  
  #want to look at the relative larger values.
  sim_pars$alpha <- runif(N_Sims,.5,1)
  for(i in 1:5){
    sim_pars[[paste('beta',i, sep="_")]] <- rnorm(N_Sims,0,.1)
  } 
  #made the sigma .5
  sim_pars$sigma <- abs(rnorm(N_Sims,0,.3))
  
  #Hurdle Model
  sim_pars$alpha_b <- rnorm(N_Sims,1,1)
  for(i in 1:5){
    sim_pars[[paste('beta_b',i, sep="_")]] <- rnorm(N_Sims,0,1)
  }
  #for testing only
  # N_Subs=3311
  
  theta <- cbind(rep(1,N_Subs),model_frame[,2:6]) %*% t(sim_pars %>% select(alpha_b,beta_b_1:beta_b_5) %>% as.matrix()) %>%data.frame
  bern_prob <- theta 
  #logistic Transform
  for(i in 1:N_Sims){
    theta[,i] <- exp(theta[,i]) / (1+exp(theta[,i]))
   bern_prob[,i] <- rbinom(N_Subs,1,theta[,i])
  }
  
  #Linear Regression
  
  mu <- cbind(rep(1,N_Subs),model_frame[,2:6]) %*% t(sim_pars %>% select(alpha,beta_1:beta_5) %>% as.matrix()) %>%data.frame
  reg_out <- mu
  for(i in 1:N_Sims){
    reg_out[,i] <-rnorm(N_Subs,mu[,i],sim_pars[i,"sigma"])
  }
  
  l_zero <- reg_out > 0
  
  ##gross, but the data's relatively small, 
  # should be replaced with something vectorized. Or just a Julia call. Don't really want to do it in c++
  for(i in 1:N_Sims){
    for(j in 1:N_Subs){
      if(l_zero[j,i]){
        x = -1
        while(x<0){
          x <- rnorm(1,mu[j,i],sim_pars[i,"sigma"])
        }
        reg_out[j,i] <-x
      }
    }
  }
  
  y  <- reg_out * bern_prob
  

  #extract means and 50% and 80% confidence intervals
  iii=1
  result_list = list()
  
  ind <- sort.int(y[,iii],index.return = T)
  y_sim <- ind$x
  num_zeros <- sum(y_sim==0)
  #this is silly, I just like being explicit
  y_zero <- y_sim[1:num_zeros]
  y_num <- y_sim[ (num_zeros+1) :nrow(model_frame)]
  
  #need to do the same re-ordering for the model frame
  model_temp <- model_frame[ind$ix,2:6]
  model_zero <- model_temp[1:num_zeros,]
  model_num <- model_temp[(num_zeros+1) :nrow(model_frame),]
  
  
  for(iii in 1:N_Sims){
    sim_data_simple <- list(
      NZ=num_zeros,
      NN = nrow(model_frame)-num_zeros,
      K = 5,
      #we do this because stan works better with normalized variables, this brings most values into the 0,1 range
      y_num = y_num,
      y_zero = y_zero,
      x_num = model_num,
      x_zero= model_zero
    )
    

    
    simfit <- stan(
      file = "lib/SimpleGaussianHurdleSpeed.stan",  # Stan program
      data = sim_data_simple,    # named list of data
      chains = 2,             # number of Markov chains
      warmup = 250,          # number of warmup iterations per chain
      iter = 1000,            # total number of iterations per chain
      cores = 2,              # number of cores (using 1 just for the vignette)
      refresh = 100          # show progress every 'refresh' iterations
    )
    
    print(simfit, pars=c("alpha","beta","sigma","alpha_b","beta_b"))
    
    posterior_samples <- extract(simfit, pars=c("alpha","beta","sigma","alpha_b","beta_b"))
    
    rm(simfit)
    quant_alpha <- quantile(posterior_samples$alpha, c(.1,.25,.5,.75,.9))
    quant_beta <- apply( posterior_samples$beta, 2, quantile,probs= c(.1,.25,.5,.75,.9))
    quant_sigma <- quantile(posterior_samples$sigma, c(.1,.25,.5,.75,.9))
    quant_alpha_b <- quantile(posterior_samples$alpha_b, c(.1,.25,.5,.75,.9))
    quant_beta_b <- apply( posterior_samples$beta_b, 2, quantile,probs= c(.1,.25,.5,.75,.9))
    
    quant<- cbind(names(sim_pars[,2:14]),data.frame(rbind(quant_alpha,t(quant_beta),quant_sigma,quant_alpha_b,t(quant_beta_b))))
    
    fifty <- c()
    eighty <- c()
    for(i in 1:(ncol(sim_pars)-1)){
     fifty[i] <- quant[i,3] < sim_pars[1,i+1]&sim_pars[1,i+1]  < quant[i,5] 
     eighty[i] <- quant[i,2] < sim_pars[1,i+1]&sim_pars[1,i+1]  < quant[i,6]
    }
    
    simr_tes = list(sim_pars[iii,],quant,fifty,eighty)
    
    result_list[[paste("sim",iii,sep="_")]] = simr_tes
  
  }
  return(result_list)
}





