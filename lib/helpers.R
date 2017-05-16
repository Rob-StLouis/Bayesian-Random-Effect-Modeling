extract_betas <- function(fit_list,which_var, var_name, blocknames){
  h <- extract(fit_list,pars=var_name)
  h <- h[[var_name]]
  # return(h)
  g <- h[,,which_var] %>% data.frame
  sds <- apply(g,2,sd) 
  mean <- colMeans(g)
  data<-rbind(mean,sds)  %>%t %>%data.frame
  #  alittle iffy w/ordering. should change to match
  
  data$block<-str_replace(blocknames,"regd1[$]block","")
  return(data)
  
}


