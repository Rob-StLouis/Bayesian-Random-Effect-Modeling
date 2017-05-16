library('ProjectTemplate')
ProjectTemplate::load.project()

#remove  the missing data- random assignment
regd1 <- teenc %>%
  select(loweduc,res_num,female,n_health,n_school,haswell,hand_pump,tubewell,app_pucca_road,bus_train,pcnt_irrig,fem_literacy ,
         pcnt_literat,sexratio_under6,hhsize.y,scst_share,households,AA0_2a,block,wishmidmineduc,higheducjob, marryover20, 
         wishhigheduc,sexist_ben,sexist_hos,educ,age) %>%
  na.omit

regd1$Female <- ifelse(regd1$female==0,"Male","Female")
g1 <- ggplot(regd1)+aes(x=educ,fill=female)+geom_histogram(binwidth =1)+facet_grid(female~.)
g1


#Ok, should do a zero inflated model, robust t-distribution. 