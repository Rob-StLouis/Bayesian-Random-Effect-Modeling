# library(clusterSEs)
# library(xtable)
# library(rstan)
# library(rstanarm)
# library(dplyr)
# library(tidyr)
# library(tidyr)
# library(readr)
# library(foreign)
# library(magrittr)
# library(ggplot2)


teenc <- read.dta("data/teenager_clean.dta",convert.factors = F)
prac2 <- read.dta("data/pra_clean.dta",convert.factors = F)
cen1 <- read.dta("data/census_birbhum07_data_20080320.dta",convert.factors = F)
ad <- read.dta("data/adult_clean_R123.dta",convert.factors = F)

load("data/household_a1.RData")
hh <-data.frame(x)

# load("data/household.RData")
# hh_c <- data.frame(x)
# teenc$hhsize.x
# colnames(hha)
# 
# table(hh$A1_2,useNA="ifany")
#adding hh level data to this

#adding some other basic stuff here 

teenc <- inner_join(teenc,hh %>% dplyr::select(serialid,membercode,A1_8,A1_7,A1_5,A1_9),by=c("serialid","membercode") )




#adding some hh composition stuff. 
hhcheck <-hh %>%
  group_by(serialid)%>%
  summarise(n_women = sum(A1_2==2),
            pct_women= mean(A1_2==2),
            n_ad = sum(A1_4_year>17),
            n_w_ad = sum(A1_2[A1_4_year>17]==2),
            n_m_ad = sum(A1_2[A1_4_year>17]==1),
            ad_pct_lit = mean(A1_5[A1_4_year>17] == 3),
            n_child = sum(A1_4_year<18),
            n_adol = sum(A1_4_year<16 &A1_4_year>10),
            n_w_child = sum(A1_2[A1_4_year<17]==2),
            ad_w_pct_lit= mean(A1_5[A1_4_year>18&A1_2==2] == 3),
            ad_m_pct_lit= mean(A1_5[A1_4_year>18&A1_2==1] == 3)
            )




teenc <- inner_join(teenc,hhcheck,by=c("serialid") )
ad <- inner_join(ad,hhcheck,by=c("serialid") )

################################
#census corrections, from table4.do file
# 
# replace AA0_2b = 114 if temp_id==114;
# replace AA0_2b = 158 if temp_id==158;
# keep AA0_3b AA0_3a AA0_2b t_popln scst_share hhsize sexratio_under6  pcnt_literat fem_literacy pcnt_irrig bus_train app_pucca_road tubewell hand_pump haswell tap n_school n_health;
# foreach i in pcnt_irrig bus_train app_pucca_road  tubewell hand_pump haswell tap n_school n_health {;
#   ge `i'_missing=1 if `i'==.;
#   replace `i'_missing=0 if `i'~=.;
#   replace `i'=0 if `i'==.;
# };
#   
#   replace AA0_3b=16 if AA0_3b==161 & AA0_2b==8;
#   replace AA0_3b=54 if AA0_3b==56 & AA0_2b==64;
#   replace AA0_3b=28 if AA0_3b==82 & AA0_2b==70;
#   replace AA0_3b=36 if AA0_3b==37 & AA0_2b==80;
#   replace AA0_3b=151 if AA0_3b==150 & AA0_2b==95;
#   replace AA0_3b=69 if AA0_3b==70 & AA0_2b==98;
#   replace AA0_3b=47 if AA0_3b==49 & AA0_2b==102;
#   replace AA0_3b=155 if AA0_3b==37 & AA0_2b==159;

#check the temp replacement makes sense
# cen1$temp_id[cen1$temp_id != cen1$AA0_2b]
#its identical except for those, so replace it wholesale 

cen1$AA0_2b <- cen1$temp_id

cen1$AA0_3b <- with(cen1, ifelse( AA0_3b==161 & AA0_2b==8, 16,AA0_3b ))
cen1$AA0_3b <- with(cen1, ifelse( AA0_3b==56 & AA0_2b==64, 54,AA0_3b ))
cen1$AA0_3b <- with(cen1, ifelse( AA0_3b==82 & AA0_2b==70, 28,AA0_3b ))
cen1$AA0_3b <- with(cen1, ifelse( AA0_3b==37 & AA0_2b==80, 36,AA0_3b ))
cen1$AA0_3b <- with(cen1, ifelse( AA0_3b==150 & AA0_2b==95, 151,AA0_3b ))
cen1$AA0_3b <- with(cen1, ifelse( AA0_3b==70 & AA0_2b==98, 69,AA0_3b ))
cen1$AA0_3b <- with(cen1, ifelse( AA0_3b==49 & AA0_2b==102, 47,AA0_3b ))
cen1$AA0_3b <- with(cen1, ifelse( AA0_3b==37 & AA0_2b==159, 155,AA0_3b ))


# test1 <-cen1 %>%
#   group_by(AA0_1b,AA0_2b,AA0_3b)%>%
#   summarize(n= n())
#ok, the goal right now is the get these four files to have the same numeric data. 

#first, the easy one, we need to get what looks like coding errors in the pra_clean data set. 
#I'm treating the census data as  correct, becuse it has unique combinations of AA0_1b,AA0_2b,AA0_3b, might be iffy, seems like smart choice here
prac2$AA0_3b <- ifelse(prac2$AA0_2b==150 & prac2$AA0_3b==137 & prac2$AA0_3a=="Dariyapur",136,prac2$AA0_3b )
prac2$AA0_3b <- ifelse(prac2$AA0_2b==159 & prac2$AA0_3b==37 & prac2$AA0_3a=="Namakanda",34,prac2$AA0_3b )
prac2$AA0_3b <- ifelse(prac2$AA0_2b==159 & prac2$AA0_3b==37 & prac2$AA0_3a=="Kamra",50,prac2$AA0_3b )

#and then same as above, seemed to have the same issues. 

prac2$AA0_3b <- with(prac2, ifelse( AA0_3b==161 & AA0_2b==8, 16,AA0_3b ))
prac2$AA0_3b <- with(prac2, ifelse( AA0_3b==56 & AA0_2b==64, 54,AA0_3b ))
prac2$AA0_3b <- with(prac2, ifelse( AA0_3b==82 & AA0_2b==70, 28,AA0_3b ))
prac2$AA0_3b <- with(prac2, ifelse( AA0_3b==37 & AA0_2b==80, 36,AA0_3b ))
prac2$AA0_3b <- with(prac2, ifelse( AA0_3b==150 & AA0_2b==95, 151,AA0_3b ))
prac2$AA0_3b <- with(prac2, ifelse( AA0_3b==70 & AA0_2b==98, 69,AA0_3b ))
prac2$AA0_3b <- with(prac2, ifelse( AA0_3b==49 & AA0_2b==102, 47,AA0_3b ))
prac2$AA0_3b <- with(prac2, ifelse( AA0_3b==37 & AA0_2b==159, 155,AA0_3b ))

#this failed
# test_join <-inner_join(cen1,prac2, by=c("AA0_1b","AA0_2b","AA0_3b"))
# #observing missing values
# test_join1 <-anti_join(cen1,prac2, by=c("AA0_1b","AA0_2b","AA0_3b")) %>%
#   select(AA0_1b,AA0_2b,AA0_3b,AA0_1a,AA0_2a,AA0_3a)
#  test_join2 <-anti_join(prac2,cen1, by=c("AA0_1b","AA0_2b","AA0_3b"))%>%
#   select(AA0_1b,AA0_2b,AA0_3b,AA0_1a,AA0_2a,AA0_3a)
# 
# #pretty clear that they are the same values, so swtiching AA_2b values for prac2, to the following: 

prac2$AA0_2b <- with(prac2, ifelse( AA0_3b==190 & AA0_2b==113, 114,AA0_2b ))
prac2$AA0_3b <- with(prac2, ifelse( AA0_3b==101 & AA0_2b==139, 10,AA0_3b ))
prac2$AA0_2b <- with(prac2, ifelse( AA0_3b==4 & AA0_2b==156, 158,AA0_2b ))

#try again
# test_join <-inner_join(cen1 %>% 
#                          select(AA0_1b,AA0_2b,AA0_3b,AA0_1a,AA0_2a,AA0_3a),
#                        prac2 %>% select(AA0_1b,AA0_2b,AA0_3b,AA0_1a,AA0_2a,AA0_3a),
#                        by=c("AA0_1b","AA0_2b","AA0_3b"))

#should all equal 495
# sum(test_join$AA0_1a.x==test_join$AA0_1a.y)
# sum(test_join$AA0_2a.x==test_join$AA0_2a.y)
# sum(test_join$AA0_3a.x==test_join$AA0_3a.y)
#   




####
#now the teen data, still planning to use the
# 
# #this is missing 50 values,
# d_set <- inner_join(teenc,test_join,by=c("AA0_1b","AA0_2b","AA0_3b"))
# t1 <- teenc %>%
#   group_by(AA0_1b,AA0_2b,AA0_3b)%>%
#   summarize(n =n())
# 
# test_join3 <-anti_join(cen1,t1,by=c("AA0_1b","AA0_2b","AA0_3b"))%>%
#   select(AA0_1b,AA0_2b,AA0_3b,AA0_1a,AA0_2a,AA0_3a)
# 
# test_join4 <-anti_join(t1,cen1, by=c("AA0_1b","AA0_2b","AA0_3b"))
# 
# teenc%>%
#   select(AA0_1b,AA0_2b,AA0_3b,AA0_1a,AA0_2a,AA0_3a)%>%
#   filter(AA0_2b==61,AA0_1b==9)

# so looks like 81 mistakenly coded as 61
teenc$AA0_2b <- with(teenc, ifelse( AA0_1b==9 & AA0_2b==61, 81,AA0_2b ))

# looks like 84 is coded as 87'
#this is in the adult data set
# teenc%>%
#   select(AA0_1b,AA0_2b,AA0_3b,AA0_1a,AA0_2a,AA0_3a)%>%
#   filter(AA0_2a=="Koma")

teenc$AA0_3b <- with(teenc, ifelse( AA0_1b==6 &AA0_2b==52 & AA0_3a=="Januri", 84,AA0_3b ))


#ok it seems like  this village isn't in the teen database, and I can't find it, which is too bad, but not the end of the world
#AA0_1b AA0_2b AA0_3b   AA0_1a   AA0_2a AA0_3a
#1     11    101     42 Rajnagar Rajnagar  Bandi
# teenc%>%
#   select(AA0_1b,AA0_2b,AA0_3b,AA0_1a,AA0_2a,AA0_3a)%>%
#   filter(AA0_2b==101)
# 
# c1<-cen1 %>%
#   select(AA0_1b,AA0_2b,AA0_3b,AA0_1a,AA0_2a,AA0_3a)

#now the teen data that doesn't show up in the other data
# 
# teenc%>%
#   select(AA0_1b,AA0_2b,AA0_3b,AA0_1a,AA0_2a,AA0_3a)%>%
#   filter(AA0_2b==103,AA0_3b==9)

teenc$AA0_2b <- with(teenc, ifelse( AA0_1b==14 &AA0_2b==103 & AA0_3b==9, 120,AA0_2b ))


# teenc%>%
#   select(AA0_1b,AA0_2b,AA0_3b,AA0_1a,AA0_2a,AA0_3a)%>%
#   filter(AA0_2b==13,AA0_3b==73)

teenc$AA0_3b <- with(teenc, ifelse( AA0_1b==2 &AA0_2b==13 & AA0_3b==73, 82,AA0_3b ))

# teenc%>%
#   select(AA0_1b,AA0_2b,AA0_3b,AA0_1a,AA0_2a,AA0_3a)%>%
#   filter(AA0_2b==134,AA0_3b==140)

teenc$AA0_2b <- with(teenc, ifelse( AA0_1b==15 &AA0_2b==134 & AA0_3b==140, 135,AA0_2b ))

# 
# teenc%>%
#   select(AA0_1b,AA0_2b,AA0_3b,AA0_1a,AA0_2a,AA0_3a)%>%
#   filter(AA0_2b==114,AA0_3b==117)

teenc$AA0_3b <- with(teenc, ifelse( AA0_1b==13 &AA0_2b==114 & AA0_3b==117, 190,AA0_3b ))

#ok, this looks reasonable. 


# d_set <- inner_join(teenc %>% select(AA0_1b,AA0_2b,AA0_3b,AA0_1a,AA0_2a,AA0_3a),test_join,by=c("AA0_1b","AA0_2b","AA0_3b"))

#this is the filtering set, which I think we should use.
# d1 <- d_set %>%
# filter(d_set$AA0_1a!=d_set$AA0_1a.x| d_set$AA0_2a!=d_set$AA0_2a.x|d_set$AA0_3a!=d_set$AA0_3a.x )%>%
#   select
# d1 <- d_set %>%
# filter(AA0_1a!=AA0_1a.x)%>%
#   select(AA0_1a,AA0_1a.x)
# 
# d2 <- d_set %>%
#   filter(AA0_2a!=AA0_2a.x)%>%
#   select(AA0_2a,AA0_2a.x)
# 
# d3 <- d_set %>%
#   filter(AA0_3a!=AA0_3a.x)%>%
#   select(AA0_3a,AA0_3a.x)
# 
# 
# d_set%>% 
#   filter (AA0_2a.y=="Joydeb-Kenduli")

#so it seems there are massive misspelling, but they all seem to be phonetically related. Exception is one Joydeb-Kenduli, 
#which doesn't appear in the census data,  rather they seem to repeat district name, may be a type, so I'll ignore it
#finally, the adult cleaning data.


#### initial test with adults
# 13508-13444
#64 seem to be mis-classified
# a_set <- inner_join(ad %>% select(AA0_1b,AA0_2b,AA0_3b,AA0_1a,AA0_2a,AA0_3a),test_join,by=c("AA0_1b","AA0_2b","AA0_3b"))



#from table4 do
ad$AA0_3b <- with(ad, ifelse( AA0_3b==286,186,AA0_3b));
ad$AA0_3b <- with(ad, ifelse( AA0_3b==129 & AA0_2b==11,119,AA0_3b));

ad$AA0_3b <-with(ad , ifelse( AA0_3b==13 & AA0_2b==23,12,AA0_3b));

ad$AA0_3b <-with(ad , ifelse( AA0_3b==71 & AA0_2b==71,19,AA0_3b));
ad$AA0_3b <-with(ad , ifelse( AA0_3b==10 & AA0_2b==74,110,AA0_3b));
ad$AA0_3b <-with(ad , ifelse( AA0_3b==45 & AA0_2b==74,145, AA0_3b));
ad$AA0_3b <-with(ad , ifelse( AA0_3b==91 & AA0_2b==96,97, AA0_3b));
ad$AA0_3b <-with(ad , ifelse( AA0_3b==107 & AA0_2b==117,160,AA0_3b));
ad$AA0_3b <-with(ad , ifelse( AA0_3b==138 & AA0_2b==138,23,AA0_3b));
ad$AA0_3b <-with(ad , ifelse( AA0_3b==87 & AA0_3a=="Januri",84,AA0_3b));
#Looks great
# a_set <- inner_join(ad %>% select(AA0_1b,AA0_2b,AA0_3b,AA0_1a,AA0_2a,AA0_3a),test_join,by=c("AA0_1b","AA0_2b","AA0_3b"))

cen1$res_num <- as.factor(cen1$res_female+cen1$res_femprev)
teenc$res_num <- as.factor(teenc$res_female+teenc$res_femprev)
ad$res_num <- as.factor(ad$res_female+ad$res_femprev)

# a1 <- ad %>%
#   group_by(AA0_1b,AA0_2b,AA0_3b)%>%
#   summarize(n =n())
# 
# test_join5 <-anti_join(cen1,a1,by=c("AA0_1b","AA0_2b","AA0_3b"))%>%
#   select(AA0_1b,AA0_2b,AA0_3b,AA0_1a,AA0_2a,AA0_3a)
# 
# test_join6 <-anti_join(a1,cen1, by=c("AA0_1b","AA0_2b","AA0_3b"))
# 

#creating the aspirations dataset 

#the straightforward addition of the variables.

teenc$inlaws_house = 1-teenc$inlaws_house
teenc$marry_young = 1-teenc$youngmarriage

teenc$avAsp2 <- (teenc$higheducjob+teenc$inlaws_house +teenc$marry_young+teenc$wishhigheduc)/4

x<- teenc %>%
  filter(res_num==0)%>%
  summarize(higheducjob1= mean(higheducjob,na.rm=T),
            hejSD =sd(higheducjob),
            inlaws_house1 = mean(inlaws_house,na.rm=T),
            ihSD =sd(inlaws_house),
            youngmarriage1 = mean(marry_young,na.rm=T),
            ymSD =sd(marry_young,na.rm=T),
            wishhigheduc1 = mean(wishhigheduc,na.rm=T),
            wheSD =sd(wishhigheduc,na.rm=T),
            wishpradhan1 = mean(wishpradhan,na.rm=T),
            wpSD =sd(wishpradhan,na.rm=T),
            avAsp21 = mean(avAsp2, na.rm=T),
            aaSD = sd(avAsp2, na.rm=T))

teenc$higheducjobN <- (teenc$higheducjob-x$higheducjob1)/x$hejSD
teenc$inlaws_houseN <- (teenc$inlaws_house-x$inlaws_house1)/x$ihSD
teenc$youngmarriageN <- (teenc$marry_young-x$youngmarriage1)/x$ymSD
teenc$wishhigheducN  <- (teenc$wishhigheduc-x$wishhigheduc1)/x$wheSD
teenc$wishpradhanN <- (teenc$wishpradhan-x$wishpradhan1)/x$wpSD
teenc$avAsp2N <- (teenc$avAsp2-x$avAsp21)/x$aaSD

# table(teenc$higheducjobN)
# table(teenc$inlaws_houseN)

teenc$avAsp <- rowMeans(teenc %>% dplyr::select(higheducjobN:wishhigheducN),na.rm =T)

cres <-cen1%>%
  dplyr::select(AA0_1b,AA0_2b,AA0_3b,t_popln,tap,n_health,n_school,haswell,hand_pump,tubewell,app_pucca_road,bus_train,pcnt_irrig,fem_literacy ,
                pcnt_literat,sexratio_under6,hhsize,scst_share,households,vill_code,res_sc,res_st,prev_res_sc,prev_res_st)


teenc %<>% inner_join(cres,by=c("AA0_1b","AA0_2b","AA0_3b"))

ad %<>% inner_join(cres,by=c("AA0_1b","AA0_2b","AA0_3b"))


int1 <- teenc %>%
  dplyr::select(serialid_teenager)

int1$ed_time <-           ifelse(teenc$C1_1_unit=="Minutes",teenc$C1_1_time,teenc$C1_1_time*60)
int1$water_time <-        ifelse(teenc$C1_2_unit=="Minutes",teenc$C1_2_time,teenc$C1_2_time*60)
int1$cook_time <-         ifelse(teenc$C1_3_unit=="Minutes",teenc$C1_3_time,teenc$C1_3_time*60)
int1$ag_home_time <-      ifelse(teenc$C1_4_unit=="Minutes",teenc$C1_4_time,teenc$C1_4_time*60)
int1$ag_no_home_time <-   ifelse(teenc$C1_5_unit=="Minutes",teenc$C1_5_time,teenc$C1_5_time*60)
int1$animals_home_time <- ifelse(teenc$C1_6_unit=="Minutes",teenc$C1_6_time,teenc$C1_6_time*60)
int1$cleaning_time <-     ifelse(teenc$C1_7_unit=="Minutes",teenc$C1_7_time,teenc$C1_7_time*60)
int1$clothes_time <-      ifelse(teenc$C1_8_unit=="Minutes",teenc$C1_8_time,teenc$C1_8_time*60)
int1$care_other_time <-   ifelse(teenc$C1_9_unit=="Minutes",teenc$C1_9_time,teenc$C1_9_time*60)
int1$firewood_time <-     ifelse(teenc$C1_10_unit=="Minutes",teenc$C1_10_time,teenc$C1_10_time*60)
int1$dom_other_time <-    ifelse(teenc$C1_11_unit=="Minutes",teenc$C1_11_time,teenc$C1_11_time*60)
int1$work_pay_time <-    ifelse(teenc$C1_12_unit=="Minutes",teenc$C1_12_time,teenc$C1_12_time*60)
int1$shop_time <-    ifelse(teenc$C1_13_unit=="Minutes",teenc$C1_13_time,teenc$C1_13_time*60)
int1$tv_time <-  ifelse(teenc$C1_14_unit=="Minutes",teenc$C1_14_time,teenc$C1_14_time*60)



#ok no time from these, hmm.
# table(teenc$C1_15_work1,teenc$C1_15_work2)
# table(teenc$C1_15_work3)
# table(teenc$C1_15_work4)
# table(teenc$C1_15_work5)

#earnings seems to be tiny role here

#this seems to be tiny, can ingo
table(teenc$C1_16)
teenc$earnmoney <- teenc$C1_16 == "Yes"

# table(teenc$C1_16,teenc$C1_17)
# table(teenc$C1_18_1)


#replacing Na with 0
int1[is.na(int1)] <- 0


#see if they are giving 0 to kids who report no chores, appears they do. 
#we can look at other chores by restricting kids who are earning money, but this is less of an issue.


teenc %<>% inner_join(int1,by=c("serialid_teenager"))

table(teenc$earnmoney,teenc$dom_other_time)
table(teenc$earnmoney,teenc$ag_no_home_time)
table(teenc$earnmoney,teenc$ag_home_time)


teenc%<>%
  mutate(chore_time = water_time+cook_time+cleaning_time+clothes_time+care_other_time+firewood_time,
         ag_time = ag_home_time+ag_no_home_time+animals_home_time,
         chore_ag_time = chore_time+ag_time)


####educational outcomes

teenc$inschool <- 2-teenc$A1_8
teenc$readwrite <- teenc$A1_5==3

#add if literate adult
teenc$litad <- ifelse(teenc$ad_pct_lit>0,1,0)
teenc$litad <-ifelse(is.na(teenc$ad_pct_lit),0,teenc$litad)


#other types of reservation statuses

teenc$res_scb <- ifelse( teenc$res_sc=="Yes" &teenc$prev_res_sc =="Yes","2","1")
teenc$res_scb <- ifelse( teenc$res_sc!="Yes" &teenc$prev_res_sc !="Yes","0",teenc$res_scb)

teenc$res_stb <- ifelse( teenc$res_st=="Yes" &teenc$prev_res_st =="Yes","2","1")
teenc$res_stb <- ifelse( teenc$res_st!="Yes" &teenc$prev_res_st !="Yes","0",teenc$res_stb)


#tying desperately to figure out those numbers, not quite working out. 

ad$adol_child15 <- with(ad, ifelse(  child15_age<16 & child15_age>10 & child15_alive=="Yes",1,0))
ad$adol_child14 <- with(ad, ifelse(  child14_age<16 & child14_age>10 & child14_alive=="Yes",1,0))
ad$adol_child13 <-with(ad,  ifelse(  child13_age<16 & child13_age>10 & child13_alive=="Yes" ,1,0))
ad$adol_child12 <-with(ad,  ifelse(  child12_age<16 & child12_age>10 & child12_alive=="Yes", 1,0))
ad$adol_child11 <-with(ad,  ifelse(  child11_age<16 & child11_age>10 & child11_alive=="Yes", 1,0))
ad$adol_child10 <-with(ad,  ifelse(  child10_age<16 & child10_age>10 & child10_alive=="Yes", 1,0))
ad$adol_child9 <-with(ad,  ifelse(  child9_age<16 & child9_age>10 & child9_alive=="Yes", 1,0))
ad$adol_child8 <-with(ad,  ifelse(  child8_age<16 & child8_age>10 & child8_alive=="Yes", 1,0))
ad$adol_child7 <-with(ad,  ifelse(  child7_age<16 & child7_age>10 & child7_alive=="Yes", 1,0))
ad$adol_child6 <-with(ad,  ifelse(  child6_age<16 & child6_age>10 & child6_alive=="Yes", 1,0))
ad$adol_child5 <-with(ad,  ifelse(  child5_age<16 & child5_age>10 & child5_alive=="Yes", 1,0))
ad$adol_child4 <-with(ad,  ifelse(  child4_age<16 & child4_age>10 & child4_alive=="Yes", 1,0))
ad$adol_child3 <-with(ad,  ifelse(  child3_age<16 & child3_age>10 & child3_alive=="Yes", 1,0))
ad$adol_child2 <-with(ad,  ifelse(  child2_age<16 & child2_age>10 & child2_alive=="Yes", 1,0))
ad$adol_child1 <-with(ad, ifelse(child1_age<16 & child1_age>10 & child1_alive=="Yes" ,1,0))
ad$adol_child <- rowSums(ad%>%dplyr::select(adol_child15:adol_child1),na.rm=T)
#not sure how to linke up the adult and children data. 

#remove variables we don't need anymore
rm(x,cen1,hh,hhcheck,prac2)




